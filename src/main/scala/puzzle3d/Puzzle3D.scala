package puzzle3d

import csp.{CNF, Literal, OneOf, BVar}
import scopt.Read
import java.io.{FileReader, File}

object Puzzle3D {
  implicit val pieceListReader: Read[Piece] = Read.reads(s => PieceParser.parseAll(PieceParser.piece,s).get)

  case class Config(problems: Seq[Piece] = Seq(),
                    prototypes: Seq[Piece] = Piece.prototypes,
                    printSolverStats: Boolean = false)

  val parser = new scopt.OptionParser[Config]("puzzle3d") {
    head("puzzle3d", "1.0")
    opt[Piece]('p', "problem") action { (x, c) =>
      c.copy(problems = c.problems :+ x) } text("add a problem") valueName("<piece-description>")
    opt[File]('f', "file") valueName("<file>") action { (x, c) =>
      c.copy(problems = c.problems ++ PieceParser.parseAll(PieceParser.pieces,new FileReader(x)).get)
    } text("read a list of problems from a file")
    opt[Unit]('v',"verbose") action { (_, c) =>
      c.copy(printSolverStats = true) } text("verbose is a flag")
    note("some notes.\n")
    help("help") text("prints this usage text")
  }


  def main(args: Array[String]) {
    val config = parser.parse(args,Config())
    config.foreach(solveAll)
  }

  def solveAll(config: Config): Unit = {
    config.problems.foreach{ problem =>
      val (sol,stats) = solveInstance(config,problem)
      sol match {
        case Some(s) =>
          println("Solution found:")
          println(prettyPrintSolution(s))
        case None =>
          println("No solution possible.")
      }
      if(config.printSolverStats){
        println("Solver stats:")
        stats.foreach{case (stat,num) => println(f"\t$stat: $num")}
      }
    }
  }

  def solveInstance(config: Config, goal: Piece): (Option[Set[Piece]], Map[String, Number]) = {
    val prototypes: Seq[Piece] = config.prototypes
    val (problemEncoding,varToPlacement) = constructSATProblem(goal, prototypes)
    val (solver, varmap) = CNF.toProblem(problemEncoding)

    val model: Option[Array[Int]] = Some(solver).filter(_.isSatisfiable).map(_.model)
    val solution: Option[Set[Piece]] = model.map(_.filter(_ >= 0).map(varmap).collect(varToPlacement).toSet)
    import collection.JavaConverters._
    (solution,solver.getStat.asScala.toMap)
  }

  def prettyPrintSolution(sol: Set[Piece]): String = sol.zipWithIndex.foldLeft(CharMap(Map())) {
    case (acc, (p, idx)) => acc.addPiece(p, ('A' + idx).toChar)
  }.toString

  /** Construct the SAT encoding of the problem, together with a mapping of boolean variables to the
    * placed pieces.
    * @param goal The blocks to fill with pieces.
    * @param prototypes At most one of each prototype may be placed, rotated and translated.
    * @return Encoding as set of clauses and a mapping from variables to placed pieces.
    */
  def constructSATProblem(goal: Piece, prototypes: Seq[Piece]): (Set[Set[Literal]],Map[BVar,Piece]) = {
    val height = goal.maxZ + 1
    val width = goal.maxX + 1
    val depth = goal.maxY + 1

    val piecePlacements: Map[Piece, Iterable[Piece]] = prototypes.map(proto =>
      proto -> (for {
        rotated <- proto.allRotations
        translated <- rotated.allTranslations(width - 1, depth - 1, height - 1) if translated.blocks.subsetOf(goal.blocks)
      } yield translated))(collection.breakOut)

    //for each location the set of rotated, translated pieces
    val occupiers: Map[(Int, Int, Int), Seq[(Piece, Piece)]] = (for {
      proto <- prototypes
      placed <- piecePlacements(proto)
      block <- placed.blocks
    } yield (block, (proto, placed))).groupBy(_._1).map {
      case (k, v) => k -> v.map(_._2)
    }

    //true if a piece (prototype) is used
    val enablePieceVars: Map[Piece, BVar] =
      prototypes.map(p => p -> BVar('pieceUsed, p))(collection.breakOut)

    //keys: first is prototype, second is placement of prototype
    //bvar is true if prototype is placed this way
    val vPlacement: Map[(Piece, Piece), BVar] = (for {
      proto <- prototypes
      place <- piecePlacements(proto)
      pp = (proto, place)
    } yield pp -> BVar('placePiece, pp))(collection.breakOut)

    //true if location is occupied
    val occupations: Map[(Int, Int, Int), BVar] = goal.blocks.map(l => l -> BVar('occupied, l))(collection.breakOut)

    //piece is either unused or placed exactly one way
    val placePiece: Iterable[OneOf] = for {
      (proto, enabled) <- enablePieceVars
      placements = piecePlacements(proto)
    } yield OneOf(Set(enabled.not) ++ placements.map(pl => vPlacement(proto -> pl).plain))

    //location is either unoccupied or occupied by exactly one placed piece
    val occupation: Iterable[OneOf] = for {
      (location, vOccupied) <- occupations
    } yield OneOf(occupiers(location).map(pp => vPlacement(pp).plain) :+ vOccupied.not)

    val placementClauses: Iterable[Set[Literal]] = placePiece.flatMap(_.clauses)
    val occupationClauses: Iterable[Set[Literal]] = occupation.flatMap(_.clauses)

    val goalEncoding: Iterable[Set[Literal]] = occupations.map {
      case (loc, bv) if goal.blocks.contains(loc) => Set(bv.plain)
      case (loc, bv) if !goal.blocks.contains(loc) => Set(bv.not)
    }

    val problemEncoding: Set[Set[Literal]] = placementClauses.toSet ++ occupationClauses.toSet ++ goalEncoding.toSet
    (problemEncoding,vPlacement.map{case ((proto,placed),v) => v -> placed})
  }
}
