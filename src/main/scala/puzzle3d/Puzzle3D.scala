package puzzle3d

import csp._
import scopt.Read
import java.io.{FileReader, File}

import scala.io.Source

object Puzzle3D {
  implicit val pieceListReader: Read[Piece] = Read.reads(s => PieceParser.parseAll(PieceParser.piece,s).get)

  case class Config(problems: Seq[Piece] = Seq(),
                    prototypes: Seq[Piece] = Piece.prototypes,
                    multiplePlacement: Boolean = false,
                    printSolverStats: Boolean = false)

  val parser = new scopt.OptionParser[Config]("puzzle3d") {
    head("puzzle3d", "1.0")
    opt[Piece]('p', "problem") action { (x, c) =>
      c.copy(problems = c.problems :+ x) } text "add a problem" valueName "<piece-description>"
    opt[File]('f', "file") valueName "<file>" action { (x, c) =>
      c.copy(problems = c.problems ++ PieceParser.parseAll(PieceParser.pieces, new FileReader(x)).get)
    } text "read a list of problems from a file"
    opt[File]("file-2d") valueName "<file>" action { (x, c) =>
      c.copy(problems = c.problems :+ PieceParser.parse2D(Source.fromFile(x).getLines().toList).fold(sys.error,identity))
    } text "read a problem from a file in 2D format"
    opt[Unit]('v',"verbose") action { (_, c) =>
      c.copy(printSolverStats = true) } text "verbose is a flag"
    opt[Unit]('m',"allow-multiple")
      .action{ (_, c) => c.copy(multiplePlacement = true) }
      .text("allow placing the same piece multiple times")
    opt[Unit]('t', "tetris")
      .action( (_,c) => c.copy(multiplePlacement = true, prototypes = Piece.prototypesTetris))
      .text("use the seven tetraminos from Tetris, also allow multiple placement")
    note("some notes.\n")
    help("help") text "prints this usage text"
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
    val problemEncoding = constructSATProblem(goal, prototypes, config)
    val (solver, solution) = CSP.solve(problemEncoding)

    val decodedSolution = solution.map{_.collect{case Placed(_,p) => p}}
    import collection.JavaConverters._
    (decodedSolution,solver.getStat.asScala.toMap)
  }

  def prettyPrintSolution(sol: Set[Piece]): String = sol.zipWithIndex.foldLeft(CharMap(Map())) {
    case (acc, (p, idx)) => acc.addPiece(p, ('A' + idx).toChar)
  }.toString
  
  sealed trait PuzzleBV
  //true if a piece (prototype) is used
  case class PieceUsed(proto: Piece) extends PuzzleBV
  case class Placed(proto: Piece, placed: Piece) extends PuzzleBV
  case class Occupied(loc: (Int,Int,Int)) extends PuzzleBV

  /** Construct the SAT encoding of the problem, together with a mapping of boolean variables to the
    * placed pieces.
    * @param goal The blocks to fill with pieces.
    * @param prototypes At most one of each prototype may be placed, rotated and translated.
    * @return Encoding as set of clauses and a mapping from variables to placed pieces.
    */
  def constructSATProblem(goal: Piece, prototypes: Seq[Piece], config: Config): Iterable[Constraint[PuzzleBV]] = {
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
    val enablePieceVars: Map[Piece, PuzzleBV] =
      prototypes.map(p => p -> PieceUsed(p))(collection.breakOut)

    //keys: first is prototype, second is placement of prototype
    //bvar is true if prototype is placed this way
    lazy val vPlacement: Map[(Piece, Piece), PuzzleBV] = (for {
      proto <- prototypes
      place <- piecePlacements(proto)
      pp = (proto, place)
    } yield pp -> Placed(pp._1,pp._2))(collection.breakOut)

    //true if location is occupied
    lazy val occupations: Map[(Int, Int, Int), PuzzleBV] = goal.blocks.map(l => l -> Occupied(l))(collection.breakOut)

    //piece is either unused or placed exactly one way
    lazy val placePiece: Iterable[OneOf[PuzzleBV]] = for {
      (proto, enabled) <- enablePieceVars
      placements = piecePlacements(proto)
    } yield OneOf(Set(Negated(enabled)) ++ placements.map(pl => Plain(vPlacement(proto -> pl))))

    //location is either unoccupied or occupied by exactly one placed piece
    lazy val occupation: Iterable[OneOf[PuzzleBV]] = for {
      (location, vOccupied) <- occupations
    } yield OneOf(occupiers(location).map(pp => Plain(vPlacement(pp))) :+ Negated(vOccupied))

    val goalEncoding: Iterable[AtLeastOne[PuzzleBV]] = occupations.map {
      case (loc, bv) if goal.blocks.contains(loc) => AtLeastOne(Set(Plain(bv)))
      case (loc, bv) if !goal.blocks.contains(loc) => AtLeastOne[PuzzleBV](Set(Negated(bv)))
    }

    occupation ++ goalEncoding ++ (if (config.multiplePlacement) Set() else placePiece)
  }
}
