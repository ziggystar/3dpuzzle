package puzzle3d

import csp.{CNF, Literal, OneOf, BVar}

/**
 * Created by thomas on 12/24/13.
 */


object Puzzle3D {
  def main(args: Array[String]) {
    val prototypes: Seq[Piece] = Piece.prototypes

    val goal = PieceParser.parse(PieceParser.piece, args(0)).get.normalize

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

    val (solver, varmap) = CNF.toProblem(placementClauses.toSet ++ occupationClauses.toSet ++ goalEncoding.toSet)

    val model = if(solver.isSatisfiable)
      Some(solver.model)
    else None

    model.foreach {
      sol =>
        val varToPlacement: Map[BVar, (Piece, Piece)] = vPlacement.map(_.swap)
        val trueVars: Array[BVar] = sol.filter(_ >= 0).map(varmap)
        //retrieve used (and placed) pieces
        val usedPieces: Array[Piece] = trueVars.collect(varToPlacement).map(_._2)
        val cm = usedPieces.zipWithIndex.foldLeft(CharMap(Map())) {
          case (acc, (p, idx)) => acc.addPiece(p, ('A' + idx).toChar)
        }
        println("First solution:")
        println(cm)
    }

    if(!model.isDefined){
      println("Not satisfiable.")
    }
  }
}
