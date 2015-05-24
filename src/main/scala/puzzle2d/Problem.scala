package puzzle2d

import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory

import scala.util.Try

/** A problem consists of a [[PieceSet]] and a [[Shape]] that has to be filled. */
case class Problem(goal: Shape, set: PieceSet, allowMultiPlacement: Boolean = false){
  case class Solution(placement: Seq[Shape]){
    require(isValid, "solution is not valid")
    def isValid: Boolean = {
      val usedValidPieces = {
        val used = placement.map(Piece(_)).groupBy(identity)
        used.keySet.forall{case k => used(k).size <= set.pieces(k)}
      }
      val noOverlap = placement.flatMap(_.locations).groupBy(identity).forall(_._2.size == 1)
      val goalCovered = placement.reduce(_.union(_)).locations == goal.locations
      usedValidPieces && goalCovered && noOverlap
    }

    def prettyPrint: String = util.pretty(placement.map(_.locations.map(_.asTuple))(collection.breakOut))
  }

  case class Placed(prototype: Piece, place: Shape)

  def solve: Option[Solution] = {
    try {
      val solver = SolverFactory.newDefault()

      val placements: IndexedSeq[Placed] = (for {
        piece <- set.pieces.keys if set.pieces(piece) > 0
        rotation <- piece.shapes
        translation <- rotation.allTranslationsWithin(goal.minX, goal.minY, goal.maxX, goal.maxY) if translation isContainedIn goal
      } yield Placed(piece, translation))(collection.breakOut)

      //maps Placed objects to variable indices
      val vars: Map[Placed, Int] = placements.zip(Stream.from(1)).toMap
      val back: Map[Int, Placed] = vars.map(_.swap)

      //every location has exactly one occupation
      goal.locations.foreach { l =>
        solver.addExactly(new VecInt(vars.collect { case (Placed(_, place), vi) if place.locations(l) => vi }(collection.breakOut): Array[Int]), 1)
      }

      //use only the allowed number of pieces of each type
      if (!allowMultiPlacement) {
        set.pieces.foreach { case (piece, max) =>
          solver.addAtMost(new VecInt(vars.collect { case (pl, vi) if pl.prototype == piece => vi }(collection.breakOut): Array[Int]), max)
        }
      }

      Some(solver).filter(_.isSatisfiable).map {
        _.model.filter(_ > 0).map(back).map(_.place)
      }.map(Solution(_))
    } catch {
      case e: Exception =>
        System.err.println(e)
        None
    }
  }
}

object Problem {
  val empty = Problem(Shape.empty, PieceSet(Map.empty))
}
