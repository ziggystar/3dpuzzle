package csp

import puzzle3d.Piece
import java.io.{FileWriter, File}

/**
 * Created by thomas on 12/24/13.
 */

trait CNF {
  def clauses: Set[Set[Literal]]
}

object CNF{
  def dimacs(clauses: Set[Set[Literal]]): String = {
    val varToIndex = clauses.flatten.map(_.variable).toSeq.distinct.zipWithIndex.toMap
    val numVars = varToIndex.size
    val clauseString = clauses.map{cl =>
      (cl.map{
        case Negation(v) => -(varToIndex(v) + 1)
        case Plain(v) => varToIndex(v) + 1
      }.toSeq :+ 0).mkString(" ")
    }.mkString("\n")
    f"p cnf ${numVars + 1} ${clauses.size}\n$clauseString"
  }
}

case class OneOf(literals: Iterable[Literal]) extends CNF{
  def clauses: Set[Set[Literal]] = literals.toSeq match{
    case Seq() => error("requiring one of empty literal-set to be true would result in empty clause")
    case Seq(x) => Set(Set(x))
    case Seq(l1,l2) => Set(Set(l1.not,l2.not),Set(l1,l2))
    case Seq(l1,l2,l3) => Set(Set(l1.not,l2.not,l3.not))
    case xs =>
      val (ls1,ls2) = xs.splitAt(xs.size/2)
      val oo2: BVar = BVar('oneof, ls2)
      val oo1: BVar = BVar('oneof, ls1)
      OneOf(ls1 :+ oo1.not).clauses ++ OneOf(ls2 :+ oo2.not).clauses ++ OneOf(Seq(oo1.plain,oo2.plain)).clauses
  }
}

case class BVar(name: Symbol, data: Any){
  def not: Literal = Negation(this)
  def plain: Literal = Plain(this)

  override val hashCode: Int = (name,data).hashCode()
}

sealed trait Literal{
  def variable: BVar
  def sign: Boolean
  def not: Literal
}
case class Negation(variable: BVar) extends Literal{
  def sign: Boolean = false
  def not: Literal = Plain(variable)
}
case class Plain(variable: BVar) extends Literal{
  def sign: Boolean = true
  def not: Literal = Negation(variable)
}

object Puzzle3D extends App{

  val maxHeight = 0
  val goal = Piece.fromASCII(
    Seq(
      """# ##
        |####
      """.stripMargin)
  )

  val locations: IndexedSeq[(Int, Int, Int)] = for{
    x <- 0 to 4
    y <- 0 to 4
    z <- 0 to maxHeight
  } yield (x,y,z)

  val piecePlacements: Map[Piece, Iterable[Piece]] = Piece.prototypes.map(proto =>
    proto -> (for{
      rotated <- proto.allRotations
      translated <- rotated.allTranslations(0 to 4,0 to 4,0 to maxHeight)
  } yield translated))(collection.breakOut)

  //for each location the set of rotated, translated pieces
  val occupiers: Map[(Int,Int,Int),Seq[(Piece,Piece)]] = (for{
    proto <- Piece.prototypes
    placed <- piecePlacements(proto)
    block <- placed.blocks
  } yield (block, (proto,placed))).groupBy(_._1).map{case (k,v) => k -> v.map(_._2)}

  //true if a piece (prototype) is used
  val enablePieceVars: Map[Piece,BVar] =
    Piece.prototypes.map(p => p -> BVar('pieceUsed,p))(collection.breakOut)

  //keys: first is prototype, second is placement of prototype
  //bvar is true if prototype is placed this way
  val vPlacement: Map[(Piece,Piece),BVar] = (for {
    proto <- Piece.prototypes
    place <- piecePlacements(proto)
    pp = (proto, place)
  } yield pp -> BVar('placePiece, pp))(collection.breakOut)

  //true if location is occupied
  val occupations: Map[(Int,Int,Int),BVar] = locations.map(l => l -> BVar('occupied,l))(collection.breakOut)

  //piece is either unused or placed exactly one way
  val placePiece: Iterable[OneOf] = for{
    (proto,enabled) <- enablePieceVars
    placements = piecePlacements(proto)
  } yield OneOf(Set(enabled.not) ++ placements.map(pl => vPlacement(proto -> pl).plain))

  //location is either unoccupied or occupied by exactly one placed piece
  val occupation: Iterable[OneOf] = for{
    (location,vOccupied) <- occupations
  } yield OneOf(occupiers(location).map(pp => vPlacement(pp).plain) :+ vOccupied.not)


  private val placementClauses: Iterable[Set[Literal]] = placePiece.flatMap(_.clauses)
  private val occupationClauses: Iterable[Set[Literal]] = occupation.flatMap(_.clauses)

  val goalEncoding: Iterable[Set[Literal]] = occupations.map{
    case (loc,bv) if goal.blocks.contains(loc) => Set(bv.plain)
    case (loc,bv) if !goal.blocks.contains(loc) => Set(bv.not)
  }
  val encoding = CNF.dimacs(placementClauses.toSet ++ occupationClauses.toSet ++ goalEncoding.toSet)
  val cnfFile = new File("puzzle.cnf")

  import resource._

  for(fos <- managed(new FileWriter(cnfFile))){
    fos.write(encoding)
  }

  println(f"placements:${piecePlacements.map{case (p,pls) => f"$p: ${pls.size}"}.mkString("\n")}")
}
