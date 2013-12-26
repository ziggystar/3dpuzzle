package csp

import puzzle3d.Piece
import java.io.{FileWriter, File}

/**
 * Created by thomas on 12/24/13.
 */


object Puzzle3D extends App{
  val prototypes: Seq[Piece] = Piece.prototypes

  val height = 1
  val width = 5
  val depth = 5

  val goal = Piece.fromASCII(
    Seq(
      """  ###
        | ####
        |#####
        |###
        |###
      """.stripMargin)
  )

  val locations: IndexedSeq[(Int, Int, Int)] = for{
    x <- 0 until width
    y <- 0 until depth
    z <- 0 until height
  } yield (x,y,z)

  val piecePlacements: Map[Piece, Iterable[Piece]] = prototypes.map(proto =>
    proto -> (for{
      rotated <- proto.allRotations
      translated <- rotated.allTranslations(width-1,depth-1,height-1)
  } yield translated))(collection.breakOut)

  //for each location the set of rotated, translated pieces
  val occupiers: Map[(Int,Int,Int),Seq[(Piece,Piece)]] = (for{
    proto <- prototypes
    placed <- piecePlacements(proto)
    block <- placed.blocks
  } yield (block, (proto,placed))).groupBy(_._1).map{case (k,v) => k -> v.map(_._2)}

  //true if a piece (prototype) is used
  val enablePieceVars: Map[Piece,BVar] =
    prototypes.map(p => p -> BVar('pieceUsed,p))(collection.breakOut)

  //keys: first is prototype, second is placement of prototype
  //bvar is true if prototype is placed this way
  val vPlacement: Map[(Piece,Piece),BVar] = (for {
    proto <- prototypes
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

  import sys.process._
  "relsat -#c puzzle.cnf" !
}
