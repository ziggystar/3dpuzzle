package puzzle3d

import scala.util.parsing.combinator.RegexParsers

object PieceParser extends RegexParsers{
  /** Create a piece from a sequence of layers. Each layer is a string, containing newlines to
    * separate rows.
    */
  def parseVoxel(s: Seq[Seq[String]]): Piece = {
    val blocks: Set[(Int, Int, Int)] = (for {
      (layer, z) <- s.zipWithIndex
      (line, y) <- layer.zipWithIndex
      (c, x) <- line.zipWithIndex if c == '#'
    } yield (x, y, z))(collection.breakOut)

    Piece(blocks)
  }

  /** Parse a 2d grid of heights, assuming no undercuts. */
  def parseSolid(s: Seq[String]): Piece = {
    val blocks: Set[(Int, Int, Int)] = (for {
      (row, y) <- s.zipWithIndex
      (heightChar, x) <- row.zipWithIndex
      height = heightChar - '0'
      z <- 0 until height
    } yield (x, y, z))(collection.breakOut)

    Piece(blocks)
  }

  def solidLine = """[0-9]*""".r
  def voxelLine = """[#\. ]*""".r
  def solidPiece: PieceParser.Parser[Piece] = "solid" ~ "(" ~> repsep(solidLine,",") <~ ")" ^^ parseSolid
  def voxelPiece: PieceParser.Parser[Piece] = "voxel" ~ "(" ~> repsep(repsep(voxelLine,","),"+") <~ ")" ^^ parseVoxel
  def piece: PieceParser.Parser[Piece] = solidPiece | voxelPiece
  def pieces: PieceParser.Parser[List[Piece]] = rep(piece)

  /** Parse a single file in 2D format. First line must only contain '2D'.
    * All following lines must contain only space or '.' for empty; or '#' for filled location.
    * @param lines
    * @return
    */
  def parse2D(lines: Iterable[String]): Either[String, Piece] = {
    def parseBody(lines: List[String]): Set[(Int,Int,Int)] = (for {
      (line,y) <- lines.zipWithIndex
      (c,x)    <- line.zipWithIndex if c == '#'
    } yield (x,y,0))(collection.breakOut)

    lines.toList match {
      case Nil =>
        Left("empty input")
      case head :: body if head == "2D" =>
        if(body.flatten.toSet.subsetOf(Set('.',' ','#')))
          Right(Piece(parseBody(body)))
        else
          Left("invalid character(s) found")
      case _ => Left("first line must only contain '2D'")
    }
  }
}
