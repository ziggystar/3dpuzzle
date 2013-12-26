package puzzle3d

import scala.collection.immutable.IndexedSeq

/**
 * Created by thomas on 12/24/13.
 */
case class Piece(blocks: Set[(Int,Int,Int)]){
  def rotate(nx: Int, ny: Int, nz: Int): Piece = {
    def rotx(block: (Int,Int,Int)) = block match {
      case (x,y,z) => (x,z,-y)
    }
    def roty(block: (Int,Int,Int)) = block match {
      case (x,y,z) => (z,y,-x)
    }
    def rotz(block: (Int,Int,Int)) = block match {
      case (x,y,z) => (y,-x,z)
    }
    val xed = Iterator.iterate(blocks)(_ map rotx).drop(nx).next()
    val yed = Iterator.iterate(xed)(_ map roty).drop(ny).next()
    val zed = Iterator.iterate(yed)(_ map rotz).drop(nz).next()
    Piece(zed)
  }

  def translate(tx: Int, ty: Int, tz: Int): Piece = Piece(blocks.map{case (x,y,z) => (x+tx,y+ty,z+tz)})

  def allRotations: Set[Piece] = (for {
    x <- 0 to 3
    y <- 0 to 3
    z <- 0 to 3
  } yield rotate(x, y, z).normalize)(collection.breakOut)

  /** inclusive, thus allTranslations(0,0,0) uses only block (0,0,0). */
  def allTranslations(rx: Int, ry: Int, rz: Int): IndexedSeq[Piece] = {
    for{
      tx <- -minX to (rx - maxX)
      ty <- -minY to (ry - maxY)
      tz <- -minZ to (rz - maxZ)
    } yield translate(tx,ty,tz)
  }

  /** Translates the piece such that the least block is (0,0,0). */
  def normalize: Piece = translate(-minX,-minY,-minZ)

  def minX = blocks.map(_._1).min
  def minY = blocks.map(_._2).min
  def minZ = blocks.map(_._3).min
  def maxX = blocks.map(_._1).max
  def maxY = blocks.map(_._2).max
  def maxZ = blocks.map(_._3).max

  override def toString: String = {
    blocks.toString + "\n" + CharMap(blocks.map(_ -> '#')(collection.breakOut)).toString
  }
}

case class CharMap(m: Map[(Int,Int,Int),Char]){
  def addPiece(p: Piece, c: Char): CharMap = CharMap(m ++ p.blocks.map(_ -> c))
  override def toString: String = {
    val stepX = m.keys.map(_._1).max + 2
    val maxX = (m.keys.map(_._3).max + 1) * stepX - 2
    val maxY = m.keys.map(_._2).max

    def makeLine(y: Int): String = (0 to maxX).map{
      case x if x % stepX == stepX - 1 => '|'
      case x if m.contains((x % stepX,y,x/stepX)) => m((x % stepX,y,x/stepX))
      case _ => '.'
    }.mkString

    (0 to maxY).map(makeLine).mkString("\n")
  }
}

object Piece{
  val prototypes: Seq[Piece] = Seq(
    Seq(
      """##
        | #""",
      """
        | #"""),
    Seq(
      """##
        | #""",
      """# """),
    Seq(
      """##
        | #""",
      """ #"""),
    Seq("###"),//I
    Seq(
      """###
        | # """),
    Seq(
      """###
        |#  """),
    Seq(
      """##
        |#  """),
    Seq(
      """##
        | ##""")
  ).map(fromASCII)

  /** Create a piece from a sequence of layers. Each layer is a string, containing newlines to
    * separate rows.
    */
  def fromASCII(layers: Seq[String]): Piece =
    Piece(
      (for {
        (layer, z) <- layers.zipWithIndex
        (line, y) <- layer.stripMargin.split("\n").zipWithIndex
        (c, x) <- line.zipWithIndex if c == '#'
      } yield (x, y, z))(collection.breakOut))
}