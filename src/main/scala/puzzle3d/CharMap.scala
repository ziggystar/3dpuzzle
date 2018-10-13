package puzzle3d

case class CharMap(m: Map[(Int, Int, Int), Char]) {
  def addPiece(p: Piece, c: Char): CharMap = CharMap(m ++ p.blocks.map(_ -> c))

  override def toString: String = {
    val stepX = m.keys.map(_._1).max + 2
    val maxX = (m.keys.map(_._3).max + 1) * stepX - 2
    val maxY = m.keys.map(_._2).max

    def makeLine(y: Int): String = (0 to maxX).map {
      case x if x % stepX == stepX - 1 => '|'
      case x if m.contains((x % stepX, y, x / stepX)) => m((x % stepX, y, x / stepX))
      case _ => '.'
    }.mkString

    (0 to maxY).map(makeLine).mkString("\n")
  }
}
