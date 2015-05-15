
package object util {
  /** Pretty print a set of 2d shapes with ascii art. */
  def pretty(vs: Set[Set[(Int,Int)]]): String = {
    import Dir._

    val xmin = vs.flatten.map(_._1).min
    val xmax = vs.flatten.map(_._1).max
    val ymin = vs.flatten.map(_._2).min
    val ymax = vs.flatten.map(_._2).max

    //in which direction is a join? (same piece)
    val table: Map[Set[Dir],Char] = Map(
      Set[Dir]() -> '╬',
      Set(Top) -> '╦',
      Set(Left) -> '╠',
      Set(Right) -> '╣',
      Set(Bottom) -> '╩',
      Set(Top,Bottom) -> '═',
      Set(Left,Right) -> '║',
      Set(Top,Left) -> '╔',
      Set(Top,Right) -> '╗',
      Set(Bottom,Left) -> '╚',
      Set(Bottom,Right) -> '╝',
      Set(Bottom,Top,Right,Left) -> ' '
    )
    def pieceAt(x: Int, y: Int): Option[Set[(Int, Int)]] = vs.find(_.contains((x,y)))
    def charAt(px: Int, py: Int): Char = {
      val setsAt = Map(
        Top -> (pieceAt(px,py) == pieceAt(px+1,py)),
        Bottom -> (pieceAt(px,py+1) == pieceAt(px+1,py+1)),
        Left -> (pieceAt(px,py) == pieceAt(px,py+1)),
        Right -> (pieceAt(px+1,py) == pieceAt(px+1,py+1))
      )
      table(setsAt.filter(_._2).keySet)
    }
    val lines = for {
      y <- (ymin-1) to ymax
    } yield (for {
        x <- (xmin-1) to xmax
      } yield charAt(x,y)).mkString
    lines.mkString("\n")
  }
}
