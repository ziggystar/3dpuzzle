package puzzle2d

/** A spot on a 2d grid. */
case class Location(x: Int, y: Int) {
  import util.Dir._
  def asTuple: (Int, Int) = (x,y)
  def step(d: Dir): Location = d match {
    case Top => Location(x,y-1)
    case Bottom => Location(x,y+1)
    case Left => Location(x-1,y)
    case Right => Location(x+1,y)
  }
}
