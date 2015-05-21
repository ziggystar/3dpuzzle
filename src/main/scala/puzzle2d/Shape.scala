package puzzle2d

case class Shape(locations: Set[Location]) {
  def minX = locations.map(_.x).min
  def minY = locations.map(_.y).min
  def maxX = locations.map(_.x).max
  def maxY = locations.map(_.y).max
  def width = maxX - minX + 1
  def height = maxY - minY + 1
  def translate(x: Int, y: Int): Shape = Shape(locations.map{case Location(xx,yy) => Location(xx + x, yy + y)})
  def normalize: Shape = translate(-minX,-minY)
  /** Rotate this shape in 90° steps clockwise around the (the lower left corner of) point (xr,yr). */
  def rotateOrigin(steps: Int, xr: Int = 0, yr: Int = 0): Shape = {
    /** Rotate a single Location 90° clock-wise around the origin. */
    def rot1(block: Location): Location = block match {
      case Location(x, y) => Location(y, -x)
    }
    def rotn(block: Location): Location = {
      val requiredRotations: Int = ((steps % 4) + 4) % 4 //mod with positive results only
      Iterator.iterate(block)(rot1).drop(requiredRotations).next()
    }
    Shape(this.translate(-xr,-yr).locations.map(rotn)).translate(xr,yr)
  }
  /** Mirror along the y-axis. */
  def mirror: Shape = Shape(locations.map{case Location(x,y) => Location(-x,y)})
  def union(other: Shape): Shape = Shape(locations.union(other.locations))
  def isContainedIn(other: Shape): Boolean = this.locations.subsetOf(other.locations)
  def flip(l: Location) = if(locations(l)) Shape(locations - l) else Shape(locations + l)
  def +(l: Location): Shape = Shape(locations + l)
  def -(l: Location): Shape = Shape(locations - l)
  def allTranslationsWithin(left: Int, bottom: Int, right: Int, top: Int): Iterable[Shape] = {
    val n = normalize
    for{
      tx <- left to (right - width + 1)
      ty <- bottom to (top - height + 1)
    } yield n.translate(tx,ty)
  }
  override def toString: String =
    (minY to maxY).map(y => (minX to maxX).map(x => if(locations(Location(x,y))) '#' else ' ')).mkString("\n")
}

object Shape {
  def empty: Shape = Shape(Set())
  /** Parse an ascii art of a shape.
    * @param blockChar Character used to denote occupied locations.
    */
  def parseString(s: String, blockChar: Char = '#', normalize: Boolean = true): Shape = {
    val locations: Seq[Location] = for{
      (line,y) <- s.lines.toSeq.reverse.zipWithIndex
      (c,x)    <- line.zipWithIndex if c == blockChar
    } yield Location(x,y)
    (if(normalize) (_:Shape).normalize else identity[Shape] _)(Shape(locations.toSet))
  }
}
