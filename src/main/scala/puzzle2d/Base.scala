package puzzle2d

case class Location(x: Int, y: Int)
case class Shape(locations: Set[Location]){
  def minX = locations.map(_.x).min
  def minY = locations.map(_.y).min
  def maxX = locations.map(_.x).max
  def maxY = locations.map(_.y).max
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
  def union(other: Shape): Shape = Shape(locations.union(other.locations))
}

object Shape {
  /** Parse an ascii art of a shape.
    * @param s
    * @param blockChar Character used to denote occupied locations.
    */
  def parseString(s: String, blockChar: Char = '#', normalize: Boolean = true): Shape = {
    val locations: Seq[Location] = for{
      (line,y) <- s.lines.toSeq.reverse.zipWithIndex
      (c,x)    <- line.zipWithIndex if c == blockChar
    } yield Location(x,y)
    (if(normalize) (_:Shape).normalize else identity[Shape](_))(Shape(locations.toSet))
  }
}

/** A set of pieces, annotated with values of type `A`. */
case class PieceSet[A](annotatedPieces: Seq[(Shape,A)])

/** A problem consists of a [[PieceSet]] and a [[Shape]] that has to be filled. */
case class Problem[A](goal: Shape, set: PieceSet[A], allowMultiPlacement: Boolean = false){
  require(!allowMultiPlacement, "multiple placement not implemented yet")
}

case class Solution[A](problem: Problem[A], placement: Seq[(Shape,A)]){
  require(validate, "solution is not valid")
  def validate: Boolean = {
    val usedValidPieces = {
      val used = placement.groupBy(identity)
      val provided = problem.set.annotatedPieces.groupBy(identity)
      used.keySet.forall{case k => used(k).size <= provided(k).size}
    }
    val goalCovered = placement.map(_._1).reduce(_.union(_)).locations == problem.goal
    usedValidPieces && goalCovered
  }

  def prettyPrint: String = ???
}

object Solver2D{
  def solve[A](problem: Problem[A]): Option[Solution[A]] = ???
}

