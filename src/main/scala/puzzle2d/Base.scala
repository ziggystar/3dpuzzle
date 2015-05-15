package puzzle2d

case class Location(x: Int, y: Int) {
  def asTuple: (Int, Int) = (x,y)
}
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

/** A multi-set of pieces. */
case class PieceSet(pieces: Map[Shape,Int]) {
  def toSeq = pieces.flatMap{case (s,count) => Seq.fill(count)(s)}
}

/** A problem consists of a [[PieceSet]] and a [[Shape]] that has to be filled. */
case class Problem(goal: Shape, set: PieceSet, allowMultiPlacement: Boolean = false){
  require(!allowMultiPlacement, "multiple placement not implemented yet")
}

case class Solution(problem: Problem, placement: Seq[Shape]){
  require(validate, "solution is not valid")
  def validate: Boolean = {
    val usedValidPieces = {
      val used = placement.groupBy(identity)
      used.keySet.forall{case k => used(k).size <= problem.set.pieces(k)}
    }
    val noOverlap = placement.flatMap(_.locations).groupBy(identity).forall(_._2.size == 1)
    val goalCovered = placement.reduce(_.union(_)).locations == problem.goal
    usedValidPieces && goalCovered && noOverlap
  }

  def prettyPrint: String = util.pretty(placement.map(_.locations.map(_.asTuple))(collection.breakOut))
}

object Solver2D{
  def solve(problem: Problem): Option[Solution] = ???
}

