package puzzle2d

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
  /** Mirror along the y-axis. */
  def flip: Shape = Shape(locations.map{case Location(x,y) => Location(-x,y)})
  def union(other: Shape): Shape = Shape(locations.union(other.locations))
  def flip(l: Location) = if(locations(l)) Shape(locations - l) else Shape(locations + l)
}

object Shape {
  def empty: Shape = Shape(Set())
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

/** An equivalence class for shapes. Allows rotation, translation and flipping. */
class Piece(prototype: Shape){
  /** The prototype rotated, flipped and normalized. */
  val shapes: Set[Shape] = for{
    flipped <- Set(prototype,prototype.flip)
    rotated <- (0 to 3).map(flipped.rotateOrigin(_))
  } yield rotated.normalize

  val representative: Shape = shapes.head

  override val hashCode: Int = shapes.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Piece => other.shapes == shapes
    case _ => false
  }
}

object Piece{
  def apply(s: Shape): Piece = new Piece(s)
}

/** A multi-set of pieces. */
case class PieceSet(pieces: Map[Piece,Int]) {
  def toSeq = pieces.flatMap{case (s,count) => Seq.fill(count)(s)}
}

/** A problem consists of a [[PieceSet]] and a [[Shape]] that has to be filled. */
case class Problem(goal: Shape, set: PieceSet, allowMultiPlacement: Boolean = false){
  require(!allowMultiPlacement, "multiple placement not implemented yet")
}

case class Solution(problem: Problem, placement: Seq[Shape]){
  require(isValid, "solution is not valid")
  def isValid: Boolean = {
    val usedValidPieces = {
      val used = placement.map(Piece(_)).groupBy(identity)
      used.keySet.forall{case k => used(k).size <= problem.set.pieces(k)}
    }
    val noOverlap = placement.flatMap(_.locations).groupBy(identity).forall(_._2.size == 1)
    val goalCovered = placement.reduce(_.union(_)).locations == problem.goal.locations
    usedValidPieces && goalCovered && noOverlap
  }

  def prettyPrint: String = util.pretty(placement.map(_.locations.map(_.asTuple))(collection.breakOut))
}

object Solver2D{
  def solve(problem: Problem): Option[Solution] = {
    import puzzle3d.{Piece => P3D}
    def shape2Piece(s: Shape) = P3D(s.locations.map{case Location(x,y) => (x,y,0)})
    def p3d2Shape(p: P3D) = Shape(p.blocks.map{case (x,y,z) if z == 0=> Location(x,y)})
      .ensuring(_.locations.size == p.blocks.size, "3d to 2d shape conversion failed because blocks with z!=0 found")

    val (solution, stats): (Option[Set[P3D]], Map[String, Number]) = puzzle3d.Puzzle3D.solveInstance(
      puzzle3d.Puzzle3D.Config(Seq(), problem.set.toSeq.map(p => shape2Piece(p.representative))(collection.breakOut), problem.allowMultiPlacement,printSolverStats = false),
      shape2Piece(problem.goal)
    )
    solution.map(pieceSet => Solution(problem,pieceSet.map(p3d2Shape)(collection.breakOut)))
  }
}

