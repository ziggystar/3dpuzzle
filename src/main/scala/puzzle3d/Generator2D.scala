package puzzle3d

import probability_monad.Distribution
import Distribution._

/**
 * Created by thomas on 04.01.15.
 */
object Generator2D {
  type Voxel = (Int,Int)

  val tetrisConfig = Puzzle3D.Config(prototypes = Piece.prototypesTetris, multiplePlacement = true)

  def toPiece(vs: Set[Voxel]): Piece = Piece(vs.map{case (x,y) => (x,y,0)})
  def fromPiece(p: Piece): Set[Voxel] = p.blocks.map{case (x,y,_) => (x,y)}
  def normalize(vs: Set[Voxel]): Set[Voxel] = {
    val minx = vs.map(_._1).min
    val miny = vs.map(_._2).min
    vs.map{case (x,y) => (x-minx,y-miny)}
  }

  def solve(problem: Set[Voxel]): (Option[Set[Set[(Int, Int)]]], Map[String, Number]) = {
    val (solution, stats) = Puzzle3D.solveInstance(tetrisConfig, toPiece(problem))
    (solution.map(_.map(fromPiece)), stats)
  }

  def main(args: Array[String]) {
    val gen: Distribution[Set[(Int, Int)]] = grow(n = 30 * 4 - 1).map(normalize)

    println(gen.map(solve).map(_._2("decisions")).map(_.doubleValue).bucketedHist(10))

    val p = gen.sample(1).head

    println(pretty(Set(p)))

    val (solution: Option[Set[Set[(Int, Int)]]], stats) = solve(p)

    println(solution.map(pretty).getOrElse("no solution found"))

    println()

    println(stats.mkString("\n"))
  }

  val seed = always(Set((0,0)))

  object Dir extends Enumeration {
    val Top,Bottom,Left,Right = Value
    type Dir = Dir.Value
  }

  def pretty(vs: Set[Set[Voxel]]): String = {
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

  def neighbours(voxel: Voxel): Seq[Voxel] = {
    val (x,y) = voxel
    Seq((x-1,y),(x+1,y),(x,y+1),(x,y-1))
  }

  def grow(p: Distribution[Set[Voxel]] = seed, n: Int = 1): Distribution[Set[Voxel]] =
    if(n == 0) p
    else {
      val grown: Distribution[Set[Voxel]] = for{
        vs <- p
        newN <- discreteUniform(vs.flatMap(neighbours) -- vs)
      } yield vs + newN
      grow(grown, n - 1)
    }
}