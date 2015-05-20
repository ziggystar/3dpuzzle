package puzzle3d

import probability_monad.Distribution
import Distribution._
import puzzle3d.Puzzle3D.Config
import scopt.OptionParser

import scala.util.Random

/** Generate 2d problems. */
object Generator2D {
  type Voxel = (Int,Int)

  val tetrisPuzzleConfig: Config = Puzzle3D.Config(prototypes = Piece.prototypesTetris, multiplePlacement = true)
  
  case class RunConfig(inputString: String = "", rngSeed: Option[Long] = None, nPieces: Int = 4)
  val cliParser = new OptionParser[RunConfig]("gen-tetris-challenge") {
    arg[String]("input-string")
      .action{case (x,c) => c.copy(inputString = x)}
      .text("string to be encoded")
      .required()
    opt[Int]('n',"problem-size")
      .action{case (x,c) => c.copy(nPieces = x
    )}
      .text{"number of blocks/4 for generated problems"}
    opt[Long]('s',"random-seed")
      .action{case (x,c) => c.copy(rngSeed = Some(x))}
      .text{"seed for random generator (default is time)"}
  }

  def toPiece(vs: Set[Voxel]): Piece = Piece(vs.map{case (x,y) => (x,y,0)})
  def fromPiece(p: Piece): Set[Voxel] = p.blocks.map{case (x,y,_) => (x,y)}
  def normalize(vs: Set[Voxel]): Set[Voxel] = {
    val minx = vs.map(_._1).min
    val miny = vs.map(_._2).min
    vs.map{case (x,y) => (x-minx,y-miny)}
  }

  def solve(problem: Set[Voxel]): (Option[Set[Set[(Int, Int)]]], Map[String, Number]) = {
    val (solution, stats) = Puzzle3D.solveInstance(tetrisPuzzleConfig, toPiece(problem))
    (solution.map(_.map(fromPiece)), stats)
  }

  def char2Bits(c: Char): Seq[Boolean] = Seq.iterate(c.toInt,8)(_ >> 1).map(b => (b & 1) == 1).reverse

  def stringToBits(a: String): Seq[Boolean] = a.flatMap(char2Bits)

  def main(args: Array[String]) {
    cliParser.parse(args,RunConfig()).foreach{config =>
      val gen: Distribution[Set[(Int, Int)]] = grow(n = config.nPieces * 4 - 1).map(normalize)
      config.rngSeed.foreach(Random.setSeed)

      val generatorWithSolver: Distribution[(Set[(Int, Int)], (Option[Set[Set[(Int, Int)]]], Map[String, Number]))] =
        gen.map(p => p -> solve(p))

      val bitstream = stringToBits(config.inputString)
      val problems = bitstream.map(b => generatorWithSolver.filter(_._2._1.isDefined == b).sample(1).head)

      println(problems.map(ps => printProblem(ps._1)).mkString("\nNEXT\n"))

      def printDebug(x: (Set[(Int, Int)], (Option[Set[Set[(Int, Int)]]], Map[String, Number]))): String = {
        x._2._1.map(pretty).getOrElse(pretty(Set(x._1)))
      }

      System.err.println(problems.map(printDebug).mkString("\n"))
    }
//    println("abc".map(char2Bits).mkString("\n"))
//    println("abc".map(c => printf("%h\n",c.toInt)))
//    sys.exit()

    //println(gen.map(solve).map(_._1.isDefined).hist)
  }

  def printProblem(vs: Set[Voxel]): String = {
    val xmin = vs.map(_._1).min
    val ymin = vs.map(_._2).min
    val ymax = vs.map(_._2).max
    val xmax = vs.map(_._1).max

    val chars = for (y <- ymin to ymax)
    yield for (x <- xmin to xmax) yield if (vs.contains((x, y))) '#' else ' '
    chars.map(_.mkString).mkString("\n")
  }

  val seed = always(Set((0,0)))



  def pretty(vs: Set[Set[Voxel]]): String = util.pretty(vs)

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
