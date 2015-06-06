package puzzle2d.gui

import java.awt._

import org.sat4j.specs.{TimeoutException, ContradictionException}
import puzzle2d.Shape
import puzzle2d._
import rx.lang.scala.schedulers.NewThreadScheduler
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subject, Observable}
import util.rx._

import scala.concurrent.Future
import scala.swing.event._
import scala.swing.{Dimension, Graphics2D, Component}
import rx.lang.scala.ExperimentalAPIs._
import scala.concurrent.ExecutionContext.Implicits.global

/** A [[scala.swing.Component]] that allows viewing and editing a [[puzzle2d.Shape]].
  * This component displays editable goal shapes. It also takes a trigger to attempt solving.
  * If a solution is found, it is also displayed.
  */
class Board(val setShape: Observable[Shape] = Observable.empty,
            val solveTrigger: Observable[Unit] = Observable.empty,
            val pieceSet: Observable[PieceSet]) extends Component{
  minimumSize = new Dimension(200,200)

  sealed trait SolveState {
    def problem: Problem
  }

  object SolveState {
    def attempt(p: Problem): SolveState = p.solve(10) match {
      case s: p.Solution => Solved(p, s)
      case p.Timeout => TimeOut(p)
      case p.Unsolvable => Unsolvable(p, Shape.empty)
    }
  }

  case class Unattempted(problem: Problem) extends SolveState
  case class Solved(problem: Problem, solution: Problem#Solution) extends SolveState
  case class Unsolvable(problem: Problem, contradiction: Shape) extends SolveState
  case class TimeOut(problem: Problem) extends SolveState
  case class Solving(problem: Problem) extends SolveState

  case class Transformation(trX: Int, trY: Int, width: Int){
    def locToScreen(l: Location): Rectangle = new Rectangle(l.x * width + trX, l.y * width + trY, width, width)
    def screenToLoc(scrX: Int, scrY: Int) = Location((scrX - trX) / width, (scrY - trY) / width )
    def screenToLoc(p: Point) = Location((p.x - trX) / width, (p.y - trY) / width )
  }

  private val origin: BehaviorSubject[(Int, Int)] = BehaviorSubject[(Int,Int)]((0,0))
  private val cellSize: BehaviorSubject[Int] = BehaviorSubject[Int](30)
  val transformation: Observable[Transformation] =
    origin.combineLatestWith(cellSize){case ((tx,ty),w) => Transformation(tx,ty,w)}

  private val mouseMoves: Subject[MouseMoved] = Subject()
  private val mousePressed: Subject[MousePressed] = Subject()
  private val mouseReleased: Subject[MouseReleased] = Subject()
  private val mouseDragged: Subject[MouseDragged] = Subject()
  this.listenTo(this.mouse.moves)
  this.listenTo(this.mouse.clicks)
  reactions += {
    case e: MouseMoved => mouseMoves.onNext(e)
    case e: MousePressed => mousePressed.onNext(e)
    case e: MouseReleased => mouseReleased.onNext(e)
    case e: MouseDragged => mouseDragged.onNext(e)
  }

  def toLocation(me: Observable[MouseEvent]): Observable[Location] =
    me.withLatestFrom(transformation)((e,t) => t.screenToLoc(e.point))

  val drags: Observable[Observable[MouseEvent]] = mousePressed.map(mp => Observable.just(mp) ++ mouseDragged.takeUntil(mouseReleased))

  def applyDragsToShape(s: Shape, ds: Observable[Observable[Location]]): Observable[Shape] = {
    val annotated = ds.flatMap(drag => drag zip Observable.from(Stream(true) ++ Stream.continually(false)))
    annotated.scan((s,false)){
      case ((os,mode),(l,true)) if os.locations(l)  => (os - l, false)
      case ((os,mode),(l,true)) if !os.locations(l) => (os + l, true)
      case ((os,false),(l,false)) => (os - l, false)
      case ((os,true),(l,false)) => (os + l, true)
    }.map(_._1)
  }

  val boardState: Observable[Shape] =
    (Observable.just(Shape.empty) ++ setShape)
      .switchMap(
        base => Observable.just(base) ++
          applyDragsToShape(base,drags.map(d => toLocation(d).distinctUntilChanged))
      )

  val problem: Observable[Problem] = boardState.combineLatestWith(pieceSet)(Problem(_,_)).distinctUntilChanged

  val solutionState: Observable[SolveState] =
    (solveTrigger.withLatestFrom(problem){
      (_,p) =>
        Observable.just(Solving(p)) ++ Observable.from(Future.apply(SolveState.attempt(p)))
    }.cache merge problem.map(p => Observable.just(Unattempted(p)))).switch

  val lastSolutionState = solutionState.manifest(Unattempted(Problem.empty))

  //used for drawing
  boardState.subscribe(_ => this.repaint())
  private val currentBoardState = boardState.manifest(Shape.empty)
  solutionState.subscribe(_ => this.repaint())

  override protected def paintComponent(g: Graphics2D): Unit = {

    val trans = Transformation(origin.getValue._1,origin.getValue._2,cellSize.getValue)
    def drawPiece(s: Shape): Unit = {
      import util.Dir._
      def drawSide(l: Location, side: Dir): Unit = {
        val r = trans.locToScreen(l)
        side match {
          case Top => g.drawLine(r.x,r.y,r.x+r.width,r.y)
          case Bottom => g.drawLine(r.x,r.y+r.height,r.x+r.width,r.y+r.height)
          case Left => g.drawLine(r.x,r.y,r.x,r.y+r.height)
          case Right => g.drawLine(r.x+r.width,r.y,r.x+r.width,r.y+r.height)
        }
      }
      for{
        l <- s.locations
        d <- util.Dir.values if !s.locations(l.step(d))
      } drawSide(l,d)
    }
    val clip: Rectangle = Option(g.getClipBounds).getOrElse(this.bounds)
    g.setBackground(Color.LIGHT_GRAY)
    g.clearRect(clip.x,clip.y,clip.width,clip.height)

    //draw goal shape
    for{
      loc <- currentBoardState.getValue.locations
      scrRect = trans.locToScreen(loc) if clip.intersects(scrRect)
    } {
      g.setColor(Color.GRAY)
      g.fillRect(scrRect.x,scrRect.y,scrRect.width,scrRect.height)
      g.setColor(g.getBackground)
      g.drawRect(scrRect.x,scrRect.y,scrRect.width,scrRect.height)
    }

    //draw solution
    g.setStroke(new BasicStroke(3f))
    lastSolutionState.getValue match {
      case Solved(_,solution) =>
        g.setColor(Color.BLACK)
        solution.placement.foreach(drawPiece)
      case Unsolvable(_,c) =>
        g.setColor(Color.RED)
        g.fillOval(10,10,10,10)
        g.drawOval(10,10,10,10)
        drawPiece(c)
      case Solving(_) =>
        g.setColor(Color.YELLOW)
        g.fillOval(10,10,10,10)
        g.drawOval(10,10,10,10)
        g.setColor(Color.BLACK)
        g.drawString("solving",30,20)
      case t: TimeOut =>
        g.setColor(Color.RED)
        g.fillOval(10,10,10,10)
        g.drawOval(10,10,10,10)
        g.setColor(Color.BLACK)
        g.drawString("timeout",30,20)
      case _ =>
    }
  }
}

