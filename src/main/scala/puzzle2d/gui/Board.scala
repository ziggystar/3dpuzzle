package puzzle2d.gui

import java.awt._

import puzzle2d.Shape
import puzzle2d._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subject, Observable}
import util.rx._

import scala.swing.event._
import scala.swing.{Dimension, Graphics2D, Component}
import rx.lang.scala.ExperimentalAPIs._

/** A [[scala.swing.Component]] that allows viewing and editing a [[puzzle2d.Shape]].
  * It also allows overlaying a solution.
  */
class Board(val setShape: Observable[Shape] = Observable.empty,
            val solveTrigger: Observable[Unit] = Observable.empty,
            val pieceSet: Observable[PieceSet]) extends Component{
  minimumSize = new Dimension(200,200)

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

  val problem: Observable[Problem] = boardState.combineLatestWith(pieceSet)(Problem(_,_))

  val currentSolution: Observable[Option[Problem#Solution]] = {
    (problem.map(Right(_)) merge solveTrigger.map(Left(_))).scan((None: Option[Problem],None: Option[Problem#Solution])){
      case ((None,_),Right(p)) => (Some(p),None)          //first problem arrives
      case ((None,_),Left(())) => (None,None)             //uninitialized solve request
      case ((Some(p),_),Right(pn)) => (Some(pn),None)     //update problem, discards solution
      case (old@(Some(_),Some(_)),Left(())) => old        //redundant solve request
      case ((Some(p),None),Left(())) => (Some(p),p.solve) //solve
    }.map(_._2)
  }
  //the below code behaves oddly, bug in rx-java?
  //problem.sample(solveTrigger).map(_.solve) merge problem.map(_ => None)

  //used for drawing
  boardState.subscribe(_ => this.repaint())
  private val currentBoardState = boardState.manifest(Shape.empty)
  currentSolution.subscribe(_ => this.repaint())
  private val solutionState = currentSolution.manifest(None)

  override def paint(g: Graphics2D): Unit = {
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
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(3f))
    solutionState.getValue.foreach{s => s.placement.foreach(drawPiece)}
  }
}

