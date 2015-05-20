package puzzle2d.gui

import java.awt._

import puzzle2d.Shape
import puzzle2d._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subject, Observable}
import util.rx._

import scala.swing.event.MouseClicked
import scala.swing.{Dimension, Graphics2D, Component}
import rx.lang.scala.ExperimentalAPIs._

/** A [[scala.swing.Component]] that allows viewing and editing a [[puzzle2d.Shape]].
  * It also allows overlaying a solution.
  */
class Board(val setShape: Observable[Shape] = Observable.empty, val solve: Observable[Unit] = Observable.empty, pieceSet: Observable[PieceSet]) extends Component{
  minimumSize = new Dimension(200,200)

  case class Transformation(trX: Int, trY: Int, width: Int){
    def locToScreen(l: Location): Rectangle = new Rectangle(l.x * width + trX, l.y * width + trY, width, width)
    def screenToLoc(scrX: Int, scrY: Int) = Location((scrX - trX) / width, (scrY - trY) / width )
    def screenToLoc(p: Point) = Location((p.x - trX) / width, (p.y - trY) / width )
  }

  private val origin: BehaviorSubject[(Int, Int)] = BehaviorSubject[(Int,Int)]((0,0))
  private val cellSize: BehaviorSubject[Int] = BehaviorSubject[Int](20)
  private val transformation: Observable[Transformation] =
    origin.combineLatestWith(cellSize){case ((tx,ty),w) => Transformation(tx,ty,w)}

  private val screenClicks: Subject[Point] = Subject[Point]()
  this.listenTo(this.mouse.clicks)
  reactions += {
    case MouseClicked(src,point,_,1,false) if src == this => screenClicks.onNext(point)
  }

  private val clickLocations = {
    screenClicks.withLatestFrom(transformation)((scr,tr) => tr.screenToLoc(scr.x,scr.y))
  }

  private val boardState: Observable[Shape] =
    (Observable.just(Shape.empty) ++ setShape).map(base => clickLocations.scan(base)(_.flip(_))).switch

  boardState.subscribe(_ => repaint())

  private val currentBoardState: BehaviorSubject[Shape] = {
    val b = BehaviorSubject[Shape](Shape.empty)
    boardState.subscribe(b)
    b
  }

  private val currentSolution: BehaviorSubject[Option[Solution]] = {
    val r = BehaviorSubject[Option[Solution]](None)
    (Observable.just[Option[Solution]](None) ++ solve.withLatestFrom(boardState.filterNot(_.locations.isEmpty) combineLatest pieceSet){
      case (_,(goal,pieces)) =>
        Solver2D.newSolver(Problem(goal,pieces))
    }).subscribe(r)
    r
  }

  currentSolution.subscribe(_ => this.repaint())

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
    g.setColor(Color.GRAY)
    for{
      loc <- currentBoardState.getValue.locations
      scrRect = trans.locToScreen(loc) if clip.intersects(scrRect)
    } {
      g.fillRect(scrRect.x,scrRect.y,scrRect.width,scrRect.height)
    }
    //draw solution
    g.setColor(Color.BLACK)
    g.setStroke(new BasicStroke(2f))
    currentSolution.getValue.foreach{s => s.placement.foreach(drawPiece)}
  }
}

