package puzzle2d.gui

import java.awt._
import java.io.{PrintStream, FileOutputStream, File}

import org.jfree.graphics2d.svg.SVGGraphics2D
import org.sat4j.specs.ISolver
import org.sat4j.tools.xplain.Xplain
import puzzle2d.Shape
import puzzle2d._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subject, Observable}
import util.rx._

import scala.collection.JavaConverters._
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
      case s@p.Solution(_,solver) => Solved(p, s, solver)
      case p.Timeout(s) => TimeOut(p)
      case p.Unsolvable(s) => Unsolvable(p, s)
    }
  }

  case class Unattempted(problem: Problem) extends SolveState
  case class Solved(problem: Problem, solution: Problem#Solution, solver: ISolver) extends SolveState {
    def usedPieces: Map[Piece,Int] = {
      val pieces = problem.set.pieces.keySet
      val solAsPieces = solution.placement.map(shape => pieces.find(_.represents(shape)).get)
      solAsPieces.groupBy(identity).mapValues(_.size)
    }
  }
  case class Unsolvable(problem: Problem, solver: ISolver) extends SolveState
  case class TimeOut(problem: Problem) extends SolveState
  case class Solving(problem: Problem) extends SolveState

  /** The translation describes how to translate the drawn shape on the canvas. */
  case class Transformation(trX: Int, trY: Int, width: Int){
    def locToScreen(l: Location): Rectangle = new Rectangle(l.x * width + trX, l.y * width + trY, width, width)
    def screenToLoc(scrX: Int, scrY: Int): Location = {
      def translate(t: Int, value: Int): Int = {
        val beforeScale = value - t
        if(beforeScale >= 0) beforeScale / width
        else beforeScale / width - 1
      }
      Location(translate(trX,scrX), translate(trY,scrY))
    }
    def screenPointToLoc(p: Point): Location = screenToLoc(p.x,p.y)
  }

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

  val drags: Observable[Observable[MouseEvent]] = mousePressed.map(mp => Observable.just(mp) ++ mouseDragged.takeUntil(mouseReleased))
  val leftDrags: Observable[Observable[MouseEvent]] = drags.filter(drag => drag.toBlocking.head.peer.getButton == java.awt.event.MouseEvent.BUTTON1)
  val rightDrags: Observable[Observable[MouseEvent]] = drags.filter(drag => drag.toBlocking.head.peer.getButton == java.awt.event.MouseEvent.BUTTON3)


  private val cellSize: Observable[Int] = BehaviorSubject[Int](30)
  private val origin: Observable[(Int, Int)] = Observable.just((0,0)) ++ setShape.withLatestFrom(cellSize){
    case (shape, cs) => (this.bounds.width/2 - (shape.minX + shape.width/2)*cs, this.bounds.height/2 - (shape.minY + shape.height/2)*cs)
  }

  val transformation: Observable[Transformation] =
    origin.combineLatestWith(cellSize){case ((tx,ty),w) => Transformation(tx,ty,w)}
  private val currentTransformation = transformation.manifest(Transformation(0,0,30))
  transformation.foreach(_ => this.repaint())

  def applyDragsToShape(s: Shape, ds: Observable[Observable[Location]]): Observable[Shape] = {
    val annotated = ds.flatMap(drag => drag zip Observable.from(Stream(true) ++ Stream.continually(false)))
    annotated.scan((s,false)){
      case ((os,mode),(l,true)) if os.locations(l)  => (os - l, false)
      case ((os,mode),(l,true)) if !os.locations(l) => (os + l, true)
      case ((os,false),(l,false)) => (os - l, false)
      case ((os,true),(l,false)) => (os + l, true)
    }.map(_._1)
  }

  val mappedDrags: Observable[Observable[Location]] = leftDrags.map { drag =>
    val tr = currentTransformation.getValue
    drag.map(me => tr.screenPointToLoc(me.point)).distinct
  }

  val boardState: Observable[Shape] =
    (Observable.just(Shape.empty) ++ setShape)
      .switchMap(base => { Observable.just(base) ++ applyDragsToShape(base, mappedDrags)})

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

  def write2SVG(f: File): Unit = {
    val svg2 = new SVGGraphics2D(this.bounds.width, this.bounds.height)
    paintComponent(svg2)
    val fout = new PrintStream(new FileOutputStream(f))
    fout.println(svg2.getSVGDocument)
    fout.close()
  }

  override protected def paintComponent(g: Graphics2D): Unit = {

    g.addRenderingHints(Map(RenderingHints.KEY_TEXT_ANTIALIASING -> RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_VRGB).asJava)

    val trans = currentTransformation.getValue

    val solveState = lastSolutionState.getValue

    def drawPiece(s: Shape, transform: Transformation = trans): Unit = {
      import util.Dir._
      def drawSide(l: Location, side: Dir): Unit = {
        val r = transform.locToScreen(l)
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
    g.setBackground(solveState match {
      case _: Unsolvable => Color.decode("#EDD8C0")
      case _ => Color.WHITE
    })
    g.clearRect(clip.x,clip.y,clip.width,clip.height)

    //draw goal shape
    for{
      loc <- currentBoardState.getValue.locations
      scrRect = trans.locToScreen(loc) if clip.intersects(scrRect)
    } {
      g.setColor(Color.decode("#7D9AAA"))
      g.fillRect(scrRect.x,scrRect.y,scrRect.width,scrRect.height)
      g.setColor(g.getBackground)
      g.drawRect(scrRect.x,scrRect.y,scrRect.width,scrRect.height)
    }

    //draw solution
    g.setStroke(new BasicStroke(3f))

    (solveState match {
      case s@Solved(_, solution, solver) => Some(solver)
      case Unsolvable(_,solver) => Some(solver)
      case _ => None
    }) foreach { solver =>
      g.setColor(Color.decode("#7D9AAA").darker.darker)
      g.drawString(s"${solver.realNumberOfVariables} variables, ${solver.nConstraints()} clauses", 10, 15)

    }
    solveState match {
      case s@Solved(_,solution,solver) =>
        solution.placement.foreach(drawPiece(_))
        val legendTransform = Transformation(0,0,20)
        s.usedPieces.zipWithIndex.foreach{case ((piece,num),index) =>
          g.setColor(Color.DARK_GRAY)
          val tr = g.getTransform
            g.translate(0,30 + index * 4 * legendTransform.width)
            g.drawString(s"$num:",0,15)
            g.translate(20,0)
            drawPiece(piece.representative,legendTransform)
            g.setTransform(tr)
        }
      case Unsolvable(_,solver) =>
        g.setColor(Color.BLACK)
        g.drawString("UNSOLVABLE",10,30)
      case Solving(_) =>
        g.setColor(Color.BLACK)
        g.drawString("SOLVING",10,30)
      case t: TimeOut =>
        g.setColor(Color.BLACK)
        g.drawString("TIMEOUT",10,30)
      case _ =>
    }
  }
}

