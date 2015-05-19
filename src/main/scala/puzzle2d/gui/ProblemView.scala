package puzzle2d.gui

import java.awt.{Color, Dimension}
import javax.swing.{UIManager, JFrame}

import puzzle2d._
import rx.lang.scala.{Observable, Subject}
import util.gui.{RXIntValue, RXButton, MigPanel, RXCheckBox}

import scala.swing._

/** Viewer for [[Shape]]. */
class ShapeView(val shape: Observable[Shape], val color: Observable[Color]) extends Component {
  //the current shape is always normalized
  private var currentState: (Shape,Color) = (Shape.empty,Color.BLACK)
  val cellWidth = 15
  val maxCells = 4
  val sideLength = maxCells * cellWidth

  minimumSize = new Dimension(sideLength,sideLength)
  shape.map(_.normalize).combineLatest(color).subscribe { nextState =>
    currentState = nextState
    val shape = currentState._1
    this.repaint()
  }
  override def paint(g: Graphics2D): Unit = {
    val currentShape: Shape = currentState._1
    g.translate(
      (maxCells - currentShape.maxX - 1).toDouble/2*cellWidth,
      (maxCells - currentShape.maxY - 1).toDouble/2*cellWidth)
    g.setColor(currentState._2)
    currentShape.locations.foreach{ case Location(x,y) =>
      g.fillRect(x*cellWidth,y*cellWidth,cellWidth,cellWidth)
    }
    g.setColor(this.background)
    currentShape.locations.foreach{ case Location(x,y) =>
        g.drawRect(x*cellWidth,y*cellWidth,cellWidth,cellWidth)
    }
  }
}

class PieceToggler(val piece: Piece, val color: Color = Color.BLACK, initiallyEnabled: Boolean = true) extends MigPanel("") {
  this.peer.setAutoscrolls(true)

  val toggler = new RXIntValue(1,minValue = 0)

  val preview = new ShapeView(Observable just piece.representative,
      toggler.rxValue.map{
        case x if x > 0 => color
        case _ => Color.GRAY
      }
  )
  this.add(preview)
  this.add(toggler)
}



class PieceSetView(val pieceSet: PieceSet) extends MigPanel("") {
  pieceSet.pieces.foreach{p =>
    this.add(new PieceToggler(p._1), "wrap")
  }
}

object Test{
  val set = PieceSet(Map(
    Piece(Shape.parseString("####")) -> 1,
    Piece(Shape.parseString("##\n##")) -> 1,
    Piece(Shape.parseString("##\n ##")) -> 1
  ))

  def main (args: Array[String] ) {
    val pieces = new PieceSetView(set)
    val root = new MigPanel(""){
      add(pieces)
    }
    val main = new MainFrame{
      title = "Puzzle 2D"
      minimumSize = new Dimension(640,480)
      contents = root
    }
    main.peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    main.open()
  }
}
