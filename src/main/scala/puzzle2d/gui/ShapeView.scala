package puzzle2d.gui

import java.awt.Color

import puzzle2d.{Location, Shape}
import rx.lang.scala.Observable

import scala.swing.{Graphics2D, Dimension, Component}

/** Viewer for [[puzzle2d.Shape]]. Not used for editing shapes. */
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
