package puzzle2d.gui

import java.awt.Color

import puzzle2d.{PieceSet, Piece}
import rx.lang.scala.Observable
import util.gui.{RXIntValue, MigPanel}

/** A view and an editor for a [[puzzle2d.PieceSet]]. */
class PieceSetView(val pieceSet: PieceSet) extends MigPanel("") {
  pieceSet.pieces.foreach{p =>
    this.add(new PieceToggler(p._1), "wrap")
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
