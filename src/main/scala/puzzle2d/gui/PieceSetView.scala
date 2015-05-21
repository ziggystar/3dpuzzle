package puzzle2d.gui

import java.awt.Color

import puzzle2d.{PieceSet, Piece}
import rx.lang.scala.Observable
import util.gui.{RXComponent, RXIntValue, MigPanel}

/** A view and an editor for a [[puzzle2d.PieceSet]]. */
class PieceSetView(val pieceSet: PieceSet) extends MigPanel("") with RXComponent[PieceSet]{
  val pieceList = pieceSet.pieces.keys.toSeq
  val togglers = pieceList.map(p => new PieceToggler(p, initially = pieceSet.pieces(p)))
  togglers.foreach(this.add(_,"wrap"))

  override def rxValue: Observable[PieceSet] = togglers.map(_.toggler.rxValue)
      .foldLeft(Observable.just(Nil): Observable[List[Int]]){ case (oli, oi) =>
        oli.combineLatestWith(oi)((l,i) => i :: l)}.map(li => PieceSet(pieceList.zip(li.reverse).toMap))
}

class PieceToggler(val piece: Piece, val color: Color = Color.BLACK, initially: Int = 1) extends MigPanel("") {
  this.peer.setAutoscrolls(true)

  val toggler = new RXIntValue(initially,minValue = 0)

  val preview = new ShapeView(Observable just piece.representative,
    toggler.rxValue.map{
      case x if x > 0 => color
      case _ => Color.GRAY
    }
  )
  this.add(preview)
  this.add(toggler)
}
