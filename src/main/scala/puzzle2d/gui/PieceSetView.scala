package puzzle2d.gui

import java.awt.Color

import puzzle2d.{Piece, PieceSet}
import rx.lang.scala.Observable
import util.gui.{MigPanel, RXComponent, RXIntValue}
import util.rx._

/** A view and an editor for a [[puzzle2d.PieceSet]]. */
class PieceSetView(pieceSet: Observable[PieceSet]) extends MigPanel("") with RXComponent[PieceSet]{
  val togglers: Observable[Seq[PieceToggler]] = pieceSet.distinctUntilChanged.map{ps =>
    ps.pieces.map{
    case (p,n) =>
      new PieceToggler(p, initially = n)
  }(collection.breakOut)}.cache

  //we simply add new components when the set changes...
  togglers.foreach{ts =>
    this.contents.clear()
    ts.foreach(this.add(_,"wrap"))
    this.peer.revalidate()
  }

  val rxValue: Observable[PieceSet] =
  togglers.flatMap(ts => Observable.combineLatest(ts.map(t => t.toggler.rxValue.map(t.piece -> _)))(tups => PieceSet(tups.toMap)))
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
