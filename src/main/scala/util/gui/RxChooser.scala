package util.gui

import rx.lang.scala.{Subject, Observable}

import scala.swing.{Action, MenuItem, PopupMenu, Button}
import scala.swing.event.ButtonClicked

/** A GUI component that allows choice from a set of values. */
class RxChooser[T](choices: Observable[Set[T]], label: String)(view: T => String = (_: String).toString)
  extends Button(label) with RXComponent[T] {

  private val subj = Subject[T]

  val menu = new PopupMenu

  choices.subscribe{ts =>
    menu.contents.clear()
    ts.foreach(t => menu.contents += new MenuItem(Action(view(t))(subj.onNext(t))))
  }

  this.listenTo(this)
  this.reactions += {
    case ButtonClicked(_) => menu.show(this,0,0)
  }
  override def rxValue: Observable[T] = subj
}
