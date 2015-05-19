package util.gui

import rx.lang.scala.{Observable, Subject}

import scala.swing.event.ButtonClicked
import scala.swing.{Label, Button, CheckBox}

/**
 * Created by thomas on 19.05.15.
 */
class RXCheckBox(msg: String = "") extends CheckBox(msg)  with RXComponent[Boolean]{
  private val enabledObsRaw = Subject[Boolean]()
  val rxValue = enabledObsRaw.distinctUntilChanged
  selected = true
  this.listenTo(this)
  reactions += {
    case e => enabledObsRaw.onNext(this.selected)
  }
}

class RXIntValue(init: Int, minValue: Int = Integer.MIN_VALUE, maxValue: Int = Integer.MAX_VALUE) extends MigPanel("")  with RXComponent[Int]{
  private val decreaseButton = new RXButton("-")
  private val increaseButton = new RXButton("+")
  val rxValue: Observable[Int] = (Observable.just(init) ++ 
    (decreaseButton.rxValue.map(_ => -1) merge increaseButton.rxValue.map(_ => +1)).scan(init){
      case (acc,n) => Some(acc + n).filter(x => x >= minValue && x <= maxValue).getOrElse(acc)}).distinctUntilChanged

  private val label = new RXLabel(rxValue.map(_.toString))
  this.add(decreaseButton)
  this.add(label)
  this.add(increaseButton)
}

class RXLabel(value: Observable[String], initialValue: String = " ") extends Label(initialValue) {
  value.subscribe{newVal =>
    this.text = newVal
  }
}

class RXButton(label: String) extends Button(label) with RXComponent[Unit]{
  private val pressSubject = Subject[Unit]()
  def rxValue: Observable[Unit] = pressSubject
  this.listenTo(this)
  this.reactions += {
    case ButtonClicked(src) if src == this => pressSubject.onNext(())
  }
}

trait RXComponent[T]{
  def rxValue: Observable[T]
}
