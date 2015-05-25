package util.gui

import rx.lang.scala.Subject

import scala.swing.Action


object ActionObs {
  def apply(name: String): ActionObs = new ActionObs(name)
}

class ActionObs(name: String) extends Action(name) {
  val rxVale: Subject[Unit] = Subject()
  override def apply(): Unit = rxVale.onNext(())
}