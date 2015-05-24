package util.gui

/**
 * Created by thomas on 24.05.15.
 */
object ActionObs {
  def apply(name: String): ActionObs = new ActionObs(name)
}

class ActionObs(name: String) extends Action(name) {
  val rxVale: Subject[Unit] = Subject()
  override def apply(): Unit = rxVale.onNext(())
}