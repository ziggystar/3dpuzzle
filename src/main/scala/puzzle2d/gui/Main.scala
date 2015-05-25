package puzzle2d.gui

import puzzle2d._

import java.awt.Dimension
import javax.swing.JFrame

import rx.lang.scala.subjects.BehaviorSubject
import util.gui.{ActionObs, MigPanel}

import scala.swing._

object Main {
  import puzzle2d.json.PuzzleJsonProtocol._
  val pieceSet: PieceSet = parsePieceSet(ClassLoader.getSystemResourceAsStream("puzzle2d/pieceset-kubix.json")).get

  // Instances available for loading
  val savedInstances = BehaviorSubject[Set[Problem]](Set[Problem]())
  //trigger for setting instance
  val setInstance: BehaviorSubject[Problem] = BehaviorSubject(Problem(Shape.empty, pieceSet, name = "default"))

  val actionSolve = ActionObs("Solve")
  val actClearBoard = ActionObs("Clear")

  def main (args: Array[String] ) {
    val solveButton = new Button(actionSolve)
    val clearButton = new Button(actClearBoard)
    val toolbar = new ToolBar {
      peer.setFloatable(false)
    }
    val pieces = new PieceSetView(setInstance.map(_.set))
    toolbar.contents ++= solveButton :: clearButton :: Nil

    val board: Board = new Board(
      pieceSet = pieces.rxValue,
      setShape = actClearBoard.rxVale.map(_ => Shape.empty) merge setInstance.map(_.goal),
      solveTrigger = actionSolve.rxVale)
    val root = new MigPanel(""){
      add(toolbar, "span 2, growx,wrap")
      add(pieces)
      add(board,"push,grow")
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





