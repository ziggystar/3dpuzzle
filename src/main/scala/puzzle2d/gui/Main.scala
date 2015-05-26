package puzzle2d.gui

import java.io.File

import puzzle2d._
import util.rx._

import java.awt.Dimension
import javax.swing.{JComponent, KeyStroke, JOptionPane, JFrame}

import rx.lang.scala.{Subject, Observable}
import rx.lang.scala.ExperimentalAPIs._
import util.gui.{RXButton, RxChooser, ActionObs, MigPanel}
import util.rx.FilePersisted

import scala.swing._

object Main {
  import puzzle2d.json.PuzzleJsonProtocol._
  val pieceSets: Set[PieceSet] =
    Set("puzzle2d/pieceset-kubix.json", "puzzle2d/pieceset-ubongo.json", "puzzle2d/pieceset-tetris.json").map(f => parsePieceSet(ClassLoader.getSystemResourceAsStream(f)).get)

  val instanceFile: File = {
    val f = new File(System.getProperty("user.home") + "/.2dpuzzle")
    if(!f.exists())
      f.mkdir()
    new File(System.getProperty("user.home") + "/.2dpuzzle/instances")
  }

  val emptyInstance = Problem(Shape.empty, pieceSets.head, name = "default")

  val actionSolve = ActionObs("Solve")
  val actClearBoard = ActionObs("Clear")

  def main (args: Array[String] ) {
    val savedInstances: Subject[Set[Problem]] = FilePersisted.asJson(instanceFile, Set())

    val solveButton = new Button(actionSolve)
    val clearButton = new Button(actClearBoard)
    val saveButton = new RXButton("Speichern")
    val selInstance = new RxChooser[Problem](savedInstances, "Problem Laden")(_.name)
    val selPieceSet = new RxChooser[PieceSet](Observable.just(pieceSets),"Teile Laden")(_.name)
    val currentPieceSet: Observable[PieceSet] = selInstance.rxValue.map(_.set) merge selPieceSet.rxValue
    val toolbar = new ToolBar {
      peer.setFloatable(false)
      contents ++= solveButton :: clearButton :: saveButton :: selInstance :: selPieceSet :: Nil
    }

    val pieces = new PieceSetView(Observable.just(pieceSets.head) ++ currentPieceSet)

    val saveNames = saveButton.rxValue.map(_ => JOptionPane.showInputDialog("Problem Name"))

    val board: Board = new Board(
      pieceSet = pieces.rxValue,
      setShape = actClearBoard.rxVale.map(_ => Shape.empty) merge selInstance.rxValue.map(_.goal),
      solveTrigger = actionSolve.rxVale)



    val root = new MigPanel(""){
      add(toolbar, "span 2, growx,wrap")
      add(pieces)
      add(board,"push,grow")
    }

    val addProblems = saveNames.withLatestFrom(board.problem){case (name,problem) => problem.copy(name = name)}
    addProblems.withLatestFrom(savedInstances.distinctUntilChanged){case (p,ps) => ps + p}.subscribe(savedInstances.onNext(_))

    val main = new MainFrame{
      title = "Puzzle 2D"
      minimumSize = new Dimension(640,480)
      contents = root
    }

    main.peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    main.open()

    //add hotkeys
    root.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("F1"), "Solve")
    root.peer.getActionMap.put("Solve", actionSolve.peer)
    solveButton.tooltip = "F1"

    root.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("F2"), "Clear")
    root.peer.getActionMap.put("Clear", actClearBoard.peer)
    solveButton.tooltip = "F2"
  }
}






