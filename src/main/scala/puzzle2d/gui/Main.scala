package puzzle2d.gui

import java.awt.Dimension
import java.io.File

import javax.swing._
import puzzle2d._
import rx.lang.scala.{Observable, Subject}
import util.gui._
import util.rx.FilePersisted

import scala.concurrent.duration.Duration
import scala.swing._

import puzzle2d.json.PuzzleJsonProtocol._

object Main {

  val pieceSets: Set[PieceSet] = Set(
      "puzzle2d/pieceset-kubix.json",
      "puzzle2d/pieceset-ubongo.json",
      "puzzle2d/pieceset-tetris.json",
      "puzzle2d/pieceset-smart-games.json"
    ).map(f => parsePieceSet(this.getClass.getClassLoader.getResourceAsStream(f)).get)

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

    val clearButton = new Button(actClearBoard)
    val selInstance = new RxChooser[Problem](savedInstances, "Load Shape")(_.name)
    val selPieceSet = new RxChooser[PieceSet](Observable.just(pieceSets),"Load Pieces")(_.name)

    val saveButton = new RXButton("Save Shape")
    val currentPieceSet: Observable[PieceSet] = selInstance.rxValue.map(_.set) merge selPieceSet.rxValue
    val exportShape = new RXButton("Export Image")
    val toolbar = new ToolBar {
      peer.setFloatable(false)
      contents ++= clearButton :: selInstance :: selPieceSet :: exportShape :: saveButton :: Nil
    }

    val pieces = new PieceSetView(Observable.just(pieceSets.head) ++ currentPieceSet)

    val saveNames = saveButton.rxValue.map(_ => JOptionPane.showInputDialog("Problem Name"))

    val solveTrigger = Subject[Unit]()
    actionSolve.rxVale.subscribe(solveTrigger)

    val board: Board = new Board(
      pieceSet = pieces.rxValue,
      setShape = actClearBoard.rxVale.map(_ => Shape.empty) merge selInstance.rxValue.map(_.goal),
      solveTrigger = solveTrigger)

    //auto-solve
    board.problem.distinctUntilChanged.debounce(Duration("1s")).map(_ => ()).subscribe(solveTrigger)

    toolbar.contents += Component.wrap(new JToolBar.Separator())
    toolbar.contents += new RXLabel(board.boardState.map(s => s"Cells: ${s.locations.size}"))

    exportShape.rxValue.foreach{ _ =>
      val fc = new FileChooser()
      val r = fc.showDialog(null, "Choose a file to save shape")
      r match {
        case FileChooser.Result.Approve => board.write2SVG(fc.selectedFile)
      }

    }

    val root: MigPanel = new MigPanel(""){
      add(toolbar, "span 2, growx,wrap")
      private val piecePane: ScrollPane = new ScrollPane(pieces){
        verticalScrollBarPolicy = ScrollPane.BarPolicy.AsNeeded
        horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
      }
      add(piecePane, "pushy,growy")
      add(board,"push,grow")
    }

    val addProblems = saveNames.withLatestFrom(board.problem){case (name,problem) => problem.copy(name = name)}
    addProblems.withLatestFrom(savedInstances.distinctUntilChanged){case (p,ps) => ps + p}.subscribe(savedInstances.onNext _)

    val main = new MainFrame{
      title = "Puzzle 2D"
      minimumSize = new Dimension(640,480)
      contents = root
    }

    main.peer.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
    main.open()

    //add hotkeys
    root.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("F2"), "Clear")
    root.peer.getActionMap.put("Clear", actClearBoard.peer)
  }
}






