package puzzle2d.gui

import java.awt.Dimension
import javax.swing.JFrame

import puzzle2d._
import rx.lang.scala.subjects.BehaviorSubject
import rx.lang.scala.{Subject, Observable}
import spray.json._
import util.gui.{RXButton, MigPanel}

import scala.io.Source
import scala.swing._

object Main {
  object PuzzleJsonProtocol extends DefaultJsonProtocol {
    case class NumberedPiece(piece: Piece, count: Int)
    implicit object JsonPiece extends JsonFormat[Piece] {
      override def read(json: JsValue): Piece = json match {
        case JsString(desc) => new Piece(Shape.parseString(desc, '#'))
        case e => throw new DeserializationException(s"expected string, found $e")
      }
      override def write(p: Piece): JsValue = JsString(p.representative.toString)
    }

    implicit val jsonPieceSet: RootJsonFormat[NumberedPiece] = jsonFormat2(NumberedPiece)

    implicit object PieceSetJs extends RootJsonFormat[PieceSet]{
      override def write(obj: PieceSet): JsValue = JsArray(obj.pieces.toVector.map{case (p,i) => NumberedPiece(p,i).toJson})

      override def read(json: JsValue): PieceSet = json match {
        case JsArray(nps) => PieceSet(nps.map(_.convertTo[NumberedPiece]).map(np => np.piece  -> np.count)(collection.breakOut))
        case e => throw new SerializationException(s"expected array of numbered pieces, found $e")
      }
    }
  }

  val pieceSet = {
    import PuzzleJsonProtocol._
    JsonParser(Source.fromInputStream(ClassLoader.getSystemResourceAsStream("puzzle2d/pieceset-kubix.json")).getLines().mkString("\n")).asJsObject.fields("piece-set").convertTo[PieceSet]
  }

  case class Instance(name: String, problem: Problem)
  object Instance {
    def empty = Instance("empty", Problem.empty)
  }

  // Instances available for loading
  val savedInstances = BehaviorSubject[Set[Instance]](Set[Instance]())
  //trigger for setting instance
  val setInstance: BehaviorSubject[Instance] = BehaviorSubject(Instance("default", Problem(Shape.empty, pieceSet)))

  val actionSolve = ActionObs("Solve")
  val actClearBoard = ActionObs("Clear")

  def main (args: Array[String] ) {
    val solveButton = new Button(actionSolve)
    val clearButton = new Button(actClearBoard)
    val toolbar = new ToolBar {
      peer.setFloatable(false)
    }
    val pieces = new PieceSetView(setInstance.map(_.problem.set))
    toolbar.contents ++= solveButton :: clearButton :: Nil

    val board: Board = new Board(
      pieceSet = pieces.rxValue,
      setShape = actClearBoard.rxVale.map(_ => Shape.empty) merge setInstance.map(_.problem.goal),
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





