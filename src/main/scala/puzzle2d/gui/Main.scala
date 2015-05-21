package puzzle2d.gui

import java.awt.Dimension
import javax.swing.JFrame

import puzzle2d._
import spray.json._
import util.gui.{RXButton, MigPanel}

import scala.collection.immutable.Map
import scala.io.Source
import scala.swing._

object Main {
  object PuzzleJsonProtocol extends DefaultJsonProtocol {

    implicit object JsonPiece extends RootJsonFormat[Piece] {
      override def read(json: JsValue): Piece = json match {
        case JsObject(fields) if fields.contains("piece") =>
          fields("piece") match {
            case JsString(desc) => new Piece(Shape.parseString(desc, '#'))
          }
      }

      override def write(p: Piece): JsValue = JsObject("piece" -> JsString(p.representative.toString))
    }

  }
  import PuzzleJsonProtocol._

  def fromJson(js: JsValue): PieceSet = js match {
    case JsObject(fields) => PieceSet(fields("piece-set").convertTo[Seq[Piece]].zip(Stream.continually(10)).toMap)
  }
  val pieceSet = {
    fromJson(JsonParser(Source.fromInputStream(ClassLoader.getSystemResourceAsStream("puzzle2d/pieceset-kubix.json")).getLines().mkString("\n")))
  }

  def main (args: Array[String] ) {
    val solveButton = new RXButton("Solve")
    val clearButton = new RXButton("Clear")
    val toolbar = new ToolBar {
      peer.setFloatable(false)
    }
    val pieces = new PieceSetView(pieceSet)
    toolbar.contents ++= solveButton :: clearButton :: Nil

    val board: Board = new Board(
      pieceSet = pieces.rxValue,
      setShape = clearButton.rxValue.map(_ => Shape.empty),
      solve = solveButton.rxValue)
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
