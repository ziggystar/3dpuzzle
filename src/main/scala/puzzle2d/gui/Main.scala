package puzzle2d.gui

import java.awt.Dimension
import javax.swing.JFrame

import puzzle2d._
import util.gui.{RXButton, MigPanel}

import scala.swing._

object Main {
  val set = PieceSet(Map(
    Piece(Shape.parseString("####")) -> 1,
    Piece(Shape.parseString("##\n##")) -> 1,
    Piece(Shape.parseString("#\n###")) -> 1,
    Piece(Shape.parseString("##\n ##")) -> 1,
    Piece(Shape.parseString("###\n #")) -> 1
  ))

  def main (args: Array[String] ) {
    val solveButton = new RXButton("Solve")
    val clearButton = new RXButton("Clear")
    val toolbar = new ToolBar {
      peer.setFloatable(false)
    }
    val pieces = new PieceSetView(set)
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
