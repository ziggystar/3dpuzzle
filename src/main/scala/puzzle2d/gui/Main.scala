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
    Piece(Shape.parseString("##\n ##")) -> 1
  ))

  def main (args: Array[String] ) {
    val pieces = new PieceSetView(set)
    val solveButton = new RXButton("Solve")
    val root = new MigPanel(""){
      add(solveButton,"wrap")
      add(pieces)
      add(new Board(pieceSet = pieces.rxValue, solve = solveButton.rxValue),"push,grow")
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
