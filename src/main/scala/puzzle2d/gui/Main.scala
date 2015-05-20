package puzzle2d.gui

import java.awt.Dimension
import javax.swing.JFrame

import puzzle2d._
import util.gui.MigPanel

import scala.swing._

object Main {
  val set = PieceSet(Map(
    Piece(Shape.parseString("####")) -> 1,
    Piece(Shape.parseString("##\n##")) -> 1,
    Piece(Shape.parseString("##\n ##")) -> 1
  ))

  def main (args: Array[String] ) {
    val pieces = new PieceSetView(set)
    val root = new MigPanel(""){
      add(pieces)
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
