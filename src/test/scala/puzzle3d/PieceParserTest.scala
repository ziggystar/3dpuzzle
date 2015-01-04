package puzzle3d

import org.specs2._
import org.specs2.specification.Fragments

/**
 * Created by thomas on 12/26/13.
 */
class PieceParserTest extends Specification with matcher.ParserMatchers {
  import PieceParser._
  val parsers = PieceParser

  def is: Fragments =
  "parse solid piece descriptions" ^
    (solidPiece must succeedOn("solid(111)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0))))) ^
      (solidPiece must succeedOn("solid(1,2)").withResult(Piece(Set((0,0,0),(0,1,0),(0,1,1))))) ^
  p^
  "parse voxel piece descriptions" ^
    (voxelPiece must succeedOn("voxel(###)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0))))) ^
    (voxelPiece must succeedOn("voxel(###+# #)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0),(0,0,1),(2,0,1)))))

}
