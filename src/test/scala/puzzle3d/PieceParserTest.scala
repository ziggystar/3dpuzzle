package puzzle3d

import org.specs2._
import org.specs2.matcher.ParserMatchers


class PieceParserTest extends Specification with ParserMatchers {
  import PieceParser._
  val parsers: PieceParser.type = PieceParser

  def is =
  "parse solid piece descriptions" ^
    "solid1" ! (solidPiece must succeedOn("solid(111)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0))))) ^
     "solid2" ! (solidPiece must succeedOn("solid(1,2)").withResult(Piece(Set((0,0,0),(0,1,0),(0,1,1))))) ^
  p^
  "parse voxel piece descriptions" ^
    "voxel1" ! (voxelPiece must succeedOn("voxel(###)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0))))) ^
    "voxel2" ! (voxelPiece must succeedOn("voxel(###+# #)").withResult(Piece(Set((0,0,0),(1,0,0),(2,0,0),(0,0,1),(2,0,1)))))

}
