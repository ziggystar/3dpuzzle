package puzzle2d

import org.specs2.Specification
import org.specs2.specification.Fragments

class ShapeTest extends Specification {
  val shapeI: Shape = Shape.parseString("####")
  override def is: Fragments =
  "parsing tests" ^
    "parse single block" ! (Shape.parseString("#") === Shape(Set(Location(0,0)))) ^
    "parse single block normalized" ! (Shape.parseString(" #") === Shape(Set(Location(0,0)))) ^
    "parse single block un-normalized" ! (Shape.parseString(" #",normalize = false) === Shape(Set(Location(1,0)))) ^
    "parse single block un-normalized/2" ! (Shape.parseString("#\n ",normalize = false) === Shape(Set(Location(0,1)))) ^
    "parse line" ! (Shape.parseString("##") === Shape(Set(Location(0,0),Location(1,0)))) ^
    "parse standing line" ! (Shape.parseString("#\n#") === Shape(Set(Location(0,0),Location(0,1)))) ^
    "rotate I" ! (shapeI.rotateOrigin(1) === Shape(Set(Location(0,0),Location(0,-1),Location(0,-2),Location(0,-3))))
}