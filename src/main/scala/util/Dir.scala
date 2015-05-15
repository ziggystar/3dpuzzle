package util

/** Direction enum. */
object Dir extends Enumeration {
  val Top,Bottom,Left,Right = Value
  type Dir = Dir.Value
}