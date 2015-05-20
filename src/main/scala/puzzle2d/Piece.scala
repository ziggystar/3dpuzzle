package puzzle2d

/** An equivalence class for shapes. Allows rotation, translation and flipping. */
class Piece(prototype: Shape){
  /** The prototype rotated, flipped and normalized. */
  val shapes: Set[Shape] = for{
    flipped <- Set(prototype,prototype.flip)
    rotated <- (0 to 3).map(flipped.rotateOrigin(_))
  } yield rotated.normalize

  val representative: Shape = shapes.head

  override val hashCode: Int = shapes.hashCode()

  override def equals(obj: scala.Any): Boolean = obj match {
    case other: Piece => other.shapes == shapes
    case _ => false
  }
}

object Piece{
  def apply(s: Shape): Piece = new Piece(s)
}
