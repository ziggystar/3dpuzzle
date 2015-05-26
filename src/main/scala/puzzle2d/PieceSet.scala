package puzzle2d

/** A multi-set of pieces. */
case class PieceSet(pieces: Map[Piece,Int], name: String = "anon") {
  def toSeq = pieces.flatMap{case (s,count) => Seq.fill(count)(s)}

  override def toString: String = "PieceSet{\n\t" + pieces.mkString("\n\t") + "}"
}

object PieceSet {
  def empty: PieceSet = PieceSet(Map(), "empty")
}
