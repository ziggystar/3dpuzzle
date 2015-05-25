package puzzle2d

/** A multi-set of pieces. */
case class PieceSet(pieces: Map[Piece,Int], name: String = "anon") {
  def toSeq = pieces.flatMap{case (s,count) => Seq.fill(count)(s)}
}

object PieceSet {
  def empty: PieceSet = PieceSet(Map(), "empty")
}
