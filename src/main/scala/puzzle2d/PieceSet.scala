package puzzle2d

/** A multi-set of pieces. */
case class PieceSet(pieces: Map[Piece,Int]) {
  def toSeq = pieces.flatMap{case (s,count) => Seq.fill(count)(s)}
}
