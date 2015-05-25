package puzzle2d.json

import java.io.InputStream

import puzzle2d.{Problem, PieceSet, Shape, Piece}
import spray.json._

import scala.io.Source
import scala.util.Try

object PuzzleJsonProtocol extends DefaultJsonProtocol {
  case class NumberedPiece(piece: Piece, count: Int)

  implicit object JsonShape extends JsonFormat[Shape] {
    override def write(obj: Shape): JsValue = JsString(obj.toString)
    override def read(json: JsValue): Shape = json match {
      case JsString(desc) => Shape.parseString(desc, '#')
      case e => throw new DeserializationException(s"expected string, found $e")
    }
  }
  implicit object JsonPiece extends JsonFormat[Piece] {
    override def read(json: JsValue): Piece = Piece(json.convertTo[Shape])
    override def write(p: Piece): JsValue = JsonShape.write(p.representative)
  }

  implicit val jsonNumberedPiece: RootJsonFormat[NumberedPiece] = jsonFormat2(NumberedPiece)
  implicit val jsonPieceSet: RootJsonFormat[PieceSet] = jsonFormat2(PieceSet(_:Map[Piece,Int],_:String))
  implicit val jsonProblem: RootJsonFormat[Problem] = jsonFormat(Problem(_: Shape, _: PieceSet, _: String), "goal", "pieceset", "name")

  def shape2Json(s: Shape): String = s.toJson.prettyPrint
  def pieceSet2Json(ps: PieceSet): String = ps.toJson.prettyPrint
  def problem2Json(p: Problem): String = p.toJson.prettyPrint
  def problemLib2Json(pl: Seq[Problem]): String = pl.toJson.prettyPrint

  def parseShape(is: InputStream): Try[Shape] = parse[Shape](is)
  def parsePieceSet(is: InputStream): Try[PieceSet] = parse[PieceSet](is)
  def parseProblem(is: InputStream): Try[Problem] = parse[Problem](is)
  def parseProblemLibrary(is: InputStream): Try[Seq[Problem]] = parse[Seq[Problem]](is)

  def parse[T: JsonReader](is: InputStream): Try[T] =
    Try(JsonParser(Source.fromInputStream(is).getLines.mkString("\n")).convertTo[T])
}

case class Persisted[T: RootJsonFormat]()