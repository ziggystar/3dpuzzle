package util.rx

import java.io.{File, FileOutputStream}

import rx.lang.scala.Subject
import rx.lang.scala.subjects.BehaviorSubject

import scala.io.Source
import scala.util.Try

/** Persistence of one single value of type `T`. */
trait Persisted[T] { outer =>
  def read: Try[T]
  def write(t: T): Try[Unit]

  def map[S](serialize: S => T)(deserialize: T => Try[S]): Persisted[S] = new Persisted[S] {
    override def write(t: S): Try[Unit] = outer.write(serialize(t))
    override def read: Try[S] = outer.read.flatMap(deserialize)
  }

  def toSubject(initial: T): Subject[T] = {
    val persistedValue = read
    val first: T = read.getOrElse{
      write(initial)
      initial
    }
    val r = BehaviorSubject(first)
    r.subscribe(write(_))
    r
  }
}

case class FilePersisted(file: File) extends Persisted[String] {
  override def read: Try[String] = Try(Source.fromFile(file).getLines().mkString("\n"))
  override def write(t: String): Try[Unit] = Try{
    val out = new FileOutputStream(file)
    out.write(t.getBytes)
    out.close()
  }
}

object FilePersisted {
  import spray.json._

  def asJson[T: RootJsonFormat](file: File, default: T): Subject[T] =
    FilePersisted(file).map((_:T).toJson.prettyPrint)(s => Try(JsonParser(s).convertTo[T])).toSubject(default)
}
