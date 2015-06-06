package util

import java.util.concurrent.atomic.AtomicReference

import _root_.rx.lang.scala.Observable
import _root_.rx.lang.scala.subjects.BehaviorSubject

/** Helpers and pimps for rxscala. */
package object rx {
  implicit class RichBehaviorSubject[T](val bs: BehaviorSubject[T]) extends AnyVal {
    def getValue: T = bs.asJavaSubject.getValue
  }

  implicit class RichObservable[T](val obs: Observable[T]) extends AnyVal {
    def manifest(initial: T) = new {
      private val last = new AtomicReference(initial)
      obs.subscribe(x => last.set(x))
      def getValue: T = last.get()
    }
    def print(msg: String = ""): Unit = obs.subscribe(x => println(s"$msg$x"))
  }
}
