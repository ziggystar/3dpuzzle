package util

import _root_.rx.lang.scala.Observable
import _root_.rx.lang.scala.subjects.BehaviorSubject

/** Helpers and pimps for rxscala. */
package object rx {
  implicit class RichBehaviorSubject[T](val bs: BehaviorSubject[T]) extends AnyVal {
    def getValue: T = bs.asJavaSubject.getValue
  }

  implicit class RichObservable[T](val obs: Observable[T]) extends AnyVal {
    def manifest(initial: T) = new {
      @volatile private var last = initial
      obs.subscribe(x => last = x)
      def getValue: T = last
    }
  }
}
