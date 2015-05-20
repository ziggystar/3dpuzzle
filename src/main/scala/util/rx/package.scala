package util

import _root_.rx.lang.scala.Observable
import _root_.rx.lang.scala.subjects.BehaviorSubject

/** Helpers and pimps for rxscala. */
package object rx {
  implicit class RichBehaviorSubject[T](val bs: BehaviorSubject[T]) extends AnyVal {
    def getValue: T = bs.asJavaSubject.getValue
  }
}
