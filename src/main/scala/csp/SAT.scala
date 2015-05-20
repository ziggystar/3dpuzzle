package csp

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import scala.collection.immutable.IndexedSeq
import scala.collection.mutable

sealed trait Literal[A]{
  def variable: A
  def sign: Boolean
  def not: Literal[A]
}
case class Negated[A](variable: A) extends Literal[A]{
  def sign: Boolean = false
  def not: Literal[A] = Plain(variable)
}
case class Plain[A](variable: A) extends Literal[A]{
  def sign: Boolean = true
  def not: Literal[A] = Negated(variable)
}

sealed trait Constraint[A]{
  def variables: Set[A]
}

case class OneOf[A](literals: Iterable[Literal[A]]) extends Constraint[A]{
  def variables = literals.map(_.variable)(collection.breakOut)
}
case class AtLeastOne[A](literals: Iterable[Literal[A]]) extends Constraint[A]{
  def variables = literals.map(_.variable)(collection.breakOut)
}


object CSP {
  def solve[A](constraints: Iterable[Constraint[A]], solver: ISolver = SolverFactory.newDefault): (ISolver, Option[Set[A]]) = {
    val allVars: IndexedSeq[A] = constraints.flatMap(_.variables).toSet.toIndexedSeq
    val toVar: mutable.HashMap[A, Int] = allVars.zip(1 to allVars.size).map(identity)(collection.breakOut)

    def literalsToIvec(lits: Iterable[Literal[A]]) = new VecInt(lits.map{
      case Plain(a) => toVar(a)
      case Negated(a) => -toVar(a)
    }(collection.breakOut): Array[Int])

    //add the constraints
    constraints.foreach{
      case OneOf(lits) => solver.addExactly(literalsToIvec(lits.toSet),1)
      case AtLeastOne(lits) => solver.addClause(literalsToIvec(lits.toSet))
    }

    val solution = Some(solver).filter(_.isSatisfiable).map(_.model.filter(_ > 0).map(v => allVars(v-1)).toSet)
    (solver,solution)
  }
}


