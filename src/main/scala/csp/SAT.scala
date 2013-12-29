package csp

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt

trait CNF {
  def clauses: Set[Set[Literal]]
}

object CNF{
  def toProblem(clauses: Set[Set[Literal]], solver: ISolver = SolverFactory.newDefault): (ISolver, Map[Int, BVar]) = {
    val varToIndex: Map[BVar, Int] =
      clauses.flatten.map(_.variable).toSeq.distinct.zipWithIndex.toMap.mapValues(_ + 1)

    def clauseToIvec(clause: Set[Literal]): IVecInt = new VecInt(clause.map{
      case Negation(v) => -varToIndex(v)
      case Plain(v) => varToIndex(v)
    }(collection.breakOut):Array[Int])

    solver.setExpectedNumberOfClauses(clauses.size)

    clauses.map(clauseToIvec).foreach(solver.addClause)

    (solver,varToIndex.map(_.swap))
  }
}

case class OneOf(literals: Iterable[Literal]) extends CNF{
  def oneOfNoNewVars(_ls: Iterable[Literal]): Set[Set[Literal]] = {
    val ls = _ls.toSet
    val notTwo = for{
      l <- ls
      other <- ls if other != l
    } yield Set(l.not,other.not)
    notTwo + ls
  }
  def clauses: Set[Set[Literal]] = oneOfNoNewVars(literals)
}

