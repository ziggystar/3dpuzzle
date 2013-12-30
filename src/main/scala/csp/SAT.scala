package csp

import org.sat4j.specs._
import org.sat4j.minisat.SolverFactory
import org.sat4j.core.VecInt
import scala.collection.mutable

trait CNF {
  def clauses: Set[Set[Literal]]
}

object CNF{
  def toProblem(clauses: Set[Set[Literal]], solver: ISolver = SolverFactory.newDefault): (ISolver, Map[Int, BVar]) = {
    val varToIndex: Map[BVar, Int] = clauses.flatten.zipWithIndex.map{case (k, v) => k.variable -> (v + 1)}(collection.breakOut)

    val literalCache = new mutable.HashMap[Literal,Int]()

    def clauseToIvec(clause: Set[Literal]): IVecInt = new VecInt(clause.map{lit =>
      literalCache.getOrElseUpdate(lit, lit match {
        case Negation(v) => -varToIndex(v)
        case Plain(v) => varToIndex(v)
      })
    }(collection.breakOut): Array[Int])

    solver.setExpectedNumberOfClauses(clauses.size)

    clauses.foreach(cl => solver.addClause(clauseToIvec(cl)))

    (solver,varToIndex.map(_.swap))
  }
}

case class OneOf(literals: Iterable[Literal]) extends CNF{
  def oneOfNoNewVars(literals: Iterable[Literal]): Set[Set[Literal]] = {
    val indexedLiterals = literals.toIndexedSeq
    val result = Set.newBuilder[Set[Literal]]
    def allPairs(i: Int = 0, j: Int = 1): Unit = {
      if(j < indexedLiterals.length){
        result += Set(indexedLiterals(i).not,indexedLiterals(j).not)
        allPairs(i,j+1)
      } else if(i < indexedLiterals.length)
        allPairs(i+1,i+2)
    }
    allPairs()
    result += indexedLiterals.toSet
    result.result()
  }
  def clauses: Set[Set[Literal]] = oneOfNoNewVars(literals)
}

