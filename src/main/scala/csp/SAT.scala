package csp

trait CNF {
  def clauses: Set[Set[Literal]]
}

object CNF{
  def dimacs(clauses: Set[Set[Literal]]): String = {
    val varToIndex = clauses.flatten.map(_.variable).toSeq.distinct.zipWithIndex.toMap
    val numVars = varToIndex.size
    val clauseString = clauses.map{cl =>
      (cl.map{
        case Negation(v) => -(varToIndex(v) + 1)
        case Plain(v) => varToIndex(v) + 1
      }.toSeq :+ 0).mkString(" ")
    }.mkString("\n")
    f"p cnf $numVars ${clauses.size}\n$clauseString"
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

