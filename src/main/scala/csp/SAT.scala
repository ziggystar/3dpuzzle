package csp

trait CNF {
  def clauses: Set[Set[Literal]]
}

object CNF{
  def dimacs(clauses: Set[Set[Literal]]): (String,Map[Int,BVar]) = {
    val varToIndex: Map[BVar, Int] =
      clauses.flatten.map(_.variable).toSeq.distinct.zipWithIndex.toMap.mapValues(_ + 1)
    val numVars = varToIndex.size
    val clauseString = clauses.map{cl =>
      (cl.map{
        case Negation(v) => -varToIndex(v)
        case Plain(v) => varToIndex(v)
      }.toSeq :+ 0).mkString(" ")
    }.mkString("\n")
    val encoding = f"p cnf $numVars ${clauses.size}\n$clauseString"
    (encoding,varToIndex.map(_.swap))
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

