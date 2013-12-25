package csp

/**
 * Created by thomas on 12/25/13.
 */
object DimacsTest extends App {
  println(CNF.dimacs(OneOf(BVar('test,1).plain :: BVar('test,2).plain :: Nil).clauses))

}
