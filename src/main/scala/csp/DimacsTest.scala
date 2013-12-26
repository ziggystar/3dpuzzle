package csp

import java.io.FileWriter

/**
 * Created by thomas on 12/25/13.
 */
object DimacsTest extends App {
  val dimacs = CNF.dimacs(OneOf((1 to 100) map (i => BVar('test,i).plain)).clauses)
  private val writer: FileWriter = new FileWriter("test.cnf", false)
  writer.write(dimacs)
  writer.close()

}
