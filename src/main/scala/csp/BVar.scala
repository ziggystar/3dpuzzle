package csp

case class BVar(name: Symbol, data: Any){
  val not: Literal = Negation(this)
  val plain: Literal = Plain(this)

  override val hashCode: Int = (name,data).hashCode()
}

sealed trait Literal{
  def variable: BVar
  def sign: Boolean
  def not: Literal
}
case class Negation(variable: BVar) extends Literal{
  def sign: Boolean = false
  def not: Literal = variable.plain
}
case class Plain(variable: BVar) extends Literal{
  def sign: Boolean = true
  def not: Literal = variable.not
}
