package my_language

case class Number(n: Int) extends Expr
case class Booleans(b: Boolean) extends Expr
case class Sum(lOp: Expr, rOp: Expr) extends Expr{
  require ( lOp.isNumerical & rOp.isNumerical, "Sum cannot be applied to non numerical type" )
}
case class Prod(lOp: Expr, rOp: Expr) extends Expr{
  require ( lOp.isNumerical & rOp.isNumerical, "Prod cannot be applied to non numerical type" )
}
case class LogicalLess(lOp: Expr, rOp: Expr) extends Expr
case class Var(name: String) extends Expr
case class IfElse(statement: Expr, pos: Expr, neg: Expr) extends Expr

sealed trait Expr {
  def eval: Expr = this match {
    case Var(name) => Var(name)
    case Number(n) => Number(n)
    case Sum(lOp, rOp) => Number(lOp.evalNum + rOp.evalNum)
    case Prod(lOp, rOp) => Number(lOp.evalNum * rOp.evalNum)
    case Booleans(b) => Booleans(b)
    case LogicalLess(lOp, rOp) => Booleans(lOp.evalNum < rOp.evalNum)
    case IfElse(statement, lOp, rOp) => if (statement.evalBool) {lOp} else {rOp}
  }
  def evalBool: Boolean = this match {
    case Booleans(b) => b
    case LogicalLess(lOp, rOp) => (lOp.evalNum < rOp.evalNum)
  }
  def evalNum: Int = this match {
    case Number(n) => n
    case Sum(lOp, rOp) => lOp.evalNum + rOp.evalNum
    case Prod(lOp, rOp) => lOp.evalNum * rOp.evalNum
  }
  def show: String = this match {
    case Var(name) => name
    case Number(n) => n.toString()
    case Booleans(b) => b.toString()
    case Prod(lOp, rOp) =>  val left = if (lOp.isInstanceOf[Sum]){"("+lOp.show+")"} else lOp.show
      val right = if (rOp.isInstanceOf[Sum]){"("+rOp.show+")"} else rOp.show
      left+"*"+right
    case Sum(lOp, rOp) => lOp.show+" + "+rOp.show
    case LogicalLess(lOp, rOp) => "(" + lOp.show + " < " + rOp.show + ")"
    case IfElse(statement, lOp, rOp) => "[If " + statement.show + " then " + lOp.show + " else " + rOp.show + "]"
  }
  def isReduciable: Boolean = this match {
    case Number(_) => false
    case Sum(_,_) => true
    case Prod(_,_) => true
    case Var(_) => true
    case Booleans(_) => false
    case LogicalLess(_,_) => true
    case IfElse(_,_,_) => true
  }
  def isNumerical: Boolean = this match {
    case Number(_) => true
    case Booleans(_) => false
    case Sum(_,_) => true
    case Prod(_,_) => true
    case LogicalLess(_,_) => false
    case IfElse(_,_,_) => false
    case Var(_) => true
  }
}
