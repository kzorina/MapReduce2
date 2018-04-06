package my_language

final class Machine {

  def run(expr: Expr, env: Map[String, Int]): Expr = {
    println(expr.show)
    if (expr.isReduciable)
      run(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Int]): Expr = expr match {
    case Var(name) => Number(env(name))
    case Sum(lOp, rOp) => if (lOp.isReduciable)
                            Sum(reductionStep(lOp, env), rOp)
                          else if (rOp.isReduciable)
                            Sum(lOp,reductionStep(rOp, env))
                          else Sum(lOp, rOp).eval
    case Prod(lOp, rOp) => if (lOp.isReduciable)
                              Prod(reductionStep(lOp, env), rOp)
                            else if (rOp.isReduciable)
                              Prod(lOp,reductionStep(rOp, env))
                            else Prod(lOp, rOp).eval
    case LogicalLess(lOp, rOp) => if (lOp.isReduciable)
                                    LogicalLess(reductionStep(lOp, env), rOp)
                                  else if (rOp.isReduciable)
                                    LogicalLess(lOp,reductionStep(rOp, env))
                                  else LogicalLess(lOp, rOp).eval
    case IfElse(statement, lOp, rOp) => if (statement.isReduciable)
                                          IfElse(reductionStep(statement, env), lOp, rOp)
                                        else if (lOp.isReduciable)
                                          IfElse(statement, reductionStep(lOp, env), rOp)
                                        else if (rOp.isReduciable)
                                          IfElse(statement, lOp, reductionStep(rOp, env))
                                        else IfElse(statement, lOp, rOp).eval

  }
}
