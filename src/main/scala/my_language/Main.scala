package my_language

object Main {
  def main(args: Array[String]): Unit = {
    println("hi")
    new Machine().run(Prod(Sum(Number(1),Prod(Number(2),Sum(Number(4),Var("x")))),Sum(Number(4),Var("y"))), Map("x" -> 1, "y" -> 3))
    new Machine().run(LogicalLess(IfElse(LogicalLess(Number(3),Number(5)),Number(8),Number(78)),Number(10)), Map("x" -> 1, "y" -> 3))
    //println(Booleans(true).show)
    //1println(LogicalLess(Number(4), Number(-17)).evalBool)
    //new Machine().run(Prod(Sum(Number(1),Prod(Number(2),Sum(Number(4),Number(5)))),Sum(Number(4),Booleans(true))))
  }
}
