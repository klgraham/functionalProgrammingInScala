package week4

trait Expr {
    def eval: Int = this match {
        case Number(n) => n
        case Sum(e1, e2) => e1.eval + e2.eval
        case Prod(e1, e2) => e1.eval * e2.eval
    }
}

case class Number(n : Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr
case class Prod(l: Expr, r: Expr) extends Expr

object Number {
    def apply(n: Int): Number = new Number(n)
}

object Var {
    def apply(x: String): String = x
}

object Sum {
    def apply(e1: Expr, e2: Expr): Sum = new Sum(e1, e2)
}

object Prod {
    def apply(e1: Expr, e2: Expr): SProdm = new Prod(e1, e2)
}

object exprs {
    def show(e: Expr): String = e match {
        case Number(n) => n.toString
        case Sum(e1, e2) => "(" + show(e1) + " + " + show(e2) + ")"
        case Var(x) => x.toString
        case Prod(x, y) => "(" + show(x) + " * " + show(y) + ")"
    }
}