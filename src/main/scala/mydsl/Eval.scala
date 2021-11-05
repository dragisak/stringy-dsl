package mydsl

import mydsl.DSL._

object Eval {

  type Result = Either[Either[String, Int], Boolean]
  object Result {
    def apply(value: Int): Result     = Left(Right(value))
    def apply(value: String): Result  = Left(Left(value))
    def apply(value: Boolean): Result = Right(value)

    def toString(r: Result): String = r match {
      case Left(Left(s))  => s""""$s""""
      case Left(Right(i)) => i.toString
      case Right(b)       => b.toString
    }
  }

  implicit class ResultOps(val x: Result) extends AnyVal {
    def +(y: Result): Result = (x, y) match {
      case (Left(Left(s)), Left(Left(p)))   => Result(s"$s$p")
      case (Left(Left(s)), Left(Left(p)))   => Result(s"$s$p")
      case (Left(Right(s)), Left(Right(p))) => Result(s + p)
      case (Left(Right(s)), Left(Left(p)))  => Result(s"$s$p")
      case _                                => throw new IllegalArgumentException("Supported params: Int, String or Boolean")
    }
  }

  def compute(expr: Expr, input: Map[String, Result] = Map.empty): Result = expr match {
    case Num(value)      => Result(value)
    case Str(value)      => Result(value)
    case Param(name)     => input.getOrElse(name, null)
    case Null            => null
    case Add(a, b)       => b.foldLeft(compute(a, input))((acc, x) => acc + compute(x, input))
    case Eq(a, b)        => Result(compute(a, input) == compute(b, input))
    case Ne(a, b)        => Result(compute(a, input) != compute(b, input))
    case IfElse(c, a, b) =>
      compute(c, input) match {
        case Right(bool) => if (bool) compute(a, input) else compute(b, input)
        case _           => throw new IllegalArgumentException("Incorrect if condition")
      }
  }

}
