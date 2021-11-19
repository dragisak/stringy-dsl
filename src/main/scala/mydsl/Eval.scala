package mydsl

import higherkindness.droste._

object Eval {

  type Result = Either[Either[String, Int], Boolean]

  object Result {
    def apply(value: Int): Result     = Left(Right(value))
    def apply(value: String): Result  = Left(Left(value))
    def apply(value: Boolean): Result = Right(value)

    def toString(r: Result): String = r match {
      case null           => "null"
      case Left(Left(s))  => s""""$s""""
      case Left(Right(i)) => i.toString
      case Right(b)       => b.toString
    }
  }

  val coalgebra: Coalgebra[ExprT, Expr] = Coalgebra[ExprT, Expr] {
    case Num(value)      => NumT(value)
    case Str(value)      => StrT(value)
    case Param(name)     => ParamT(name)
    case Null            => NullT()
    case Add(a, b)       => AddT(a, b)
    case Eq(a, b)        => EqT(a, b)
    case Ne(a, b)        => NeT(a, b)
    case IfElse(c, a, b) => IfElseT(c, a, b)
  }

  implicit class ResultOps(val x: Result) extends AnyVal {
    def +(y: Result): Result = (x, y) match {
      case (Left(Left(s)), Left(Left(p)))   => Result(s"$s$p")
      case (Left(Right(s)), Left(Right(p))) => Result(s + p)
      case (Left(Right(s)), Left(Left(p)))  => Result(s"$s$p")
      case _                                => throw new IllegalArgumentException("Supported params: Int or String")
    }
  }

  def algebra(input: Map[String, Result] = Map.empty): Algebra[ExprT, Result] = Algebra[ExprT, Result] {
    case NumT(value)      => Result(value)
    case StrT(value)      => Result(value)
    case ParamT(name)     => input.getOrElse(name, null)
    case NullT()          => null
    case AddT(a, b)       => b.foldLeft(a)(_ + _)
    case EqT(a, b)        => Result(a == b)
    case NeT(a, b)        => Result(a != b)
    case IfElseT(c, a, b) =>
      c match {
        case Right(bool) => if (bool) a else b
        case _           => throw new IllegalArgumentException("Incorrect if condition")
      }
  }

  def compute(input: Map[String, Result] = Map.empty): Expr => Result =
    scheme.ghylo(
      algebra(input).gather(Gather.cata),
      coalgebra.scatter(Scatter.ana)
    )

}
