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

  implicit class ResultOps(val x: Result) extends AnyVal {
    def +(y: Result): Result = (x, y) match {
      case (Left(Left(s)), Left(Left(p)))   => Result(s"$s$p")
      case (Left(Left(s)), Left(Right(p)))  => Result(s"$s$p")
      case (Left(Right(s)), Left(Right(p))) => Result(s + p)
      case (Left(Right(s)), Left(Left(p)))  => Result(s"$s$p")
      case (Right(b), Left(Left(p)))        => Result(s"$b$p")
      case (Right(b), Left(Right(p)))       => Result(s"$b$p")
      case (Left(Left(s)), Right(b))        => Result(s"$s$b")
      case (Left(Right(s)), Right(b))       => Result(s"$s$b")
      case (Right(a), Right(b))             => Result(a && b)
    }
  }

  val coalgebra: Coalgebra[ExprT, Expr] = Coalgebra {
    case Num(value)       => NumT(value)
    case Str(value)       => StrT(value)
    case Param(name)      => ParamT(name)
    case Null             => NullT()
    case Neg(value)       => NegT(value)
    case Add(a, b)        => AddT(a, b)
    case BoolConst(value) => BoolConstT(value)
    case Eq(a, b)         => EqT(a, b)
    case Ne(a, b)         => NeT(a, b)
    case IfElse(c, a, b)  => IfElseT(c, a, b)
  }

  def algebra(input: Map[String, Result] = Map.empty): Algebra[ExprT, Result] = Algebra[ExprT, Result] {
    case NumT(value)       => Result(value)
    case StrT(value)       => Result(value)
    case ParamT(name)      => input.getOrElse(name, null)
    case NullT()           => null
    case NegT(value)       =>
      value match {
        case Left(Right(i)) => Result(-i)
        case _              => throw new IllegalArgumentException("Only integers can be negated")
      }
    case AddT(a, b)        => b.foldLeft(a)(_ + _)
    case BoolConstT(value) => Result(value)
    case EqT(a, b)         => Result(a == b)
    case NeT(a, b)         => Result(a != b)
    case IfElseT(c, a, b)  =>
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
