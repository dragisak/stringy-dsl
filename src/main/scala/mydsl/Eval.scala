package mydsl

import higherkindness.droste._

object Eval {

  sealed trait NumberResult
  final case class IntResult(value: Int)       extends NumberResult
  final case class DoubleResult(value: Double) extends NumberResult

  type Result = Either[Either[String, NumberResult], Boolean]

  object Result {
    def apply(value: Int): Result          = Left(Right(IntResult(value)))
    def apply(value: Double): Result       = Left(Right(DoubleResult(value)))
    def apply(value: String): Result       = Left(Left(value))
    def apply(value: Boolean): Result      = Right(value)
    def apply(value: NumberResult): Result = Left(Right(value))

    def numberToString(n: NumberResult): String = n match {
      case IntResult(i)    => i.toString
      case DoubleResult(d) =>
        if (d.isWhole) d.toLong.toString
        else BigDecimal(d).bigDecimal.stripTrailingZeros.toPlainString
    }

    def plainToString(r: Result): String = r match {
      case null           => "null"
      case Left(Left(s))  => s
      case Left(Right(n)) => numberToString(n)
      case Right(b)       => b.toString
    }

    def toString(r: Result): String = r match {
      case null           => "null"
      case Left(Left(s))  => s""""$s""""
      case Left(Right(n)) => numberToString(n)
      case Right(b)       => b.toString
    }
  }

  private def toDouble(n: NumberResult): Double = n match {
    case IntResult(i)    => i.toDouble
    case DoubleResult(d) => d
  }

  private def asNumber(value: Result): NumberResult =
    value match {
      case Left(Right(n)) => n
      case _              => throw new IllegalArgumentException("Only numbers are allowed in arithmetic operations")
    }

  private def negateNumber(n: NumberResult): NumberResult = n match {
    case IntResult(i)    => IntResult(-i)
    case DoubleResult(d) => DoubleResult(-d)
  }

  private def addNumbers(a: NumberResult, b: NumberResult): NumberResult = (a, b) match {
    case (IntResult(x), IntResult(y)) => IntResult(x + y)
    case _                            => DoubleResult(toDouble(a) + toDouble(b))
  }

  private def multiplyNumbers(a: NumberResult, b: NumberResult): NumberResult = (a, b) match {
    case (IntResult(x), IntResult(y)) => IntResult(x * y)
    case _                            => DoubleResult(toDouble(a) * toDouble(b))
  }

  private def divideNumbers(a: NumberResult, b: NumberResult): NumberResult =
    DoubleResult(toDouble(a) / toDouble(b))

  private def sameNumberValue(a: NumberResult, b: NumberResult): Boolean =
    toDouble(a) == toDouble(b)

  implicit class ResultOps(val x: Result) extends AnyVal {
    def +(y: Result): Result = (x, y) match {
      case (Left(Left(s)), Left(Left(p)))   => Result(s"$s$p")
      case (Left(Left(s)), Left(Right(p)))  => Result(s"${s}${Result.numberToString(p)}")
      case (Left(Right(s)), Left(Right(p))) => Result(addNumbers(s, p))
      case (Left(Right(s)), Left(Left(p)))  => Result(s"${Result.numberToString(s)}$p")
      case (Right(b), Left(Left(p)))        => Result(s"$b$p")
      case (Right(b), Left(Right(p)))       => Result(s"${b}${Result.numberToString(p)}")
      case (Left(Left(s)), Right(b))        => Result(s"$s$b")
      case (Left(Right(s)), Right(b))       => Result(s"${Result.numberToString(s)}$b")
      case (Right(a), Right(b))             => Result(a && b)
    }
  }

  val coalgebra: Coalgebra[ExprT, Expr] = Coalgebra {
    case IntNum(value)    => IntNumT(value)
    case DoubleNum(value) => DoubleNumT(value)
    case Str(value)       => StrT(value)
    case Param(name)      => ParamT(name)
    case Null             => NullT()
    case Neg(value)       => NegT(value)
    case Mul(a, b)        => MulT(a, b)
    case Div(a, b)        => DivT(a, b)
    case Add(a, b)        => AddT(a, b)
    case BoolConst(value) => BoolConstT(value)
    case Eq(a, b)         => EqT(a, b)
    case Ne(a, b)         => NeT(a, b)
    case IfElse(c, a, b)  => IfElseT(c, a, b)
  }

  def algebra(input: Map[String, Result] = Map.empty): Algebra[ExprT, Result] = Algebra[ExprT, Result] {
    case IntNumT(value)    => Result(value)
    case DoubleNumT(value) => Result(value)
    case StrT(value)       => Result(value)
    case ParamT(name)      => input.getOrElse(name, null)
    case NullT()           => null
    case NegT(value)       => Result(negateNumber(asNumber(value)))
    case MulT(a, b)        => Result(multiplyNumbers(asNumber(a), asNumber(b)))
    case DivT(a, b)        => Result(divideNumbers(asNumber(a), asNumber(b)))
    case AddT(a, b)        => b.foldLeft(a)(_ + _)
    case BoolConstT(value) => Result(value)
    case EqT(a, b)         =>
      (a, b) match {
        case (Left(Right(x)), Left(Right(y))) => Result(sameNumberValue(x, y))
        case _                                => Result(a == b)
      }
    case NeT(a, b)         =>
      (a, b) match {
        case (Left(Right(x)), Left(Right(y))) => Result(!sameNumberValue(x, y))
        case _                                => Result(a != b)
      }
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
