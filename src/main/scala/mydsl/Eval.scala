package mydsl

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import higherkindness.droste._
import scala.collection.mutable

object Eval {

  sealed trait NumberResult
  final case class IntResult(value: Int)           extends NumberResult
  final case class DoubleResult(value: Double)     extends NumberResult
  final case class ArrayResult(value: Seq[Result]) extends NumberResult

  type Result = Either[Either[String, NumberResult], Boolean]

  object Result {
    def apply(value: Int): Result          = Left(Right(IntResult(value)))
    def apply(value: Double): Result       = Left(Right(DoubleResult(value)))
    def apply(value: String): Result       = Left(Left(value))
    def apply(value: Boolean): Result      = Right(value)
    def apply(value: NumberResult): Result = Left(Right(value))
    def apply(value: Seq[Result]): Result  = Left(Right(ArrayResult(value)))

    def numberToString(n: NumberResult): String = n match {
      case IntResult(i)        => i.toString
      case DoubleResult(d)     =>
        if (d.isWhole) d.toLong.toString
        else BigDecimal(d).bigDecimal.stripTrailingZeros.toPlainString
      case ArrayResult(values) =>
        values.map(plainToString).mkString("[", ", ", "]")
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
    case _               => throw new IllegalArgumentException("Only numbers are allowed in arithmetic operations")
  }

  private def asNumber(value: Result): NumberResult =
    value match {
      case Left(Right(n @ IntResult(_)))    => n
      case Left(Right(n @ DoubleResult(_))) => n
      case _                                => throw new IllegalArgumentException("Only numbers are allowed in arithmetic operations")
    }

  private def asArray(value: Result, functionName: String): Seq[Result] =
    value match {
      case Left(Right(ArrayResult(values))) => values
      case _                                => throw new IllegalArgumentException(s"$functionName expects array argument")
    }

  private def asString(value: Result, functionName: String): String =
    value match {
      case Left(Left(s)) => s
      case _             => throw new IllegalArgumentException(s"$functionName expects string argument")
    }

  private def asInt(value: Result, functionName: String): Int =
    value match {
      case Left(Right(IntResult(i))) => i
      case _                         => throw new IllegalArgumentException(s"$functionName expects integer arguments")
    }

  private def substring(value: String, start: Int, length: Option[Int]): String = {
    if (start < 0 || length.exists(_ < 0))
      throw new IllegalArgumentException("substr expects non-negative start and length")
    else if (start >= value.length) ""
    else
      length match {
        case Some(0) => ""
        case Some(l) => value.substring(start, math.min(value.length, start + l))
        case None    => value.substring(start)
      }
  }

  private def md5Hex(value: String): String = {
    val messageDigest = MessageDigest.getInstance("MD5")
    val bytes         = messageDigest.digest(value.getBytes(StandardCharsets.UTF_8))
    bytes.map(byte => f"${byte & 0xff}%02x").mkString
  }

  private def negateNumber(n: NumberResult): NumberResult = n match {
    case IntResult(i)    => IntResult(-i)
    case DoubleResult(d) => DoubleResult(-d)
    case _               => throw new IllegalArgumentException("Only numbers are allowed in arithmetic operations")
  }

  private def absoluteNumber(n: NumberResult): NumberResult = n match {
    case IntResult(i)    => IntResult(math.abs(i))
    case DoubleResult(d) => DoubleResult(math.abs(d))
    case _               => throw new IllegalArgumentException("Only numbers are allowed in arithmetic operations")
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
    case IntNum(value)        => IntNumT(value)
    case DoubleNum(value)     => DoubleNumT(value)
    case Str(value)           => StrT(value)
    case Param(name)          => ParamT(name)
    case Null                 => NullT()
    case Neg(value)           => NegT(value)
    case Mul(a, b)            => MulT(a, b)
    case Div(a, b)            => DivT(a, b)
    case Add(a, b)            => AddT(a, b)
    case Substr(v, s, l)      => SubstrT(v, s, l)
    case Md5(v)               => Md5T(v)
    case Length(v)            => LengthT(v)
    case Abs(v)               => AbsT(v)
    case BoolConst(value)     => BoolConstT(value)
    case Eq(a, b)             => EqT(a, b)
    case Ne(a, b)             => NeT(a, b)
    case Lt(a, b)             => LtT(a, b)
    case IfElse(c, a, b)      => IfElseT(c, a, b)
    case VarDecl(_, _)        => throw new IllegalArgumentException("Var declarations can only appear in top-level scripts")
    case Assign(_, _)         => throw new IllegalArgumentException("Assignments can only appear in top-level scripts")
    case Inc(_)               => throw new IllegalArgumentException("Increments can only appear in top-level scripts")
    case Append(_, _)         => throw new IllegalArgumentException("Append operations can only appear in top-level scripts")
    case Remove(_, _)         => throw new IllegalArgumentException("Remove operations can only appear in top-level scripts")
    case ArrayIndex(_, _)     => throw new IllegalArgumentException("Array indexing can only appear in top-level scripts")
    case ArrayEmpty           => throw new IllegalArgumentException("Array literals can only appear in top-level scripts")
    case Block(_)             => throw new IllegalArgumentException("Blocks can only be evaluated at top level")
    case ForLoop(_, _, _, _)  => throw new IllegalArgumentException("For loops can only be evaluated at top level")
    case ForEachLoop(_, _, _) => throw new IllegalArgumentException("For-each loops can only be evaluated at top level")
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
    case SubstrT(v, s, l)  =>
      Result(substring(asString(v, "substr"), asInt(s, "substr"), l.map(asInt(_, "substr"))))
    case Md5T(v)           => Result(md5Hex(Result.plainToString(v)))
    case LengthT(v)        =>
      v match {
        case Left(Left(s))                 => Result(s.length)
        case Left(Right(ArrayResult(arr))) => Result(arr.length)
        case _                             => throw new IllegalArgumentException("length expects string or array argument")
      }
    case AbsT(v)           => Result(absoluteNumber(asNumber(v)))
    case BoolConstT(value) => Result(value)
    case EqT(a, b)         =>
      (a, b) match {
        case (Left(Right(x @ IntResult(_))), Left(Right(y @ IntResult(_))))       => Result(sameNumberValue(x, y))
        case (Left(Right(x @ IntResult(_))), Left(Right(y @ DoubleResult(_))))    => Result(sameNumberValue(x, y))
        case (Left(Right(x @ DoubleResult(_))), Left(Right(y @ IntResult(_))))    => Result(sameNumberValue(x, y))
        case (Left(Right(x @ DoubleResult(_))), Left(Right(y @ DoubleResult(_)))) => Result(sameNumberValue(x, y))
        case _                                                                    => Result(a == b)
      }
    case NeT(a, b)         =>
      (a, b) match {
        case (Left(Right(x @ IntResult(_))), Left(Right(y @ IntResult(_))))       => Result(!sameNumberValue(x, y))
        case (Left(Right(x @ IntResult(_))), Left(Right(y @ DoubleResult(_))))    => Result(!sameNumberValue(x, y))
        case (Left(Right(x @ DoubleResult(_))), Left(Right(y @ IntResult(_))))    => Result(!sameNumberValue(x, y))
        case (Left(Right(x @ DoubleResult(_))), Left(Right(y @ DoubleResult(_)))) => Result(!sameNumberValue(x, y))
        case _                                                                    => Result(a != b)
      }
    case LtT(a, b)         =>
      Result(toDouble(asNumber(a)) < toDouble(asNumber(b)))
    case IfElseT(c, a, b)  =>
      c match {
        case Right(bool) => if (bool) a else b
        case _           => throw new IllegalArgumentException("Incorrect if condition")
      }
  }

  private def computeExpr(input: Map[String, Result]): Expr => Result =
    scheme.ghylo(
      algebra(input).gather(Gather.cata),
      coalgebra.scatter(Scatter.ana)
    )

  def compute(input: Map[String, Result] = Map.empty): Expr => Result = { expr =>
    val env = List(mutable.Map.from(input))
    evalExpr(expr, env)
  }

  private def evalExpr(expr: Expr, env: List[mutable.Map[String, Result]]): Result =
    expr match {
      case VarDecl(name, valueExpr) =>
        val value = evalExpr(valueExpr, env)
        env.head.update(name, value)
        value

      case Assign(name, valueExpr) =>
        val frame =
          env
            .find(_.contains(name))
            .getOrElse(throw new IllegalArgumentException(s"Cannot assign undefined variable '$name'"))
        val value = evalExpr(valueExpr, env)
        frame.update(name, value)
        value

      case Append(name, valueExpr) =>
        val frame   =
          env
            .find(_.contains(name))
            .getOrElse(throw new IllegalArgumentException(s"Cannot append to undefined variable '$name'"))
        val current = frame(name)
        val arr     = asArray(current, "append")
        val value   = evalExpr(valueExpr, env)
        val updated = Result(arr :+ value)
        frame.update(name, updated)
        updated

      case Remove(name, indexExpr) =>
        val frame   =
          env
            .find(_.contains(name))
            .getOrElse(throw new IllegalArgumentException(s"Cannot remove from undefined variable '$name'"))
        val current = frame(name)
        val arr     = asArray(current, "remove")
        val index   = asInt(evalExpr(indexExpr, env), "remove")
        if (index < 0 || index >= arr.length)
          throw new IllegalArgumentException(s"Array index out of bounds: $index")
        val updated = Result(arr.patch(index, Nil, 1))
        frame.update(name, updated)
        updated

      case ArrayIndex(name, indexExpr) =>
        val frame   =
          env
            .find(_.contains(name))
            .getOrElse(throw new IllegalArgumentException(s"Cannot index undefined variable '$name'"))
        val current = frame(name)
        val arr     = asArray(current, "index")
        val index   = asInt(evalExpr(indexExpr, env), "index")
        if (index < 0 || index >= arr.length)
          throw new IllegalArgumentException(s"Array index out of bounds: $index")
        arr(index)

      case ArrayEmpty =>
        Result(Seq.empty)

      case Inc(name) =>
        val frame   =
          env
            .find(_.contains(name))
            .getOrElse(throw new IllegalArgumentException(s"Cannot increment undefined variable '$name'"))
        val current = frame(name)
        val updated = current match {
          case Left(Right(IntResult(i))) => Result(i + 1)
          case _                         => throw new IllegalArgumentException(s"Increment expects integer variable '$name'")
        }
        frame.update(name, updated)
        updated

      case Block(statements) =>
        if (statements.isEmpty)
          throw new IllegalArgumentException("Program must contain at least one statement")

        val scopedEnv    = mutable.Map.empty[String, Result] :: env
        var last: Result = null
        statements.foreach { statement =>
          last = evalExpr(statement, scopedEnv)
        }
        last

      case IfElse(c, a, b) =>
        evalExpr(c, env) match {
          case Right(bool) =>
            val branchEnv = mutable.Map.empty[String, Result] :: env
            if (bool) evalExpr(a, branchEnv) else evalExpr(b, branchEnv)
          case _           => throw new IllegalArgumentException("Incorrect if condition")
        }

      case ForLoop(init, condition, update, body) =>
        val loopEnv = mutable.Map.empty[String, Result] :: env
        val _       = evalExpr(init, loopEnv)

        var last: Result = null
        var running      = true

        while (running) {
          val shouldRun = evalExpr(condition, loopEnv) match {
            case Right(bool) => bool
            case _           => throw new IllegalArgumentException("Incorrect for condition")
          }

          if (shouldRun) {
            val iterationEnv = mutable.Map.empty[String, Result] :: loopEnv
            last = evalExpr(body, iterationEnv)
            val _            = evalExpr(update, loopEnv)
          } else {
            running = false
          }
        }

        last

      case ForEachLoop(itemName, arrayName, body) =>
        val arrayFrame = env
          .find(_.contains(arrayName))
          .getOrElse(
            throw new IllegalArgumentException(s"Cannot iterate undefined variable '$arrayName'")
          )
        val values     = asArray(arrayFrame(arrayName), "for-in")

        var last: Result = null
        values.foreach { value =>
          val iterationEnv = mutable.Map.empty[String, Result] :: env
          iterationEnv.head.update(itemName, value)
          last = evalExpr(body, iterationEnv)
        }
        last

      case plainExpr =>
        computeExpr(view(env))(plainExpr)
    }

  private def view(env: List[mutable.Map[String, Result]]): Map[String, Result] =
    env.reverse.foldLeft(Map.empty[String, Result]) { (acc, frame) =>
      acc ++ frame.toMap
    }

}
