package mydsl

import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.Numbers._
import cats.parse.Parser._
object DSL {

  sealed trait Expr
  case class Num(value: Int)                   extends Expr
  case class Str(value: String)                extends Expr
  case class Param(name: String)               extends Expr
  case object Null                             extends Expr
  case class Add(a: Expr, b: Seq[Expr])        extends Expr
  sealed trait Bool                            extends Expr
  case class Eq(a: Expr, b: Expr)              extends Bool
  case class Ne(a: Expr, b: Expr)              extends Bool
  case class IfElse(c: Bool, a: Expr, b: Expr) extends Expr

  val whitespace: P[Unit]    = P.charIn(" \t\r\n").void
  val whitespaces0: P0[Unit] = whitespace.rep0.void
  val plus: P[Unit]          = P.char('+').surroundedBy(whitespaces0)
  val equals: P[Unit]        = P.string("==").surroundedBy(whitespaces0)
  val notEquals: P[Unit]     = P.string("!=").surroundedBy(whitespaces0)
  val parensL: P[Unit]       = P.char('(').surroundedBy(whitespaces0)
  val parensR: P[Unit]       = P.char(')').surroundedBy(whitespaces0)
  val curlyL: P[Unit]        = P.char('{').surroundedBy(whitespaces0)
  val curlyR: P[Unit]        = P.char('}').surroundedBy(whitespaces0)
  val `if`: P[Unit]          = P.string("if").surroundedBy(whitespaces0)
  val `else`: P[Unit]        = P.string("else").surroundedBy(whitespaces0)

  val reservedWords: P[Unit] = `if` | `else`

  val firstParamChar: P[Char] = P.charIn(('a' to 'z') ++ ('A' to 'Z') :+ '_')
  val anyParamChar: P[Char]   = P.charIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') :+ '_' :+ '.')

  val param: P0[Expr] = (!reservedWords *> (firstParamChar ~ anyParamChar.rep0.string))
    .map { case (first, rest) =>
      Param(first + rest)
    }
    .withContext("param")

  val number: P[Expr]  = digits.map(s => Num(s.toInt)).withContext("num")
  // TODO escape '
  val string: P0[Expr] = P.charsWhile(_ != '\'').rep0.string.surroundedBy(P.char('\'')).map(Str).withContext("str")
  val `null`: P[Expr]  = P.string("null").as(Null)

  val constant: P0[Expr] = (number | string | param | op.between(parensL, parensR)).withContext("constant")

  val eq: P0[Bool] = ((op <* equals) ~ op).map(Eq.tupled).withContext("eq")

  val ne: P0[Bool] = ((op <* notEquals) ~ op).map(Ne.tupled).withContext("ne")

  def op: P0[Expr] = P.defer0(constant ~ (plus *> constant).rep0).map(Add.tupled).withContext("op")

  val conditional: P0[Bool] = (eq | ne).withContext("conditional")

  val ifElse: P[Expr] =
    ((`if` *> conditional.between(parensL, parensR) ~
      expr.between(curlyL, curlyR) <* `else`) ~
      expr.between(curlyL, curlyR))
      .map { case ((cond, whenTrue), whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }
      .withContext("if-else")

  def expr: P0[Expr] = P.defer0(ifElse | op).withContext("expr")

  val dsl: P0[Expr] = expr <* P.end

  def parseDsl(s: String): Either[Error, Expr] = dsl.parseAll(s.trim)
}
