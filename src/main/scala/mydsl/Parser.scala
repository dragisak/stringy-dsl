package mydsl

import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.Numbers._
import cats.parse.Parser._

object Parser {

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
      Param(s"$first$rest")
    }
    .withContext("param")

  val number: P[Expr] = digits.map(s => Num(s.toInt)).withContext("num")

  // TODO escape '
  val string: P0[Expr] = P.charsWhile(_ != '\'').rep0.string.surroundedBy(P.char('\'')).map(Str).withContext("str")
  val `null`: P[Expr]  = P.string("null").as(Null)

  val constant: P0[Expr] =
    (number | string | param | op.between(parensL, parensR)).surroundedBy(whitespaces0).withContext("constant")

  val booleanOp: P[Either[Unit, Unit]] = equals.eitherOr(notEquals)

  val conditional: P0[Bool] = (op ~ booleanOp ~ op)
    .between(parensL, parensR)
    .map {
      case ((e1, Right(_)), e2) => Eq(e1, e2)
      case ((e1, Left(_)), e2)  => Ne(e1, e2)
    }
    .withContext("conditional")

  def op: P0[Expr] = P.defer0(constant ~ (plus *> constant).backtrack.rep0).map(Add.tupled).withContext("op")

  val ifElse: P[Expr] =
    ((`if` *> conditional ~
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
