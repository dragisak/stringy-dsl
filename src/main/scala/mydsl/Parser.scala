package mydsl

import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.Numbers._
import cats.parse.Parser._

object Parser {

  private val whitespace: P[Unit]    = P.charIn(" \t\r\n").void
  private val whitespaces0: P0[Unit] = whitespace.rep0.void
  private val plus: P[Unit]          = P.char('+').surroundedBy(whitespaces0)
  private val equals: P[Unit]        = P.string("==").surroundedBy(whitespaces0)
  private val notEquals: P[Unit]     = P.string("!=").surroundedBy(whitespaces0)
  private val parensL: P[Unit]       = P.char('(').surroundedBy(whitespaces0)
  private val parensR: P[Unit]       = P.char(')').surroundedBy(whitespaces0)
  private val curlyL: P[Unit]        = P.char('{').surroundedBy(whitespaces0)
  private val curlyR: P[Unit]        = P.char('}').surroundedBy(whitespaces0)
  private val `if`: P[Unit]          = P.string("if").surroundedBy(whitespaces0)
  private val `else`: P[Unit]        = P.string("else").surroundedBy(whitespaces0)

  private val reservedWords: P[Unit] = `if` | `else`

  private val firstParamChar: P[Char] = P.charIn(('a' to 'z') ++ ('A' to 'Z') :+ '_')
  private val anyParamChar: P[Char]   = P.charIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') :+ '_' :+ '.')

  private val param: P0[Expr] = (!reservedWords *> (firstParamChar ~ anyParamChar.rep0.string))
    .map { case (first, rest) =>
      Param(s"$first$rest")
    }
    .withContext("param")

  private val number: P[Expr] = digits.map(s => Num(s.toInt)).withContext("num")

  // TODO escape '
  private val string: P0[Expr] =
    P.charsWhile(_ != '\'').rep0.string.surroundedBy(P.char('\'')).map(Str).withContext("str")
  private val `null`: P[Expr]  = P.string("null").as(Null)

  private val constant: P0[Expr] =
    (number | string | `null` | param | op.between(parensL, parensR)).surroundedBy(whitespaces0).withContext("constant")

  private val booleanOp: P[Either[Unit, Unit]] = equals.eitherOr(notEquals)

  private val conditional: P0[Bool] = (op ~ booleanOp ~ op)
    .between(parensL, parensR)
    .map {
      case ((e1, Right(_)), e2) => Eq(e1, e2)
      case ((e1, Left(_)), e2)  => Ne(e1, e2)
    }
    .withContext("conditional")

  private def op: P0[Expr] = P.defer0(constant ~ (plus *> constant).backtrack.rep0).map(Add.tupled).withContext("op")

  private val ifElse: P[Expr] =
    ((`if` *> conditional ~
      expr.between(curlyL, curlyR) <* `else`) ~
      expr.between(curlyL, curlyR))
      .map { case ((cond, whenTrue), whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }
      .withContext("if-else")

  private def expr: P0[Expr] = P.defer0(ifElse | op).withContext("expr")

  private val dsl: P0[Expr] = expr <* P.end

  def parseDsl(s: String): Either[Error, Expr] = dsl.parseAll(s.trim)
}
