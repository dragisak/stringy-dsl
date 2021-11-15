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
  val plus: P[Unit]          = (whitespaces0.with1 ~ P.char('+') ~ whitespaces0).void
  val equals: P[Unit]        = (whitespaces0.with1 ~ P.string("==") ~ whitespaces0).void
  val notEquals: P[Unit]     = (whitespaces0.with1 ~ P.string("!=") ~ whitespaces0).void
  val parensL: P[Unit]       = (whitespaces0.with1 ~ P.char('(') ~ whitespaces0).void
  val parensR: P[Unit]       = (whitespaces0.with1 ~ P.char(')') ~ whitespaces0).void
  val curlyL: P[Unit]        = (whitespaces0.with1 ~ P.char('{') ~ whitespaces0).void
  val curlyR: P[Unit]        = (whitespaces0.with1 ~ P.char('}') ~ whitespaces0).void
  val `if`: P[Unit]          = (whitespaces0.with1 ~ P.string("if") ~ whitespaces0).void
  val `else`: P[Unit]        = (whitespaces0.with1 ~ P.string("else") ~ whitespaces0).void

  val reservedWords: P[Unit] = `if` | `else`

  val firstParamChar: P[Char] = P.charIn(('a' to 'z') ++ ('A' to 'Z') :+ '_')
  val anyParamChar: P[Char]   = digit | firstParamChar | P.char('.').as('.')

  val param: P0[Expr] = (!reservedWords *> (firstParamChar ~ anyParamChar.rep0.string)).map { case (first, rest) =>
    Param(first + rest)
  }

  val number: P[Expr]  = digits.map(s => Num(s.toInt))
  val string: P0[Expr] = P.anyChar.rep0.string.surroundedBy(P.char('\'')).map(Str)
  val `null`: P[Expr]  = P.string("null").as(Null)

  def constant: P0[Expr] = P.defer0(number | string | param | add.between(parensL, parensR))

  def add: P0[Expr] = P
    .defer0(constant ~ (plus *> constant).rep0)
    .map { case (a, b) => Add(a, b) }

  val eq: P0[Bool] = ((add <* equals) ~ add)
    .map { case (a, b) => Eq(a, b) }

  val ne: P0[Bool] = ((add <* notEquals) ~ add)
    .map { case (a, b) => Ne(a, b) }

  val conditional: P0[Bool] = eq | ne

  def ifElse: P[Expr] =
    ((`if` *> conditional.between(parensL, parensR) ~
      expr.between(curlyL, curlyR) <* `else`) ~
      expr.between(curlyL, curlyR))
      .map { case ((cond, whenTrue), whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }

  def expr: P0[Expr] = P.defer0(ifElse | add)

  val dsl: P0[Expr] = expr <* P.end

  def parseDsl(s: String): Either[Error, Expr] = dsl.parseAll(s.trim)
}
