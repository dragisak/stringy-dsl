package mydsl

import fastparse._
import SingleLineWhitespace._

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

  def plus[_: P]: P[Unit]      = P("+")
  def equals[_: P]: P[Unit]    = P("==")
  def notEquals[_: P]: P[Unit] = P("!=")
  def parensL[_: P]: P[Unit]   = P("(")
  def parensR[_: P]: P[Unit]   = P(")")
  def curlyL[_: P]: P[Unit]    = P("{")
  def curlyR[_: P]: P[Unit]    = P("}")
  def `if`[_: P]: P[Unit]      = P("if")
  def `else`[_: P]: P[Unit]    = P("else")

  def reservedWords[_: P]: P[Unit] = P(`if` | `else`)

  def param[_: P, T]: P[Expr] = P(!reservedWords ~ CharIn("a-zA-Z_").! ~ CharIn("a-zA-Z0-9._").rep.!)
    .map { case (first, rest) => Param(first + rest) }

  def number[_: P]: P[Expr] = P(CharIn("0-9").rep(1).!).map(s => Num(s.toInt))
  def string[_: P]: P[Expr] = P("'" ~ CharsWhile(_ != '\'', 1).! ~ "'").map(Str)
  def `null`[_: P]: P[Expr] = P("null").map(_ => Null)

  def constant[_: P]: P[Expr] = P(number | string | param | parens)

  def add[_: P]: P[Expr] = P(constant ~ (plus ~/ constant).rep)
    .map { case (a, b) => Add(a, b) }

  def parens[_: P]: P[Expr] = P(parensL ~/ add ~ parensR)

  def eq[_: P]: P[Bool] = P(add ~ equals ~ add)
    .map { case (a, b) => Eq(a, b) }

  def ne[_: P]: P[Bool] = P(add ~ notEquals ~ add)
    .map { case (a, b) => Ne(a, b) }

  def conditional[_: P]: P[Bool] = P(eq | ne)

  def ifElse[_: P]: P[Expr] =
    P(`if` ~/ parensL ~/ conditional ~ parensR ~/ curlyL ~/ expr ~ curlyR ~/ `else` ~/ curlyL ~/ expr ~ curlyR)
      .map { case (cond, whenTrue, whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }

  def expr[_: P]: P[Expr] = P(ifElse | add)

  def dsl[_: P]: P[Expr] = P(expr ~ End)

  def parseDsl(s: String): Parsed[Expr] = parse(s.trim, dsl(_))
}
