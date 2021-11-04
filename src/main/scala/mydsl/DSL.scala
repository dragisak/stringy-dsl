package mydsl

import fastparse._
import NoWhitespace._

object DSL {

  sealed trait Expr
  case class Num(value: Int)                      extends Expr
  case class Str(value: String)                   extends Expr
  case class Add(a: Expr, b: Expr)                extends Expr
  sealed trait Bool                               extends Expr
  case class Eq(a: Expr, b: Expr)                 extends Bool
  case class IfElse[T](c: Bool, a: Expr, b: Expr) extends Expr

  def plus[_: P]: P[Unit]    = P("+")
  def equals[_: P]: P[Unit]  = P("==")
  def parensL[_: P]: P[Unit] = P("(")
  def parensR[_: P]: P[Unit] = P(")")
  def curlyL[_: P]: P[Unit]  = P("{")
  def curlyR[_: P]: P[Unit]  = P("}")

  def number[_: P]: P[Expr] = P(CharIn("0-9").rep(1).!.map(s => Num(s.toInt)))
  def string[_: P]: P[Expr] = P(("'" ~ AnyChar.rep.! ~ "'").map(Str))

  def add[_: P](p: => P[Expr]): P[Expr] = P((p ~ plus ~ p).map { case (a, b) => Add(a, b) })

  def eq[_: P, T](p: => P[Expr]): P[Bool] = P((p ~ equals ~ p).map { case (a, b) => Eq(a, b) })

  def ifElse[_: P, T](p: => P[Expr]): P[Expr] =
    P(
      ("if" ~ parensL ~ eq(p) ~ parensR ~ curlyL ~ add(p) ~ curlyR ~ "else" ~ curlyL ~ add(p) ~ curlyR)
        .map { case (cond, whenTrue, whenFalse) =>
          IfElse(cond, whenTrue, whenFalse)
        }
    )

}
