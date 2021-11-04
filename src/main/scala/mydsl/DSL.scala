package mydsl

import fastparse._
import NoWhitespace._

object DSL {

  sealed trait Expr[T]
  case class Num(value: Int)                                     extends Expr[Int]
  case class Str(value: String)                                  extends Expr[String]
  case class Add[T](a: Expr[T], b: Expr[T])                      extends Expr[T]
  case class Eq[T](a: Expr[T], b: Expr[T])                       extends Expr[Boolean]
  case class IfElse[T](c: Expr[Boolean], a: Expr[T], b: Expr[T]) extends Expr[T]

  def plus[_: P]: P[Unit]    = P("+")
  def equals[_: P]: P[Unit]  = P("==")
  def parensL[_: P]: P[Unit] = P("(")
  def parensR[_: P]: P[Unit] = P(")")
  def curlyL[_: P]: P[Unit]  = P("{")
  def curlyR[_: P]: P[Unit]  = P("}")

  def number[_: P]: P[Expr[Int]]    = P(CharIn("0-9").rep(1).!.map(s => Num(s.toInt)))
  def string[_: P]: P[Expr[String]] = P(("'" ~ AnyChar.rep.! ~ "'").map(Str))

  def add[_: P, T](p: => P[Expr[T]]): P[Expr[T]] = P((p ~ plus ~ p).map { case (a, b) => Add(a, b) })

  def eq[_: P, T](p: => P[Expr[T]]): P[Expr[Boolean]] = P((p ~ equals ~ p).map { case (a, b) => Eq(a, b) })

  def ifElse[_: P, T](p: => P[Expr[T]]): P[Expr[T]] =
    P(
      ("if" ~ parensL ~ eq(p) ~ parensR ~ curlyL ~ add(p) ~ curlyR ~ "else" ~ curlyL ~ add(p) ~ curlyR)
        .map { case (cond, whenTrue, whenFalse) =>
          IfElse(cond, whenTrue, whenFalse)
        }
    )

}
