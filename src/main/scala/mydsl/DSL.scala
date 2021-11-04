package mydsl

import fastparse._
import SingleLineWhitespace._

object DSL {

  sealed trait Expr[T]
  case class Num(value: Int)                                     extends Expr[Int]
  case class Str(value: String)                                  extends Expr[String]
  case class Add[T](a: Expr[T], b: Seq[Expr[T]])                 extends Expr[T]
  case class Eq[T](a: Expr[T], b: Expr[T])                       extends Expr[Boolean]
  case class IfElse[T](c: Expr[Boolean], a: Expr[T], b: Expr[T]) extends Expr[T]

  def plus[_: P]: P[Unit]    = P("+")
  def equals[_: P]: P[Unit]  = P("==")
  def parensL[_: P]: P[Unit] = P("(")
  def parensR[_: P]: P[Unit] = P(")")
  def curlyL[_: P]: P[Unit]  = P("{")
  def curlyR[_: P]: P[Unit]  = P("}")

  def number[_: P]: P[Expr[Int]]    = P(CharIn("0-9").rep(1).!.map(s => Num(s.toInt)))
  def string[_: P]: P[Expr[String]] = P(("'" ~ CharIn("a-zA-Z0-9").rep.! ~ "'").map(Str))

  def add[_: P, T](p: => P[Expr[T]]): P[Expr[T]] = P(p ~ (plus ~ add(p)).rep)
    .map { case (a, b) => Add(a, b) }

  def eq[_: P, T](p: => P[Expr[T]]): P[Expr[Boolean]] = P(add(p) ~ equals ~ add(p))
    .map { case (a, b) => Eq(a, b) }

  def ifElse[_: P, T](p: => P[Expr[T]]): P[Expr[T]] =
    P("if" ~ parensL ~ eq(p) ~ parensR ~ curlyL ~ expr(p) ~ curlyR ~ "else" ~ curlyL ~ expr(p) ~ curlyR)
      .map { case (cond, whenTrue, whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }

  def expr[_: P, T](p: => P[Expr[T]]): P[Expr[T]] = P(ifElse(p) | add(p))

  def numExpr[_: P]: P[Expr[Int]]    = expr(number)
  def strExpr[_: P]: P[Expr[String]] = expr(string)

  def dsl[_: P] = P(numExpr | strExpr)

  def parseDsl(s: String) = parse(s.trim, dsl(_))
}
