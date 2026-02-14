package mydsl

import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.Numbers._
import cats.parse.Parser._

object Parser {

  private def decodeEscapes(input: String): String = {
    val out = new StringBuilder(input.length)
    var i   = 0
    while (i < input.length) {
      val c = input.charAt(i)
      if (c == '\\' && i + 1 < input.length) {
        input.charAt(i + 1) match {
          case '\''  => out += '\''
          case '\\'  => out += '\\'
          case 'n'   => out += '\n'
          case 'r'   => out += '\r'
          case 't'   => out += '\t'
          case other =>
            out += '\\'
            out += other
        }
        i += 2
      } else {
        out += c
        i += 1
      }
    }
    out.result()
  }

  private val whitespace: P[Unit]     = P.charIn(" \t\r\n").void
  private val whitespaces0: P0[Unit]  = whitespace.rep0.void
  private val plus: P[Unit]           = P.char('+').surroundedBy(whitespaces0)
  private val minus: P[Unit]          = P.char('-').surroundedBy(whitespaces0)
  private val equals: P[Unit]         = P.string("==").surroundedBy(whitespaces0)
  private val notEquals: P[Unit]      = P.string("!=").surroundedBy(whitespaces0)
  private val parensL: P[Unit]        = P.char('(').surroundedBy(whitespaces0)
  private val parensR: P[Unit]        = P.char(')').surroundedBy(whitespaces0)
  private val curlyL: P[Unit]         = P.char('{').surroundedBy(whitespaces0)
  private val curlyR: P[Unit]         = P.char('}').surroundedBy(whitespaces0)
  private val firstParamChar: P[Char] = P.charIn(('a' to 'z') ++ ('A' to 'Z') :+ '_')
  private val anyParamChar: P[Char]   = P.charIn(('0' to '9') ++ ('a' to 'z') ++ ('A' to 'Z') :+ '_' :+ '.')

  private val `if`: P[Unit]   = (P.string("if") <* !anyParamChar).surroundedBy(whitespaces0)
  private val `else`: P[Unit] = (P.string("else") <* !anyParamChar).surroundedBy(whitespaces0)

  private val reservedWords: P[Unit] = `if` | `else`

  private val param: P0[Expr] = (!reservedWords *> (firstParamChar ~ anyParamChar.rep0.string))
    .map { case (first, rest) =>
      Param(s"$first$rest")
    }
    .withContext("param")

  private val number: P[Expr] = digits.map(s => Num(s.toInt)).withContext("num")

  private val escapedChunk: P[String]   = (P.char('\\') ~ P.anyChar).map { case (_, c) => s"\\$c" }
  private val unescapedChunk: P[String] = P.charWhere(c => c != '\'' && c != '\\').map(_.toString)

  private val string: P0[Expr] = (escapedChunk | unescapedChunk).rep0
    .map(_.mkString)
    .surroundedBy(P.char('\''))
    .map(s => Str(decodeEscapes(s)))
    .withContext("str")

  private val `true`: P0[Bool]  = (P.string("true") <* !anyParamChar).as(BoolConst(true))
  private val `false`: P0[Bool] = (P.string("false") <* !anyParamChar).as(BoolConst(false))
  private val bool: P0[Bool]    = (`true` | `false`).withContext("bool")

  private val `null`: P0[Expr] = (P.string("null") <* !anyParamChar).as(Null)

  private val constant: P0[Expr] =
    (number | string | bool.backtrack | `null`.backtrack | param | op.between(parensL, parensR))
      .surroundedBy(whitespaces0)
      .withContext("constant")

  private val booleanOp: P[Either[Unit, Unit]] = equals.eitherOr(notEquals)

  private val comparison: P0[Bool] = (op ~ booleanOp ~ op)
    .map {
      case ((e1, Right(_)), e2) => Eq(e1, e2)
      case ((e1, Left(_)), e2)  => Ne(e1, e2)
    }
    .withContext("comparison")

  private val conditional: P0[Bool] = (comparison.backtrack | bool)
    .between(parensL, parensR)
    .withContext("conditional")

  private val addOrSubtract: P[Boolean] = plus.as(true) | minus.as(false)

  private def op: P0[Expr] = P
    .defer0(constant ~ (addOrSubtract ~ constant).backtrack.rep0)
    .map { case (head, tail) =>
      Add(head, tail.map { case (isAdd, expr) => if (isAdd) expr else Neg(expr) })
    }
    .withContext("op")

  private val ifElse: P[Expr] =
    ((`if` *> conditional ~
      expr.between(curlyL, curlyR) <* `else`) ~
      expr.between(curlyL, curlyR))
      .map { case ((cond, whenTrue), whenFalse) =>
        IfElse(cond, whenTrue, whenFalse)
      }
      .withContext("if-else")

  private def expr: P0[Expr] = P.defer0(ifElse.backtrack | op).withContext("expr")

  private val dsl: P0[Expr] = expr <* P.end

  def parseDsl(s: String): Either[Error, Expr] = dsl.parseAll(s.trim)
}
