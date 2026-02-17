package mydsl

import cats.parse.{Parser => P}
import cats.parse.{Parser0 => P0}
import cats.parse.Numbers._
import cats.parse.Parser._

object Parser {

  private val identifierRegex = "[A-Za-z_][A-Za-z0-9_\\.]*"
  private val varDeclRegex    = s"""var\\s+($identifierRegex)\\s*=\\s*(.+)""".r
  private val assignRegex     = s"""($identifierRegex)\\s*=\\s*(.+)""".r
  private val appendRegex     = s"""($identifierRegex)\\s*\\.\\s*append\\s*\\((.+)\\)""".r
  private val removeRegex     = s"""($identifierRegex)\\s*\\.\\s*remove\\s*\\((.+)\\)""".r
  private val indexRegex      = s"""($identifierRegex)\\s*\\[(.+)\\]""".r
  private val arrayEmptyRegex = """array\s*\[\s*\]""".r
  private val incRegex        = s"""($identifierRegex)\\s*\\+\\+""".r

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
  private val multiply: P[Unit]       = P.char('*').surroundedBy(whitespaces0)
  private val divide: P[Unit]         = P.char('/').surroundedBy(whitespaces0)
  private val comma: P[Unit]          = P.char(',').surroundedBy(whitespaces0)
  private val equals: P[Unit]         = P.string("==").surroundedBy(whitespaces0)
  private val notEquals: P[Unit]      = P.string("!=").surroundedBy(whitespaces0)
  private val lessThan: P[Unit]       = P.char('<').surroundedBy(whitespaces0)
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

  private val integerPart: P[String]    = digits
  private val fractionalPart: P[String] = (P.char('.') *> digits).map("." + _)
  private val number: P[Expr]           = (integerPart ~ fractionalPart.?)
    .map { case (intPart, fractionPart) =>
      fractionPart match {
        case Some(fraction) => DoubleNum((intPart + fraction).toDouble)
        case None           => IntNum(intPart.toInt)
      }
    }
    .withContext("num")

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

  private def functionName(name: String): P[Unit] =
    (P.string(name) <* !anyParamChar).void

  private def substrArgs: P0[(Expr, Expr, Option[Expr])] =
    ((op <* comma) ~ op ~ (comma *> op).?).map { case ((value, start), length) =>
      (value, start, length)
    }

  private def substrCall: P0[Expr] =
    (functionName("substr") *> substrArgs.between(parensL, parensR))
      .map { case (value, start, length) =>
        Substr(value, start, length)
      }
      .withContext("substr")

  private def md5Call: P0[Expr] =
    (functionName("md5") *> op.between(parensL, parensR))
      .map(Md5)
      .withContext("md5")

  private def lengthCall: P0[Expr] =
    (functionName("length") *> op.between(parensL, parensR))
      .map(Length)
      .withContext("length")

  private def absCall: P0[Expr] =
    (functionName("abs") *> op.between(parensL, parensR))
      .map(Abs)
      .withContext("abs")

  private def functionCall: P0[Expr] =
    P.defer0(substrCall.backtrack | md5Call.backtrack | lengthCall.backtrack | absCall.backtrack)

  private val atom: P0[Expr] =
    (number | string | bool.backtrack | `null`.backtrack | functionCall.backtrack | param | op
      .between(parensL, parensR))
      .surroundedBy(whitespaces0)
      .withContext("atom")

  private val booleanOp: P[Int] = equals.as(0) | notEquals.as(1) | lessThan.as(2)

  private val comparison: P0[Bool] = (op ~ booleanOp ~ op)
    .map {
      case ((e1, 0), e2) => Eq(e1, e2)
      case ((e1, 1), e2) => Ne(e1, e2)
      case ((e1, 2), e2) => Lt(e1, e2)
      case _             => throw new IllegalStateException("Unsupported comparison operator")
    }
    .withContext("comparison")

  private val conditional: P0[Bool]   = (comparison.backtrack | bool)
    .between(parensL, parensR)
    .withContext("conditional")
  private val conditionExpr: P0[Bool] = (comparison.backtrack | bool) <* P.end

  private val multiplyOrDivide: P[Boolean] = multiply.as(true) | divide.as(false)
  private val addOrSubtract: P[Boolean]    = plus.as(true) | minus.as(false)

  private def term: P0[Expr] = P
    .defer0(atom ~ (multiplyOrDivide ~ atom).backtrack.rep0)
    .map { case (head, tail) =>
      tail.foldLeft(head) { case (acc, (isMultiply, expr)) =>
        if (isMultiply) Mul(acc, expr) else Div(acc, expr)
      }
    }
    .withContext("term")

  private def op: P0[Expr] = P
    .defer0(term ~ (addOrSubtract ~ term).backtrack.rep0)
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

  def parseDsl(s: String): Either[Error, Expr] =
    parseExpression(s) match {
      case right @ Right(_) => right
      case Left(err)        => parseScript(s).left.map(_ => err)
    }

  private def parseExpression(s: String): Either[Error, Expr] =
    dsl.parseAll(s.trim)

  private def parseCondition(s: String): Either[Error, Bool] =
    conditionExpr.parseAll(s.trim)

  private def parseScript(s: String): Either[Error, Expr] = {
    val lines = splitTopLevelStatements(s)

    lines match {
      case Nil         => parseExpression("")
      case line :: Nil => parseStatement(line)
      case _           =>
        lines
          .foldLeft[Either[Error, List[Expr]]](Right(Nil)) { (acc, line) =>
            acc.flatMap { statements =>
              parseStatement(line).map(_ :: statements)
            }
          }
          .map(statements => Block(statements.reverse))
    }
  }

  private def parseStatement(line: String): Either[Error, Expr] =
    line match {
      case varDeclRegex(name, rhs)  => parseDsl(rhs).map(expr => VarDecl(name, expr))
      case assignRegex(name, rhs)   => parseDsl(rhs).map(expr => Assign(name, expr))
      case appendRegex(name, value) =>
        parseDsl(value).map(v => Append(name, v))
      case removeRegex(name, index) =>
        parseDsl(index).map(i => Remove(name, i))
      case indexRegex(name, index)  =>
        parseDsl(index).map(i => ArrayIndex(name, i))
      case arrayEmptyRegex()        =>
        Right(ArrayEmpty)
      case incRegex(name)           => Right(Inc(name))
      case _                        =>
        parseExpression(line) match {
          case right @ Right(_)      => right
          case Left(expressionError) =>
            parseIfWithScriptBranches(line)
              .orElse(parseForEachLoop(line))
              .orElse(parseForLoop(line))
              .toRight(expressionError)
        }
    }

  private final case class Delimited(content: String, next: Int)

  private def splitTopLevelStatements(s: String): List[String] = {
    val statements = scala.collection.mutable.ListBuffer.empty[String]
    var start      = 0
    var i          = 0
    var parenDepth = 0
    var braceDepth = 0
    var inString   = false
    var escaped    = false

    while (i < s.length) {
      val ch = s.charAt(i)

      if (inString) {
        if (escaped) escaped = false
        else if (ch == '\\') escaped = true
        else if (ch == '\'') inString = false
      } else {
        ch match {
          case '\''                                       => inString = true
          case '('                                        => parenDepth += 1
          case ')'                                        => if (parenDepth > 0) parenDepth -= 1
          case '{'                                        => braceDepth += 1
          case '}'                                        => if (braceDepth > 0) braceDepth -= 1
          case '\n' if parenDepth == 0 && braceDepth == 0 =>
            val chunk = s.substring(start, i).trim
            if (chunk.nonEmpty) statements += chunk
            start = i + 1
          case _                                          => ()
        }
      }

      i += 1
    }

    val tail = s.substring(start).trim
    if (tail.nonEmpty) statements += tail
    statements.toList
  }

  private def parseIfWithScriptBranches(line: String): Option[Expr] = {
    val text = line.trim

    def skipWhitespaces(idx: Int): Int = {
      var i = idx
      while (i < text.length && text.charAt(i).isWhitespace) i += 1
      i
    }

    def startsWithWord(idx: Int, word: String): Boolean = {
      val end = idx + word.length
      if (end > text.length || !text.regionMatches(idx, word, 0, word.length)) false
      else {
        val hasFollowingIdentifierChar =
          end < text.length && (text.charAt(end).isLetterOrDigit || text.charAt(end) == '_')
        !hasFollowingIdentifierChar
      }
    }

    def parseDelimited(start: Int, open: Char, close: Char): Option[Delimited] = {
      if (start >= text.length || text.charAt(start) != open) None
      else {
        var i        = start + 1
        var depth    = 1
        var inString = false
        var escaped  = false

        while (i < text.length) {
          val ch = text.charAt(i)
          if (inString) {
            if (escaped) escaped = false
            else if (ch == '\\') escaped = true
            else if (ch == '\'') inString = false
          } else {
            if (ch == '\'') inString = true
            else if (ch == open) depth += 1
            else if (ch == close) {
              depth -= 1
              if (depth == 0) return Some(Delimited(text.substring(start + 1, i), i + 1))
            }
          }
          i += 1
        }

        None
      }
    }

    val ifStart = skipWhitespaces(0)
    if (!startsWithWord(ifStart, "if")) None
    else {
      val condStart = skipWhitespaces(ifStart + 2)
      parseDelimited(condStart, '(', ')').flatMap { cond =>
        val thenStart = skipWhitespaces(cond.next)
        parseDelimited(thenStart, '{', '}').flatMap { whenTrue =>
          val elseStart = skipWhitespaces(whenTrue.next)
          if (!startsWithWord(elseStart, "else")) None
          else {
            val whenFalseStart = skipWhitespaces(elseStart + 4)
            parseDelimited(whenFalseStart, '{', '}').flatMap { whenFalse =>
              if (skipWhitespaces(whenFalse.next) != text.length) None
              else
                for {
                  condition <- parseCondition(cond.content).toOption
                  trueExpr  <- parseDsl(whenTrue.content).toOption
                  falseExpr <- parseDsl(whenFalse.content).toOption
                } yield IfElse(condition, trueExpr, falseExpr)
            }
          }
        }
      }
    }
  }

  private def parseForLoop(line: String): Option[Expr] = {
    val text = line.trim

    def skipWhitespaces(idx: Int): Int = {
      var i = idx
      while (i < text.length && text.charAt(i).isWhitespace) i += 1
      i
    }

    def startsWithWord(idx: Int, word: String): Boolean = {
      val end = idx + word.length
      if (end > text.length || !text.regionMatches(idx, word, 0, word.length)) false
      else {
        val hasFollowingIdentifierChar =
          end < text.length && (text.charAt(end).isLetterOrDigit || text.charAt(end) == '_')
        !hasFollowingIdentifierChar
      }
    }

    def parseDelimited(start: Int, open: Char, close: Char): Option[Delimited] = {
      if (start >= text.length || text.charAt(start) != open) None
      else {
        var i        = start + 1
        var depth    = 1
        var inString = false
        var escaped  = false

        while (i < text.length) {
          val ch = text.charAt(i)
          if (inString) {
            if (escaped) escaped = false
            else if (ch == '\\') escaped = true
            else if (ch == '\'') inString = false
          } else {
            if (ch == '\'') inString = true
            else if (ch == open) depth += 1
            else if (ch == close) {
              depth -= 1
              if (depth == 0) return Some(Delimited(text.substring(start + 1, i), i + 1))
            }
          }
          i += 1
        }

        None
      }
    }

    def splitForHeader(header: String): Option[(String, String, String)] = {
      var i          = 0
      var inString   = false
      var escaped    = false
      var parenDepth = 0
      var braceDepth = 0
      var firstSep   = -1
      var secondSep  = -1

      while (i < header.length) {
        val ch = header.charAt(i)
        if (inString) {
          if (escaped) escaped = false
          else if (ch == '\\') escaped = true
          else if (ch == '\'') inString = false
        } else {
          ch match {
            case '\''                                                                                   => inString = true
            case '('                                                                                    => parenDepth += 1
            case ')' if parenDepth > 0                                                                  => parenDepth -= 1
            case '{'                                                                                    => braceDepth += 1
            case '}' if braceDepth > 0                                                                  => braceDepth -= 1
            case ';' if parenDepth == 0 && braceDepth == 0 && firstSep == -1                            =>
              firstSep = i
            case (';' | ',') if parenDepth == 0 && braceDepth == 0 && firstSep != -1 && secondSep == -1 =>
              secondSep = i
            case _                                                                                      => ()
          }
        }
        i += 1
      }

      if (firstSep == -1 || secondSep == -1) None
      else {
        val init   = header.substring(0, firstSep).trim
        val cond   = header.substring(firstSep + 1, secondSep).trim
        val update = header.substring(secondSep + 1).trim
        if (init.isEmpty || cond.isEmpty || update.isEmpty) None else Some((init, cond, update))
      }
    }

    val forStart = skipWhitespaces(0)
    if (!startsWithWord(forStart, "for")) None
    else {
      val headerStart = skipWhitespaces(forStart + 3)
      parseDelimited(headerStart, '(', ')').flatMap { header =>
        val bodyStart = skipWhitespaces(header.next)
        parseDelimited(bodyStart, '{', '}').flatMap { body =>
          if (skipWhitespaces(body.next) != text.length) None
          else {
            splitForHeader(header.content).flatMap { case (initRaw, conditionRaw, updateRaw) =>
              for {
                init      <- parseStatement(initRaw).toOption
                condition <- parseCondition(conditionRaw).toOption
                update    <- parseStatement(updateRaw).toOption
                bodyExpr  <- parseDsl(body.content).toOption
              } yield ForLoop(init, condition, update, bodyExpr)
            }
          }
        }
      }
    }
  }

  private def parseForEachLoop(line: String): Option[Expr] = {
    val text = line.trim

    def skipWhitespaces(idx: Int): Int = {
      var i = idx
      while (i < text.length && text.charAt(i).isWhitespace) i += 1
      i
    }

    def startsWithWord(idx: Int, word: String): Boolean = {
      val end = idx + word.length
      if (end > text.length || !text.regionMatches(idx, word, 0, word.length)) false
      else {
        val hasFollowingIdentifierChar =
          end < text.length && (text.charAt(end).isLetterOrDigit || text.charAt(end) == '_')
        !hasFollowingIdentifierChar
      }
    }

    def parseDelimited(start: Int, open: Char, close: Char): Option[Delimited] = {
      if (start >= text.length || text.charAt(start) != open) None
      else {
        var i        = start + 1
        var depth    = 1
        var inString = false
        var escaped  = false

        while (i < text.length) {
          val ch = text.charAt(i)
          if (inString) {
            if (escaped) escaped = false
            else if (ch == '\\') escaped = true
            else if (ch == '\'') inString = false
          } else {
            if (ch == '\'') inString = true
            else if (ch == open) depth += 1
            else if (ch == close) {
              depth -= 1
              if (depth == 0) return Some(Delimited(text.substring(start + 1, i), i + 1))
            }
          }
          i += 1
        }

        None
      }
    }

    val forEachHeaderRegex = s"""var\\s+($identifierRegex)\\s+in\\s+($identifierRegex)""".r

    val forStart = skipWhitespaces(0)
    if (!startsWithWord(forStart, "for")) None
    else {
      val headerStart = skipWhitespaces(forStart + 3)
      parseDelimited(headerStart, '(', ')').flatMap { header =>
        val bodyStart = skipWhitespaces(header.next)
        parseDelimited(bodyStart, '{', '}').flatMap { body =>
          if (skipWhitespaces(body.next) != text.length) None
          else {
            header.content.trim match {
              case forEachHeaderRegex(itemName, arrayName) =>
                parseDsl(body.content).toOption.map(bodyExpr => ForEachLoop(itemName, arrayName, bodyExpr))
              case _                                       => None
            }
          }
        }
      }
    }
  }
}
