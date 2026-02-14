package mydsl

object Cli {

  private val usage: String =
    """Usage:
      |  sbt "run \"<expression>\" [key=value ...]"
      |
      |Examples:
      |  sbt "run \"3 + 4 + a.b\" a.b=10"
      |  sbt "run \"if ( is_enabled == true ) { 1 } else { 0 }\" is_enabled=true"
      |  sbt "run \"organization.v1 + suffix\" organization.v1=hello suffix=world"
      |
      |Binding value rules:
      |  - Numbers: 42, 3.14
      |  - Booleans: true, false
      |  - Null: null
      |  - Strings: anything else (optionally quoted with ' or ")
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val normalizedArgs = if (args.headOption.contains("--")) args.drop(1) else args

    if (normalizedArgs.isEmpty || normalizedArgs.contains("-h") || normalizedArgs.contains("--help")) {
      println(usage)
      ()
    } else {
      val expression = normalizedArgs.head

      parseBindings(normalizedArgs.drop(1).toList) match {
        case Left(error)     =>
          Console.err.println(error)
          sys.exit(1)
        case Right(bindings) =>
          Parser.parseDsl(expression) match {
            case Left(error)   =>
              Console.err.println(s"Parse error: $error")
              sys.exit(1)
            case Right(parsed) =>
              val result = Eval.compute(bindings)(parsed)
              println(Eval.Result.toString(result))
          }
      }
    }
  }

  private def parseBindings(rawBindings: List[String]): Either[String, Map[String, Eval.Result]] =
    rawBindings.foldLeft[Either[String, Map[String, Eval.Result]]](Right(Map.empty)) { (acc, raw) =>
      acc.flatMap { m =>
        parseBinding(raw).map { case (key, valueToken) =>
          m.updated(key, parseValue(valueToken))
        }
      }
    }

  private def parseBinding(raw: String): Either[String, (String, String)] =
    raw.split("=", 2).toList match {
      case key :: value :: Nil if key.nonEmpty => Right((key, value))
      case _                                   => Left(s"Invalid binding '$raw'. Expected key=value.")
    }

  private def parseValue(raw: String): Eval.Result =
    raw match {
      case "null"  => null
      case "true"  => Eval.Result(true)
      case "false" => Eval.Result(false)
      case _       =>
        parseInt(raw)
          .map(Eval.Result(_))
          .orElse(parseDouble(raw).map(Eval.Result(_)))
          .getOrElse(Eval.Result(unquote(raw)))
    }

  private def parseInt(raw: String): Option[Int] =
    try {
      Some(raw.toInt)
    } catch {
      case _: NumberFormatException => None
    }

  private def parseDouble(raw: String): Option[Double] =
    try {
      Some(raw.toDouble)
    } catch {
      case _: NumberFormatException => None
    }

  private def unquote(raw: String): String = {
    val isSingleQuoted = raw.startsWith("'") && raw.endsWith("'") && raw.length >= 2
    val isDoubleQuoted = raw.startsWith("\"") && raw.endsWith("\"") && raw.length >= 2
    if (isSingleQuoted || isDoubleQuoted) raw.substring(1, raw.length - 1) else raw
  }
}
