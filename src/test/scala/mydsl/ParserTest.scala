package mydsl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._
import org.scalatest.EitherValues._
import Parser._
import Eval._

class ParserTest extends AnyWordSpec {

  "valid strings" should {
    List(
      " 3 ",
      "organization.V1",
      "_Organization_V1",
      "true",
      "false",
      "3.14",
      " 'Organization_V1' ",
      " 'it\\'s' ",
      " 3 + 4 + 8 + a.b ",
      " 3.5 + 4.25 ",
      " 3 - 1 + a.b ",
      " (3 + 4) + (8 + a.b) ",
      "3+4+8+a.b",
      " 'http://foo.bar/baz?v1=xxx' + 'b1' + '3A.4' ",
      "if ( true ) { 1 } else { 0 }",
      "if ( 2 == 3 ) { 5 + 1 + organization.identifier } else { 6 }",
      "if ( 2 != (3 + 1) ) { if('a' == null) { null } else { 5 } } else { 6 }",
      """ if ( a.b != (3 + 1) ) {
        |   if(x == null) {
        |     4
        |   } else {
        |     5
        |   }
        | } else {
        |   6
        | }
        |""".stripMargin
    ).foreach { str =>
      s""""$str"""" in {
        parseDsl(str) shouldBe Symbol("right")
      }

    }
  }

  "invalid strings" should {
    List(
      " 3d ",
      " 3 + 4 +/ 8 + a.b ",
      " 3 + 4d + a.b ",
      " 3. ",
      " 3 - ",
      " if ( trues ) { 1 } else { 0 }",
      " if ( 2 + 3 ) { 5 + 1 + organization.identifier } else { 6 }"
    ).foreach { str =>
      s""""$str"""" in {
        parseDsl(str) should not be Symbol("right")
      }
    }
  }

  "eval" should {
    val input: Map[String, Result] = Map(
      "organization.v1"         -> Result("Google"),
      " 'foo/bar' "             -> Result("foo/bar"),
      "organization.identifier" -> Result("organizationId"),
      "a.b"                     -> Result(10),
      "a.c"                     -> Result(10.5),
      "b1"                      -> Result("bar"),
      "is_enabled"              -> Result(true)
    )

    List(
      " 3 "                                                                 -> Result(3),
      "true"                                                                -> Result(true),
      "false"                                                               -> Result(false),
      "3.14"                                                                -> Result(3.14),
      "organization.v1"                                                     -> Result("Google"),
      " 'it\\'s' "                                                          -> Result("it's"),
      " 3 + 4 + 8 + a.b "                                                   -> Result(25),
      " 3.5 + 4.25 "                                                        -> Result(7.75),
      " 10 - 3 + a.b "                                                      -> Result(17),
      " 10.5 - 0.5 + 0.25 "                                                 -> Result(10.25),
      " a.c + 1.5 "                                                         -> Result(12.0),
      " 10 - (3 + 1) "                                                      -> Result(6),
      " (3 + 4) + (8 + a.b) "                                               -> Result(25),
      "3+4+8+a.b"                                                           -> Result(25),
      " 'http://foo.bar/baz?v1=xxx/' + b1 + '/3A.4' "                       -> Result("http://foo.bar/baz?v1=xxx/bar/3A.4"),
      " 'v=' + 3.5 "                                                        -> Result("v=3.5"),
      "if ( true ) { 1 } else { 0 }"                                        -> Result(1),
      "if ( 1.5 == 1.5 ) { 1 } else { 0 }"                                  -> Result(1),
      "if ( false ) { 1 } else { 0 }"                                       -> Result(0),
      "if ( is_enabled == true ) { 1 } else { 0 }"                          -> Result(1),
      "if ( 2 == a.b ) { 5 + 1 + organization.identifier } else { 6 }"      -> Result(6),
      "if ( 2 == a.b ) { 5 + 1 + organization.identifier } else { null }"   -> null,
      "if ( 10 == a.b ) { 5 + 1 + organization.identifier } else { 6 }"     -> Result("6organizationId"),
      "if ( a.b != null ) { a.b } else { 0 }"                               -> Result(10),
      "if ( a.b == null ) { 0 } else { a.b }"                               -> Result(10),
      "if ( a.b.c != null ) { a.b.c } else { 0 }"                           -> Result(0),
      "if ( a.b != (3 + 1) ) { if(x == null) { 4 } else { 5 } } else { 6 }" -> Result(4),
      """if ( a.b != (3 + 1) ) {
        |   if(x == null) {
        |     4
        |   } else {
        |     5
        |   }
        | } else {
        |   6 
        | }
        |""".stripMargin                                                    -> Result(4)
    ).foreach { case (s, res) =>
      s""" "$s" == ${Result.toString(res)} """ in {
        compute(input)(parseDsl(s).value) shouldBe res
      }
    }
  }

  "parser edge cases" should {
    "parse keyword-prefixed identifiers as params" in {
      List("true1", "false_flag", "nullValue", "ifx", "elsey").map(parseDsl(_).value) shouldBe
        List(
          Add(Param("true1"), Nil),
          Add(Param("false_flag"), Nil),
          Add(Param("nullValue"), Nil),
          Add(Param("ifx"), Nil),
          Add(Param("elsey"), Nil)
        )
    }

    "reject malformed expressions" in {
      List(
        "1 +",
        "1 - - 2",
        "if (true) { 1 }",
        "if ( true ) { 1 } else { 0 } trailing",
        "'unterminated"
      ).forall(parseDsl(_).isLeft) shouldBe true
    }
  }

  "eval edge cases" should {
    "decode escaped control characters" in {
      compute(Map.empty)(parseDsl("'a\\nb\\tend'").value) shouldBe Result("a\nb\tend")
    }

    "preserve unknown escape sequences as literal backslash + char" in {
      compute(Map.empty)(parseDsl("'a\\qb'").value) shouldBe Result("a\\qb")
    }

    "throw for subtraction with non-integers" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("3 - true").value)
      }
      ex.getMessage shouldBe "Only numbers can be negated"
    }

    "throw for addition involving null" in {
      the[MatchError] thrownBy {
        compute(Map.empty)(parseDsl("null + 1").value)
      }
    }

    "return doubles for mixed integer/double arithmetic" in {
      val numericResults = List(
        "3 + 2.5"   -> 5.5,
        "3.5 + 2"   -> 5.5,
        "10 - 2.5"  -> 7.5,
        "10.5 - 2"  -> 8.5,
        "2 + 3.0"   -> 5.0,
        "10.0 - 2"  -> 8.0
      ).map { case (expr, expected) =>
        val result = compute(Map.empty)(parseDsl(expr).value)
        result match {
          case Left(Right(n)) =>
            if (n != expected) fail(s"Unexpected numeric result for '$expr': expected $expected, got $n")
            n
          case other          => fail(s"Expected numeric result for '$expr', got: $other")
        }
      }

      numericResults.forall(_.isInstanceOf[Double]) shouldBe true
    }
  }

}
