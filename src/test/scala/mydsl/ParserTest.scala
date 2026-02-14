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
      " 'Organization_V1' ",
      " 'it\\'s' ",
      " 3 + 4 + 8 + a.b ",
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
      "b1"                      -> Result("bar"),
      "is_enabled"              -> Result(true)
    )

    List(
      " 3 "                                                                 -> Result(3),
      "true"                                                                -> Result(true),
      "false"                                                               -> Result(false),
      "organization.v1"                                                     -> Result("Google"),
      " 'it\\'s' "                                                          -> Result("it's"),
      " 3 + 4 + 8 + a.b "                                                   -> Result(25),
      " 10 - 3 + a.b "                                                      -> Result(17),
      " 10 - (3 + 1) "                                                      -> Result(6),
      " (3 + 4) + (8 + a.b) "                                               -> Result(25),
      "3+4+8+a.b"                                                           -> Result(25),
      " 'http://foo.bar/baz?v1=xxx/' + b1 + '/3A.4' "                       -> Result("http://foo.bar/baz?v1=xxx/bar/3A.4"),
      "if ( true ) { 1 } else { 0 }"                                        -> Result(1),
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

}
