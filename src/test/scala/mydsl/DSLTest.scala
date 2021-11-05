package mydsl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._
import DSL._
import Eval._

class DSLTest extends AnyWordSpec {

  "valid strings" should {
    List(
      " 3 ",
      "organization.v1",
      " 3 + 4 + 8 + a.b ",
      " (3 + 4) + (8 + a.b) ",
      "3+4+8+a.b",
      " 'http://foo.bar/baz?v1=xxx' + 'b1' + '3A.4' ",
      " if ( 2 == 3 ) { 5 + 1 + organization.identifier } else { 6 }",
      " if ( 2 != (3 + 1) ) { if('a' == null) { null } else { 5 } } else { 6 }",
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
        parseDsl(str) shouldBe Symbol("success")
      }

    }
  }

  "invalid strings" should {
    List(
      " 3d ",
      " 3 + 4 +/ 8 + a.b ",
      " 3 + 4d + a.b ",
      " if ( 2 + 3 ) { 5 + 1 + organization.identifier } else { 6 }"
    ).foreach { str =>
      s""""$str"""" in {
        parseDsl(str) should not be Symbol("success")
      }
    }
  }

  "eval" should {
    val input: Map[String, Result] = Map(
      "organization.v1"         -> Result("Google"),
      "organization.identifier" -> Result("organizationId"),
      "a.b"                     -> Result(10),
      "b1"                      -> Result("bar")
    )

    List(
      " 3 "                                                                  -> Result(3),
      "organization.v1"                                                      -> Result("Google"),
      " 3 + 4 + 8 + a.b "                                                    -> Result(25),
      " (3 + 4) + (8 + a.b) "                                                -> Result(25),
      "3+4+8+a.b"                                                            -> Result(25),
      " 'http://foo.bar/baz?v1=xxx/' + b1 + '/3A.4' "                        -> Result("http://foo.bar/baz?v1=xxx/bar/3A.4"),
      " if ( 2 == a.b ) { 5 + 1 + organization.identifier } else { 6 }"      -> Result(6),
      " if ( 10 == a.b ) { 5 + 1 + organization.identifier } else { 6 }"     -> Result("6organizationId"),
      " if ( a.b != (3 + 1) ) { if(x == null) { 4 } else { 5 } } else { 6 }" -> Result(4),
      """ if ( a.b != (3 + 1) ) {
        |   if(x == null) {
        |     4
        |   } else {
        |     5
        |   }
        | } else {
        |   6 
        | }
        |""".stripMargin                                                     -> Result(4)
    ).foreach { case (s, res) =>
      s""" "$s" == ${Result.toString(res)} """ in {
        compute(parseDsl(s).get.value, input) shouldBe res
      }
    }
  }

}
