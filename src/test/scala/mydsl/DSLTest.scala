package mydsl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._
import DSL._

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
      " if ( 2 != (3 + 1) ) { if('a' == null) { null } else { 5 } } else { 6 }"
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
}
