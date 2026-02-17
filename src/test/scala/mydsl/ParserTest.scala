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
      " 2 * 3 ",
      " 8 / 2 ",
      " 2 + 3 * 4 ",
      " (2 + 3) * 4 ",
      " (3 + 4) + (8 + a.b) ",
      "3+4+8+a.b",
      " 'http://foo.bar/baz?v1=xxx' + 'b1' + '3A.4' ",
      " substr('abcdef', 1, 3) ",
      " substr('abcdef', 2) ",
      " md5('abc') ",
      " length('abc') ",
      " abs(0 - 3) ",
      " mod(10, 3) ",
      "var arr = array[]",
      "var arr = array[]\narr.remove(0)",
      "for (var a in arr) { a }",
      "if ( 1 < 2 ) { 1 } else { 0 }",
      "for( var i = 0; i < 3, i++) { i }",
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
      " 3 */ 2 ",
      " 3 // 2 ",
      " 3. ",
      " 3 - ",
      " substr('abc') ",
      " substr('abc', 1, 1, 1) ",
      " substring('abc', 1) ",
      " md5() ",
      " length() ",
      " abs() ",
      " mod() ",
      " mod(1) ",
      " for( var i = 0; i < 10 ) { i } ",
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
      " 2 * 3 "                                                             -> Result(6),
      " 7 / 2 "                                                             -> Result(3.5),
      " 2 + 3 * 4 "                                                         -> Result(14),
      " (2 + 3) * 4 "                                                       -> Result(20),
      " 10 - 6 / 2 "                                                        -> Result(7.0),
      " 10.5 - 0.5 + 0.25 "                                                 -> Result(10.25),
      " a.c + 1.5 "                                                         -> Result(12.0),
      " 10 - (3 + 1) "                                                      -> Result(6),
      " (3 + 4) + (8 + a.b) "                                               -> Result(25),
      "3+4+8+a.b"                                                           -> Result(25),
      " 'http://foo.bar/baz?v1=xxx/' + b1 + '/3A.4' "                       -> Result("http://foo.bar/baz?v1=xxx/bar/3A.4"),
      " substr('abcdef', 1, 3) "                                            -> Result("bcd"),
      " substr('abcdef', 2) "                                               -> Result("cdef"),
      " substr('abcdef', length('abcdef') - 3, 3) "                         -> Result("def"),
      " substr('abcdef', length('abcdef') - 3) "                            -> Result("def"),
      " substr('abcdef', 4, 999) "                                          -> Result("ef"),
      " substr(organization.v1, 0, 3) "                                     -> Result("Goo"),
      " substr('abcdef', 10, 3) "                                           -> Result(""),
      " md5('abc') "                                                        -> Result("900150983cd24fb0d6963f7d28e17f72"),
      " md5(10) "                                                           -> Result("d3d9446802a44259755d38e6d163e820"),
      " length('abc') "                                                     -> Result(3),
      " length(organization.v1) "                                           -> Result(6),
      " abs(0 - 3) "                                                        -> Result(3),
      " abs(3.5 - 10) "                                                     -> Result(6.5),
      " mod(10, 3) "                                                        -> Result(1),
      " mod(10.5, 3) "                                                      -> Result(1.5),
      " 'v=' + 3.5 "                                                        -> Result("v=3.5"),
      "if ( 1 < 2 ) { 1 } else { 0 }"                                       -> Result(1),
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
      List("true1", "false_flag", "nullValue", "ifx", "elsey", "md5", "substr", "length", "abs", "mod").map(
        parseDsl(_).value
      ) shouldBe
        List(
          Add(Param("true1"), Nil),
          Add(Param("false_flag"), Nil),
          Add(Param("nullValue"), Nil),
          Add(Param("ifx"), Nil),
          Add(Param("elsey"), Nil),
          Add(Param("md5"), Nil),
          Add(Param("substr"), Nil),
          Add(Param("length"), Nil),
          Add(Param("abs"), Nil),
          Add(Param("mod"), Nil)
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

    "parse newline-separated var declaration and increment" in {
      parseDsl(
        """var i = 0
          |i++
          |""".stripMargin
      ).value shouldBe Block(List(VarDecl("i", Add(IntNum(0), Nil)), Inc("i")))
    }

    "reject val declaration in program mode" in {
      parseDsl("val i = 0").isLeft shouldBe true
    }

    "reject semicolon-separated statements in program mode" in {
      parseDsl("var i = 0; i++").isLeft shouldBe true
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
      ex.getMessage shouldBe "Only numbers are allowed in arithmetic operations"
    }

    "throw for addition involving null" in {
      an[MatchError] should be thrownBy {
        compute(Map.empty)(parseDsl("null + 1").value)
      }
    }

    "throw for multiplication with non-numeric operands" in {
      val multiplyEx = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("2 * true").value)
      }
      multiplyEx.getMessage shouldBe "Only numbers are allowed in arithmetic operations"
    }

    "throw for division with non-numeric operands" in {
      val divideEx = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("'x' / 2").value)
      }
      divideEx.getMessage shouldBe "Only numbers are allowed in arithmetic operations"
    }

    "throw for substr with non-string first argument" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("substr(1, 0, 1)").value)
      }
      ex.getMessage shouldBe "substr expects string argument"
    }

    "throw for substr with non-integer start/length arguments" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("substr('abc', 1.5, 1)").value)
      }
      ex.getMessage shouldBe "substr expects integer arguments"
    }

    "throw for substr with negative start or length" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("substr('abc', 0 - 1, 1)").value)
      }
      ex.getMessage shouldBe "substr expects non-negative start and length"
    }

    "throw for length with non-string first argument" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("length(1)").value)
      }
      ex.getMessage shouldBe "length expects string or array argument"
    }

    "throw for abs with non-numeric argument" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("abs('abc')").value)
      }
      ex.getMessage shouldBe "Only numbers are allowed in arithmetic operations"
    }

    "throw for mod with non-numeric argument" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("mod('abc', 2)").value)
      }
      ex.getMessage shouldBe "Only numbers are allowed in arithmetic operations"
    }

    "keep ints for int+int and promote to double for mixed addition" in {
      val intPlusInt = compute(Map.empty)(parseDsl("2 + 3").value)
      intPlusInt match {
        case Left(Right(IntResult(5))) => ()
        case other                     => fail(s"Expected IntResult(5), got: $other")
      }

      List(
        "3 + 2.5"   -> 5.5,
        "3.5 + 2"   -> 5.5,
        "2 + 3.0"   -> 5.0,
        "2.5 + 3.5" -> 6.0
      ).foreach { case (expr, expected) =>
        val result = compute(Map.empty)(parseDsl(expr).value)
        result match {
          case Left(Right(DoubleResult(v))) =>
            if (v != expected) fail(s"Unexpected result for '$expr': expected $expected, got $v")
          case other                        => fail(s"Expected DoubleResult for '$expr', got: $other")
        }
      }
    }

    "evaluate var declaration followed by increment" in {
      compute(Map.empty)(parseDsl("var i = 0\ni++").value) shouldBe Result(1)
    }

    "evaluate incremented value in later expression" in {
      compute(Map.empty)(parseDsl("var i = 0\ni++\ni").value) shouldBe Result(1)
    }

    "throw for incrementing undefined variable" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("i++").value)
      }
      ex.getMessage shouldBe "Cannot increment undefined variable 'i'"
    }

    "throw for incrementing non-integer variable" in {
      val ex = the[IllegalArgumentException] thrownBy {
        compute(Map.empty)(parseDsl("var s = 'x'\ns++").value)
      }
      ex.getMessage shouldBe "Increment expects integer variable 's'"
    }

    "use vars in expressions with strings, booleans and doubles" in {
      val program =
        """var s = 'hello'
          |var b = true
          |var d = 2.5
          |var i = 2
          |if ( b == true ) { s + '-' + (d + i) } else { 'nope' }
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result("hello-4.5")
    }

    "support vars inside if branches and keep them scoped" in {
      val program =
        """if ( true ) {
          |  var a = 1
          |  a++
          |} else {
          |  var a = 2
          |  a++
          |}
          |a + 3
          |""".stripMargin

      an[MatchError] should be thrownBy {
        compute(Map.empty)(parseDsl(program).value)
      }
    }

    "evaluate scoped vars in if branches on happy path" in {
      val program =
        """if ( true ) {
          |  var a = 1
          |  a++
          |} else {
          |  var a = 2
          |  a++
          |}
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(2)
    }

    "evaluate for loop happy path" in {
      compute(Map.empty)(parseDsl("for( var i = 0; i < 10, i++) { i }").value) shouldBe Result(9)
    }

    "evaluate for loop with multiline body script" in {
      val program =
        """for( var i = 0; i < 3, i++) {
          |  var a = i
          |  a++
          |}
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(3)
    }

    "sum numbers in a for loop using assignment" in {
      val program =
        """var sum = 0
          |for (var i=0; i < 10; i++) {
          |   sum = sum + i
          |}
          |sum
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(45)
    }

    "support array append and length" in {
      val program =
        """var arr = array[]
          |arr.append(1)
          |arr.append(2)
          |length(arr)
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(2)
    }

    "support array indexing in variable assignment" in {
      val program =
        """var arr = array[]
          |arr.append('x')
          |arr.append('y')
          |var x = arr[1]
          |x
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result("y")
    }

    "support array remove by index" in {
      val program =
        """var arr = array[]
          |arr.append('a')
          |arr.append('b')
          |arr.append('c')
          |arr.remove(1)
          |var x = arr[1]
          |x
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result("c")
    }

    "return array literal as expression value" in {
      compute(Map.empty)(parseDsl("array[]").value) shouldBe Result(Seq.empty)
    }

    "return populated array variable as expression value" in {
      val program =
        """var arr = array[]
          |arr.append(1)
          |arr.append(2)
          |arr
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(Seq(Result(1), Result(2)))
    }

    "iterate arrays with for-in loop" in {
      val program =
        """var arr = array[]
          |arr.append(1)
          |arr.append(2)
          |arr.append(3)
          |var sum = 0
          |for (var a in arr) {
          |  sum = sum + a
          |}
          |sum
          |""".stripMargin

      compute(Map.empty)(parseDsl(program).value) shouldBe Result(6)
    }

    "keep loop variable scoped to the for loop" in {
      val program =
        """for( var i = 0; i < 2, i++) { i }
          |i + 3
          |""".stripMargin

      an[MatchError] should be thrownBy {
        compute(Map.empty)(parseDsl(program).value)
      }
    }
  }

}
