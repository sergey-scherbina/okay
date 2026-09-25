package okay2.codec

/** The opt-in literal API from both sides (okay-codec's
 * TestJsonLiterals): a literal converts, a String VALUE is refused with
 * a message naming both ways out, nothing converts without the import.
 * Scala 3 asks `typeCheckErrors`; munit's `compileErrors` is the Scala 2
 * door (hard errors only, which is all these are). */
class TestJsonLiterals extends munit.FunSuite {

  import Json._

  test("under the import, a string LITERAL converts, and so do Int, Double and Boolean") {
    import Json.literals._
    def take(j: Json): Json = j
    assertEquals(take("ada"), JStr("ada"))
    assertEquals(take(36), JNum(36.0))
    assertEquals(take(1.5), JNum(1.5))
    assertEquals(take(true), JBool(true))
  }

  test("a String VALUE is refused, and the message names both ways out") {
    val errors = compileErrors(
      """import okay2.codec.Json.literals._
         val doc: String = "{\"a\":1}"
         val j: Json = doc""")
    assert(errors.nonEmpty, "a String value must not convert")
    assert(errors.contains("JStr"), s"the message must name JStr: $errors")
    assert(errors.contains("Json.parse"), s"the message must name Json.parse: $errors")
  }

  test("a val holding a literal is refused too; the literal itself converts (paired)") {
    assertEquals(compileErrors(
      """import okay2.codec.Json.literals._
         val j: Json = "ada""""), "")
    assert(compileErrors(
      """import okay2.codec.Json.literals._
         val s = "ada"
         val j: Json = s""").nonEmpty, "a val must not")
  }

  test("without the import nothing converts (paired)") {
    assertEquals(compileErrors("""import okay2.codec.Json.literals._
                                  val j: Json = 36"""), "")
    assert(compileErrors("""val j: Json = 36""").nonEmpty)
    assert(compileErrors("""val j: Json = "ada"""").nonEmpty)
  }

  test("Long does not convert: JNum is a Double and the loss would be silent") {
    assert(compileErrors("""import okay2.codec.Json.literals._
                            val j: Json = 9007199254740993L""").nonEmpty, "a Long does not")
    assert(compileErrors("""import okay2.codec.Json.literals._
                            val j: Json = 'c'""").nonEmpty, "a Char does not")
    assert(compileErrors("""import okay2.codec.Json.literals._
                            val j: Json = 1.5f""").nonEmpty, "a Float does not")
  }

  test("=== compares a Json with what the import converts") {
    import Json.literals._
    assert(JStr("x") === "x")
    assert(!(JStr("x") === "y"))
  }
}
