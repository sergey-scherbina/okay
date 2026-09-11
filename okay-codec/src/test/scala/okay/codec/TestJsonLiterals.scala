package okay.codec

import scala.compiletime.testing.{typeChecks, typeCheckErrors}

/**
 * The opt-in literal API, from both sides (json-literals). Every
 * assertion here is about what the COMPILER does, because that is
 * where the design lives: a literal converts, a `String` value is
 * refused, and nothing at all happens without the import.
 */
class TestJsonLiterals extends munit.FunSuite {

  import Json.*

  test("under the import, a string LITERAL converts, and so do Int, Double and Boolean") {
    import Json.literals.given
    def take(j: Json): Json = j
    assertEquals(take("ada"), JStr("ada"))
    assertEquals(take(36), JNum(36.0))
    assertEquals(take(1.5), JNum(1.5))
    assertEquals(take(true), JBool(true))
  }

  test("a String VALUE is refused, and the message names both ways out") {
    val errors = typeCheckErrors(
      """import okay.codec.Json
         import okay.codec.Json.literals.given
         val doc: String = "{\"a\":1}"
         val j: Json = doc""")
    assert(errors.nonEmpty, "a String value must not convert")
    val message = errors.map(_.message).mkString(" ")
    assert(message.contains("JStr"), s"the message must name JStr: $message")
    assert(message.contains("Json.parse"), s"the message must name Json.parse: $message")
  }

  test("a val holding a literal is refused too — a singleton type is not a constant") {
    // the refuted step, kept as a test: `L <: String & Singleton` alone
    // accepts this, because `s.type` IS a singleton. constValueOpt is
    // what separates a constant type from a reference to one.
    //
    // PAIRED, because a `false` from typeChecks says only "no", not
    // "no, for the reason I meant": the two snippets differ by one
    // word, so the refusal is the val and nothing else.
    val head =
      """import okay.codec.Json
         import okay.codec.Json.literals.given
         val s = "ada"
         """
    assert(typeChecks(head + """val j: Json = "ada""""), "the literal itself must convert")
    assert(!typeChecks(head + """val j: Json = s"""), "a val must not")
  }

  test("without the import nothing converts: the same literal is a plain type error") {
    // paired again: the only difference between the two is the import
    val withImport = """import okay.codec.Json
                        import okay.codec.Json.literals.given
                        """
    val without = """import okay.codec.Json
                     """
    assert(typeChecks(withImport + """val j: Json = "ada""""))
    assert(!typeChecks(without + """val j: Json = "ada""""))
    assert(typeChecks(withImport + """val j: Json = 36"""))
    assert(!typeChecks(without + """val j: Json = 36"""))
  }

  test("Long does not convert: JNum is a Double and the loss would be silent") {
    // the Int beside it converts, so the refusal is the type and not
    // the snippet
    val head = """import okay.codec.Json
                  import okay.codec.Json.literals.given
                  """
    assert(typeChecks(head + """val j: Json = 36"""), "an Int converts")
    assert(!typeChecks(head + """val j: Json = 9007199254740993L"""), "a Long does not")
  }

  test("=== compares a Json with what the import converts; == does not compile either way") {
    import Json.literals.{given, *}
    assert(JStr("x") === "x")
    assert(!(JStr("x") === "y"))
    // and the comparison this API was suspected of breaking is refused
    // by the compiler itself, with or without the import: Scala 3
    // derives CanEqual for enums
    assert(!typeChecks(
      """import okay.codec.Json
         import okay.codec.Json.literals.given
         val b: Boolean = Json.JStr("x") == "x""""))
  }
}
