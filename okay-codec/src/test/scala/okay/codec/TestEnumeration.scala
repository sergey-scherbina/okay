package okay.codec

/**
 * `Schema.enumeration` (codec-jsonschema-refinement-enum): a refinement
 * over a finite vocabulary that every wire treats as `refine` and a
 * JSON Schema declares as `enum` — so a `response_format` contract or a
 * tool declaration carries the words a prompt used to have to state.
 */
class TestEnumeration extends munit.FunSuite {

  enum Colour:
    case Red, Green, Blue
  given colourSchema: Schema[Colour] = Schema.enumeration[Colour, String](Colour.values.toVector, _.toString.toLowerCase)

  final case class Paint(colour: Colour, litres: Int) derives Schema

  test("on the wire it is the refinement: the name goes out, the name comes back, an unknown name is a decode error naming the vocabulary") {
    assertEquals(Json.write(Colour.Green), "\"green\"")
    assertEquals(Json.read[Colour]("\"blue\""), Right(Colour.Blue))
    assertEquals(Cbor.read[Colour](Cbor.write(Colour.Red)), Right(Colour.Red))
    assertEquals(Json.read[Paint]("""{"colour":"red","litres":2}"""), Right(Paint(Colour.Red, 2)))
    Json.read[Colour]("\"puce\"") match
      case Left(e) => assert(e.contains("puce") && e.contains("red, green, blue"), e)
      case Right(v) => fail(s"decoded $v")
  }

  test("in a JSON Schema it is an enum beside the underlying type; a plain refine stays a plain string") {
    val enumerated = Json.print(JsonSchema.of(colourSchema))
    assert(enumerated.contains("\"type\":\"string\"") && enumerated.contains("\"enum\":[\"red\",\"green\",\"blue\"]"), enumerated)
    val inProduct = Json.print(JsonSchema.of(summon[Schema[Paint]]))
    assert(inProduct.contains("\"enum\":[\"red\",\"green\",\"blue\"]"), inProduct)
    val plain: Schema[Colour] = Schema.refine[Colour, String](
      s => Colour.values.find(_.toString.equalsIgnoreCase(s)).toRight(s"unknown '$s'"), _.toString.toLowerCase)
    val refined = Json.print(JsonSchema.of(plain))
    assert(refined.contains("\"type\":\"string\"") && !refined.contains("enum"), refined)
  }

  test("the vocabulary can be any underlying type: an enumeration over integers declares integer values") {
    enum Level:
      case One, Two
    given Schema[Level] = Schema.enumeration[Level, Int](Level.values.toVector, _.ordinal + 1)
    assertEquals(Json.write(Level.Two), "2")
    assertEquals(Json.read[Level]("1"), Right(Level.One))
    val s = Json.print(JsonSchema.of(summon[Schema[Level]]))
    assert(s.contains("\"type\":\"integer\"") && s.contains("\"enum\":[1,2]"), s)
  }
}
