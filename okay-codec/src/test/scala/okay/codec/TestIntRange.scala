package okay.codec

/**
 * sint-decode-truncates (specs/codecs.md "Integers that do not fit"):
 * a wire integer that does not fit the field is REFUSED at every door —
 * the fold decoders, Validate, and the three compile-time staged
 * codecs — where every one of them used to `.toInt` it: 3000000000
 * read as 2147483647 from JSON, 2^32 as 0 from CBOR, 1.5 as 1.
 */
class TestIntRange extends munit.FunSuite:

  final case class Box(n: Int)
  given Schema[Box] = Schema.derived
  final case class LBox(n: Long)
  given Schema[LBox] = Schema.derived

  private val sj = Staged.json[Box]
  private val sc = Staged.cbor[Box]
  private val ss = Staged.strict[Box]
  private val lj = Staged.json[LBox]
  private val ls = Staged.strict[LBox]

  /** every JSON door, one answer each */
  private def jsonDoors(text: String): List[(String, Either[?, Int])] = List(
    "decode" -> Json.read[Box](text).map(_.n),
    "strict" -> JsonStrict.read[Box](text).map(_.n),
    "validate" -> Validate.decode(summon[Schema[Box]])(Json.parse(text)).map(_.n),
    "staged" -> sj.decode(Json.parse(text)).map(_.n),
    "stagedStrict" -> ss.decode(text).map(_.n))

  test("an Int field refuses a JSON integer past Int at every door") {
    for text <- List("""{"n":3000000000}""", """{"n":-2147483649}""") do
      jsonDoors(text).foreach((door, r) => assert(r.isLeft, s"$door read $text as $r"))
  }

  test("an Int field refuses a fraction at every door, rather than truncating it") {
    jsonDoors("""{"n":1.5}""").foreach((door, r) => assert(r.isLeft, s"$door read 1.5 as $r"))
  }

  test("an Int field still reads its own extremes at every door") {
    for v <- List(Int.MaxValue, Int.MinValue, 0) do
      jsonDoors(s"""{"n":$v}""").foreach((door, r) => assertEquals(r, Right(v), door))
  }

  test("CBOR: an Int field refuses 2^32 (it read as 0), fold and staged alike") {
    val bytes = Cbor.write(LBox(1L << 32))
    val fold = Cbor.read[Box](bytes)
    val staged = sc.decode(bytes)
    assert(fold.isLeft, s"fold: $fold"); assert(staged.isLeft, s"staged: $staged")
    assertEquals(Cbor.read[Box](Cbor.write(Box(Int.MinValue))), Right(Box(Int.MinValue)))
    assertEquals(sc.decode(Cbor.write(Box(Int.MaxValue))), Right(Box(Int.MaxValue)))
  }

  test("a Long field refuses a fraction at every JSON door") {
    val text = """{"n":2.5}"""
    List("decode" -> Json.read[LBox](text), "strict" -> JsonStrict.read[LBox](text),
      "validate" -> Validate.decode(summon[Schema[LBox]])(Json.parse(text)),
      "staged" -> lj.decode(Json.parse(text)), "stagedStrict" -> ls.decode(text))
      .foreach((door, r) => assert(r.isLeft, s"$door read 2.5 as $r"))
  }
