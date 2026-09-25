package okay2.codec

final case class IntBox(n: Int)
final case class LongBox(n: Long)

/** A wire integer that does not fit the field is REFUSED at every door
 * rather than truncated (okay-codec's TestIntRange, its two ported
 * doors: the fold decoder and the strict reader). */
class TestIntRange extends munit.FunSuite {

  private def doors(text: String): List[(String, Either[String, Int])] = List(
    "decode" -> Json.read[IntBox](text).map(_.n),
    "strict" -> JsonStrict.read[IntBox](text).map(_.n))

  test("an Int field refuses a JSON integer past Int at every door") {
    for (text <- List("""{"n":3000000000}""", """{"n":-2147483649}"""))
      doors(text).foreach { case (door, r) => assert(r.isLeft, s"$door read $text as $r") }
  }

  test("an Int field refuses a fraction at every door, rather than truncating it") {
    doors("""{"n":1.5}""").foreach { case (door, r) => assert(r.isLeft, s"$door read 1.5 as $r") }
  }

  test("an Int field still reads its own extremes at every door") {
    for (v <- List(Int.MaxValue, Int.MinValue, 0))
      doors(s"""{"n":$v}""").foreach { case (door, r) => assertEquals(r, Right(v), door) }
  }

  test("a Long field refuses a fraction at every door") {
    val text = """{"n":2.5}"""
    assert(Json.read[LongBox](text).isLeft)
    assert(JsonStrict.read[LongBox](text).isLeft)
  }
}
