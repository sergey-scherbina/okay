package okay.staging

import okay.codec.*

/**
 * sint-decode-truncates, the run-time staged doors: generated code
 * refuses an integer that does not fit its field, as the fold does
 * (okay-codec's TestIntRange covers the fold and compile-time doors).
 */
class TestRuntimeStagedIntRange extends munit.FunSuite:

  final case class Box(n: Int) derives Schema
  final case class LBox(n: Long) derives Schema

  test("run-time staged JSON, strict and CBOR refuse what an Int cannot hold") {
    val s = summon[Schema[Box]]
    val json = RuntimeStaged.json(s)
    val strict = RuntimeStaged.strict(s)
    val cbor = RuntimeStaged.cbor(s)
    assert(RuntimeStaged.isStaged(s), s"not staged: ${RuntimeStaged.lastFailure.map(_._2.toString)}")
    for text <- List("""{"n":3000000000}""", """{"n":1.5}""") do
      val j = json.decode(Json.parse(text)); val t = strict.decode(text)
      assert(j.isLeft, s"json read $text as $j"); assert(t.isLeft, s"strict read $text as $t")
    val c = cbor.decode(Cbor.write(LBox(1L << 32)))
    assert(c.isLeft, s"cbor read 2^32 as $c")
    assertEquals(json.decode(Json.parse(s"""{"n":${Int.MinValue}}""")), Right(Box(Int.MinValue)))
  }

  test("run-time staged JSON and strict refuse a fraction into a Long") {
    val s = summon[Schema[LBox]]
    val j = RuntimeStaged.json(s).decode(Json.parse("""{"n":2.5}"""))
    val t = RuntimeStaged.strict(s).decode("""{"n":2.5}""")
    assert(j.isLeft, s"$j"); assert(t.isLeft, s"$t")
  }
