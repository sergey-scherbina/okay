package okay.rag

import okay.codec.*

/**
 * okay-codec's and okay-rag's readme examples, COMPILED — the rule
 * TestMcpReadme set and TestReadmes (okay-security) followed: a readme
 * whose code does not compile is worse than none.
 *
 * Here rather than in okay-codec because okay-rag sees both, and the
 * dependency arrow forbids the reverse.
 */
class TestReadmes extends munit.FunSuite {

  // ── okay-codec/README.md ───────────────────────────────────────

  final case class Person(name: String, age: Int, nick: Option[String])
  given Schema[Person] = Schema.derived

  test("codec: round-tripping a value") {
    val text = Json.write(Person("Ann", 33, None))
    val back = Json.read[Person](text)
    assertEquals(back, Right(Person("Ann", 33, None)))

    assertEquals(Cbor.read[Person](Cbor.write(Person("Ann", 33, None))),
      Right(Person("Ann", 33, None)))
  }

  // ── okay-rag/README.md ─────────────────────────────────────────

  test("rag: indexing and searching, with no model in sight") {
    val files = Seq(
      Source("Greeter.scala", "class Greeter(name: String) {\n  def hello = name\n}\n"),
      Source("Http.scala", "class Http {\n  def network: String = \"requests\"\n}\n"))
    val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))

    val index = Keyword.index(segs)
    val hits = Keyword.search(index, "network requests", 3)
    val _ = hits.map(h => (h.segment.source, h.score))
    assert(hits.nonEmpty)

    // the readme's claim that the index is a monoid
    val M = summon[okay.Monoid[Postings]]
    val (l, r) = segs.splitAt(segs.length / 2)
    assertEquals(Keyword.search(M.combine(Keyword.index(l), Keyword.index(r)),
      "network requests", 3).map(_.segment.source), hits.map(_.segment.source))
  }
}
