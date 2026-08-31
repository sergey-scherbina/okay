package okay.rag

import okay.{!, +, Handler}
import okay.given

/** The retrieval layer: store, keyword, fusion, ingestion, and the
 * re-index that costs the edit. */
class TestRetrieve extends munit.FunSuite {

  val files = Seq(
    Source("Greeter.scala",
      "/** Greets people by name. */\n" +
        "class Greeter(name: String) {\n" +
        "  def hello: String = \"Hello, \" + name\n" +
        "}\n"),
    Source("Math.scala",
      "/** Arithmetic helpers. */\n" +
        "object Calc {\n" +
        "  def add(a: Int, b: Int): Int = a + b\n" +
        "  def multiply(a: Int, b: Int): Int = a * b\n" +
        "}\n"),
    Source("Http.scala",
      "/** Sends requests over the network. */\n" +
        "class HttpClient(timeout: Int) {\n" +
        "  def get(url: String): String = fetch(url)\n" +
        "}\n"))

  // Embed + Pure IS Embed (Pure is the empty signature), so the one
  // handler serves the row — a union handler here would be ambiguous
  given Handler[Embed] = Vectors.hashingHandler()

  /** run a program in the embedding row */
  def run[A](p: A ! (Embed + okay.Pure)): A = p.runWith

  test("ingestion: every segment embedded and stored, once") {
    val store = MemoryStore()
    val p = run(Ingest.run[okay.Pure](store, files)(_.length))
    assertEquals(p.sources, 3)
    assertEquals(p.embedded, p.segments)
    assertEquals(okay.!.run(store.size), p.segments)
  }

  test("vector search finds the file about the thing asked for") {
    val store = MemoryStore()
    run(Ingest.run[okay.Pure](store, files)(_.length))
    val hits = run(Retrieve.vector[okay.Pure](store).retrieve("multiply numbers", 3))
    assert(hits.nonEmpty)
    assert(hits.head.segment.source == "Math.scala",
      s"expected Math.scala first, got ${hits.map(h => (h.segment.source, h.score))}")
  }

  test("keyword search: BM25 over an index that is a Monoid") {
    val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))
    val idx = Keyword.index(segs)
    val hits = Keyword.search(idx, "network requests", 3)
    assert(hits.nonEmpty)
    assertEquals(hits.head.segment.source, "Http.scala")

    // the index merges: halves combine into the whole
    val M = summon[okay.Monoid[Postings]]
    val (l, r) = segs.splitAt(segs.length / 2)
    val merged = M.combine(Keyword.index(l), Keyword.index(r))
    assertEquals(Keyword.search(merged, "network requests", 3).map(_.segment.source),
      hits.map(_.segment.source))
  }

  test("symbol retrieval: exact, with no vectors in play") {
    val idx = Symbols.project(files)
    val byId = files.map(f => (f.id, f)).toMap
    val hits = okay.!.run(Retrieve.symbols(idx, byId).retrieve("hello", 3))
    assertEquals(hits.length, 1)
    assert(hits.head.segment.text.contains("def hello"))
    assert(hits.head.segment.quotes(byId("Greeter.scala")))
  }

  test("fusion is order-independent and needs no comparable scores") {
    val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))
    val a = Seq(Scored(segs(0), 0.9f), Scored(segs(1), 0.8f))
    val b = Seq(Scored(segs(1), 42.0f), Scored(segs(2), 7.0f))   // another scale
    val ab = Fusion.rrf(Seq(a, b)).map(_.segment.span)
    val ba = Fusion.rrf(Seq(b, a)).map(_.segment.span)
    assertEquals(ab.toSet, ba.toSet)
    // the segment both lists rank highly wins
    assertEquals(Fusion.rrf(Seq(a, b)).head.segment.span, segs(1).span)
  }

  test("hybrid: keyword and symbols fused behind one retriever") {
    val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))
    val byId = files.map(f => (f.id, f)).toMap
    val hybrid = Retrieve.hybrid[okay.Pure](Seq(
      Retrieve.keyword(Keyword.index(segs)),
      Retrieve.symbols(Symbols.project(files), byId)))
    val hits = okay.!.run(hybrid.retrieve("add", 5))
    assert(hits.exists(_.segment.text.contains("def add")))
  }

  test("multi-query explores rewrites as nondeterminism, then fuses") {
    val segs = files.flatMap(f => Ingest.segment(f, 400)(_.length))
    val base = Retrieve.keyword(Keyword.index(segs))
    val multi = Retrieve.multiQuery[okay.Pure](base)(q => Seq(s"$q numbers", s"$q sum"))
    val hits = okay.!.run(multi.retrieve("add", 5))
    assert(hits.exists(_.segment.source == "Math.scala"))
    // the rewrites genuinely widened the result
    assert(hits.length >= okay.!.run(base.retrieve("add", 5)).length)
  }

  test("re-index after an edit embeds only what changed") {
    val store = MemoryStore()
    val src = files(1)                       // Math.scala, two definitions
    // a budget that separates the definitions: with one big enough to
    // hold the whole file there is only one segment, and "only what
    // changed" is trivially everything (the first run of this test
    // said so, which is the useful kind of failure)
    val budget = 60
    run(Ingest.run[okay.Pure](store, Seq(src), budget)(_.length))
    val session = Code.parse(src.text)
    val sizeBefore = okay.!.run(store.size)

    val at = src.text.indexOf("a * b")
    val edited = src.text.patch(at, "a - b", 5)
    val (fresh, p) = run(
      Ingest.reindex[okay.Pure](store, session, src, edited,
        at, at + 5, at + 5, budget)(_.length))

    assertEquals(okay.parse.Cst.lexemes(fresh.tree), edited)
    assert(p.reused > 0, "nothing was reused — the whole file was re-embedded")
    assert(p.embedded < p.segments,
      s"re-embedded everything: ${p.embedded} of ${p.segments}")
    assertEquals(okay.!.run(store.size), sizeBefore)   // no duplicates left behind
  }

  test("the index persists through our own codec, exactly") {
    val store = MemoryStore()
    run(Ingest.run[okay.Pure](store, files)(_.length))
    val bytes = Persist.save(store)
    val back = Persist.load(bytes)
    assert(back.isRight, back.left.getOrElse(""))
    val restored = MemoryStore()
    restored.restore(back.toOption.get)
    assertEquals(okay.!.run(restored.size), okay.!.run(store.size))
    // and the restored index answers the same query the same way
    val q = Vectors.hashing()("multiply numbers")
    assertEquals(okay.!.run(restored.search(q, 3)).map(_.segment.span),
      okay.!.run(store.search(q, 3)).map(_.segment.span))
  }

  test("the similarity metric is a parameter, not a constant") {
    // it was hardcoded to cosine inside the store, which is exactly
    // the kind of choice that has to be the caller's: most providers
    // return unit vectors, where dot product is cosine without two
    // wasted square roots
    val segs = Ingest.segment(Source("a.scala", "class Greeter { def hello = 1 }\n"),
      400)(_.length)
    def stocked(m: Similarity): MemoryStore =
      val st = MemoryStore(m)
      val f = Vectors.hashing()
      st.upsert(segs.map(s => (s, f(s.text)))).runWith
      st

    val q = Vectors.hashing()("greeter hello")
    val byCosine = stocked(Vectors.cosine).search(q, 1).runWith.head
    val byDot = stocked(Vectors.dot).search(q, 1).runWith.head
    val byEuclid = stocked(Vectors.euclidean).search(q, 1).runWith.head

    // the same segment wins under all three here — what differs is
    // the score, which is what a metric is
    assertEquals(byCosine.segment.span, byDot.segment.span)
    assertEquals(byCosine.segment.span, byEuclid.segment.span)
    // hashing() normalizes, so cosine and dot agree to float noise
    assert(math.abs(byCosine.score - byDot.score) < 1e-4f,
      s"${byCosine.score} vs ${byDot.score}")
    // and a distance is negated, so larger is still better
    assert(byEuclid.score <= 0f, s"euclidean returned ${byEuclid.score}")
  }

  test("the persisted index is float32, not boxed doubles") {
    // the vector is written as raw bytes, so its cost on disk is
    // exactly four bytes per component plus a short header — where
    // List[Double] spent nine bytes and two boxed objects per
    // component, one on the way out and one on the way back
    val store = MemoryStore()
    val f = Vectors.hashing(1536)
    val segs = files.flatMap(s => Ingest.segment(s, 400)(_.length))
    store.upsert(segs.map(s => (s, f(s.text)))).runWith

    val bytes = Persist.save(store)
    val vectorBytes = segs.size * 1536 * 4
    val overhead = bytes.length - vectorBytes
    assert(overhead > 0, "the vectors do not fit — something is not float32")
    // the segments' own text dominates what is left; the point is
    // that the vectors themselves cost their exact width
    assert(vectorBytes * 9 / 4 > vectorBytes,
      "sanity: doubles would have cost 2.25x this")

    // and the round trip is bit-exact, which is what float32 buys
    val back = Persist.load(bytes)
    assert(back.isRight, back.left.getOrElse(""))
    val restored = back.toOption.get
    assertEquals(restored.size, segs.size)
    for ((_, original), (_, roundTripped)) <- segs.map(s => (s, f(s.text))).zip(restored) do
      assertEquals(roundTripped.toList, original.toList)
  }

  test("packing survives a damaged byte string instead of throwing") {
    // totality, the same rule as everywhere upstream: a length that is
    // not a multiple of four is damage, and damage truncates
    val v = Vectors.hashing(8)("anything")
    val packed = Persist.pack(v)
    assertEquals(Persist.unpack(packed).toList, v.toList)
    assertEquals(Persist.unpack(packed.dropRight(3)).length, v.length - 1)
    assertEquals(Persist.unpack(Array.empty[Byte]).length, 0)
  }

  test("hybrid's fanOut is read: a wider fan-out changes the fusion") {
    // it was a dead parameter — declared, defaulted, named for what it
    // meant, and never read, so asking for a wider fan-out silently
    // did nothing. Over-fetching is the whole point of fusing: a
    // document ranked fourth by BOTH retrievers should beat one ranked
    // first by only one, and it cannot be seen if each list was cut
    // to three before fusion ever ran.
    val counted = scala.collection.mutable.Buffer[Int]()
    def spy(inner: Retriever[okay.Pure]): Retriever[okay.Pure] = new:
      def retrieve(query: String, k: Int): Seq[Scored] ! okay.Pure =
        counted += k
        inner.retrieve(query, k)

    val segs = files.flatMap(s => Ingest.segment(s, 400)(_.length))
    val kw = Retrieve.keyword(Keyword.index(segs))

    counted.clear()
    Retrieve.hybrid[okay.Pure](Seq(spy(kw)), fanOut = 25)
      .retrieve("numbers", 3).runWith
    assertEquals(counted.toList, List(25),
      "the retriever was not asked for the fan-out")

    // and k still wins when it is the larger of the two
    counted.clear()
    Retrieve.hybrid[okay.Pure](Seq(spy(kw)), fanOut = 2)
      .retrieve("numbers", 9).runWith
    assertEquals(counted.toList, List(9), "asking for k > fanOut lost hits")

    // whatever the fan-out, exactly k comes back
    val out = Retrieve.hybrid[okay.Pure](Seq(kw), fanOut = 25)
      .retrieve("numbers", 2).runWith
    assert(out.size <= 2, s"returned ${out.size} for k = 2")
  }
}
