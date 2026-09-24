package okay

import okay.Writer.byValue.given

/** two signatures with nothing but their class to be told apart by */
enum Ping[+A] derives Effect:
  case Pong() extends Ping[Int]

enum Peng[+A] derives Effect:
  case Pung() extends Peng[String]

/**
 * `Distinct[R]` against the rows `TestRowIdentity` already runs.
 *
 * That suite is the specification: every row it shows routing
 * correctly must compile here, and every row it shows misrouting must
 * not. The two suites are the same statement at the two times a row
 * can be wrong — and this one is the time you can still fix it.
 */
class TestDistinct extends munit.FunSuite {

  test("two signatures: different classes, nothing to confuse") {
    summon[Distinct[Reader % Int + Writer % String]]
  }

  test("two READERS misroute, and the compiler now says so") {
    val e = compileErrors("summon[Distinct[Reader % Int + Reader % String]]")
    assert(e.contains("cannot be told apart in one row"), e)
    assert(e.contains("docs/many-instances.md"), e)
  }

  test("two WRITERS are told apart — the test reads the told value") {
    // TestRowIdentity runs this row and both writers collect the
    // right elements; the check must not refuse what works, and it
    // knows only because `Writer.byValue.writerK` (imported above)
    // declares TypeableK.ByValue — the default `writerK` is by the
    // class of `Say`, unmarked, and would refuse this row
    summon[Distinct[Writer % String + Writer % Int]]
    // the same import, used in the open where the linter can see it
    // (Distinct's macro-time search does not count as a use)
    val _ = summon[TypeableK.ByValue[Writer % String]]
  }

  test("three members, the collision in the middle") {
    val e = compileErrors(
      "summon[Distinct[Writer % String + Reader % Int + Reader % Long]]")
    assert(e.contains("cannot be told apart in one row"), e)
  }

  test("two KEYS over the same signature: a good row") {
    summon[Distinct[Tag.Of["a", Reader % Int] + Tag.Of["b", Reader % Int]]]
  }

  test("ONE key over one signature, twice: the case Tag cannot fix") {
    // tag-test-the-signature-too made the runtime test key AND
    // signature; this is the half it could not reach, since the
    // signature erases to one class and the key is the same
    val e = compileErrors(
      """summon[Distinct[
           Tag.Of["same", Reader % Int] + Tag.Of["same", Reader % String]]]""")
    assert(e.contains("cannot be told apart in one row"), e)
  }

  test("one key over TWO signatures: the row tag-test-the-signature-too opened") {
    summon[Distinct[Tag.Of["k", Ping] + Tag.Of["k", Peng]]]
  }

  test("an untagged signature does not collide with itself under a key") {
    summon[Distinct[Ping + Tag.Of["k", Ping]]]
  }

  /**
   * A ROW CANNOT REPEAT A MEMBER AT ALL, which is worth pinning
   * because it is the reason the check never has to consider it: `+`
   * is a UNION, `F | F` is `F`, and the compiler collapses it before
   * any macro sees the type. Two `Instances.Of[Ping]` are the one
   * member `Instances` was written to be — which is also the right
   * answer, since one member is one test and there is nothing to
   * misroute.
   */
  test("a repeated member is ONE member: the union collapses") {
    summon[(Instances.Of[Ping] + Instances.Of[Ping])[Int] =:= Instances.Of[Ping][Int]]
    summon[Distinct[Instances.Of[Ping] + Instances.Of[Ping]]]
    summon[Distinct[Reader % Int + Reader % Int]]
  }

  test("Instances over different signatures is a row") {
    summon[Distinct[Instances.Of[Ping] + Instances.Of[Peng]]]
  }

  /**
   * An abstract row is ALLOWED, and this is not a gap in the check —
   * it is what row-generic code is. An interpreter's residual `G` is
   * unknown at the definition site and checked where it is
   * instantiated, which is the only place the answer exists.
   */
  test("an abstract member is allowed: nothing is known, so nothing is refused") {
    def residual[G[+_]](using Distinct[Reader % Int + G]): Int = 1
    assertEquals(residual[Writer % String], 1)
  }

  /**
   * A HANDLER'S REST SOLVED AS THE ROW ITSELF, found by this lane in a
   * benchmark (`ProducerWriterCarrierBenchmark.chunksMapWriter`). With
   * the rest left to inference inside an enclosing handler,
   * `Writer.collect(Writer.map(p)(f))` solves map's rest G as
   * `Writer % Long` itself (`F | F` is `F`, so Distinct cannot see it),
   * and `map` tested G FIRST: every Say was forwarded unmapped, a
   * silently wrong answer. `map` and `expand` test Writer first now.
   */
  test("Writer.map and expand map, whatever their rest is inferred as") {
    val p: Unit ! Writer % Long = Writer.tell(1L).flatMap(_ => Writer.tell(5L))
    assertEquals(!.run(Writer.collect(Writer.map(p)(_ * 2))), (Vector(2L, 10L), ()))
    assertEquals(!.run(Writer.collect(Writer.map[Long, Long, Unit, Pure](p)(_ * 2))), (Vector(2L, 10L), ()))
    assertEquals(!.run(Writer.collect(Writer.expand(p)(x => Vector(x, x)))), (Vector(1L, 1L, 5L, 5L), ()))
  }

  test("Writer.uncons and the stream iterator yield the told values when the rest IS the Writer") {
    val p: Unit ! Writer % Long = Writer.tell(1L).flatMap(_ => Writer.tell(5L))
    // the rest forced to the row itself, as inference does inside an enclosing handler
    // the residual row is the Writer too, so collect drains what uncons did not yield
    val (escaped, first) = !.run(Writer.collect(Writer.uncons[Long, Unit, Writer % Long](p)))
    assertEquals(first.map(_._1), Right(1L))
    assertEquals(escaped, Vector.empty[Long])
  }

  test("Pure is a member that collides with nothing") {
    summon[Distinct[Ping + Pure]]
    summon[Distinct[Pure + Pure]]
  }

  /** the handlers ask too (distinct-on-handlers, 2026-09-24): a
   * per-signature handler splits its signature out of the row by the
   * same class test, so it is where two of one class misroute */
  test("the handlers refuse a row holding two of one class: Reader.run, State.handle, Throws.runEither") {
    val r = compileErrors("Reader.run[Int, String, Reader % String](7)(twoReaders)")
    assert(r.contains("cannot be told apart in one row"), r)
    val s = compileErrors("State.handle(1)(twoStates)")
    assert(s.contains("cannot be told apart in one row"), s)
    val t = compileErrors("runEither[Int, Throws % String, Int](twoThrows)")
    assert(t.contains("cannot be told apart in one row"), t)
  }

  test("and a distinct row still runs through them all") {
    import okay.Row.at
    type R3 = State % Int + Reader % Int + Throws % String
    val ok: Int ! R3 = State.get[Int].at[R3].flatMap(n => Reader.ask[Int].at[R3].map(_ + n))
    assertEquals(!.run(runEither(Reader.run(2)(State.handle(1)(ok)))), Right((1, 3)))
  }
}

val twoReaders: String ! Reader % Int + Reader % String =
  okay.effect[Reader % Int + Reader % String, Int](Reader.Ask())
    .flatMap(n => okay.effect[Reader % Int + Reader % String, String](Reader.Ask()).map(s => s"$n/$s"))
val twoStates: Int ! State % Int + State % String =
  okay.effect[State % Int + State % String, Int](State.Get()).map(_ + 1)
val twoThrows: Int ! Throws % Int + Throws % String =
  okay.effect[Throws % Int + Throws % String, Int](Throws(1))
