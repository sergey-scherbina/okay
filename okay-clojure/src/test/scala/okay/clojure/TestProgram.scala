package okay.clojure

import okay.{!, %, +, Async, Choose, Chunks, Reader, Stage, State, Throws, Writer, effect, pure, runChoice, runEither, through}
import okay.given
import java.util.concurrent.atomic.AtomicInteger

/**
 * Clojure programs written in `okay.core` run as okay programs, and lazy
 * seqs cross both ways (specs/clojure.md, stage 2). The Clojure side is
 * src/test/resources/okay/clojure/programs.clj; `okay.core` itself is a
 * resource of the main jar. Every loop is bounded, so a broken driver
 * FAILS rather than hangs.
 */
class TestProgram extends munit.FunSuite {

  val ns = "okay.clojure.programs"
  def fn(name: String) = Clj.fn(ns, name).fold(e => fail(e), identity)
  def value(name: String) = Clj.value(ns, name).fold(e => fail(e), identity)

  def emit[I](xs: Seq[I]): Unit ! Writer % I =
    xs.foldRight(pure[Writer % I, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))
  def emitIn[I, G[+_]](xs: Seq[I]): Unit ! Writer % I + G =
    xs.foldRight(pure[Writer % I + G, Unit](()))((x, p) => effect[Writer % I + G, Unit](Writer(x)).flatMap(_ => p))
  def own[I, O, A](src: Unit ! Writer % I, s: Stage[I, O, A]): List[O] =
    !.run(Writer.run(through(src)(s)))._1.toList

  val inputs: List[List[Long]] = List(Nil, List(7L), (1L to 10L).toList, (1L to 1000L).toList)

  // ---------------------------------------------------------------- stage

  test("okay.core loads from the jar, and a Clojure running-sum stage equals okay's own") {
    val okaySide = Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))
    for xs <- inputs do
      val clj = Program.stage[Long, java.lang.Long](fn("running-sum").invoke(Long.box(0L)))
      assertEquals(own(emit(xs), clj).map(_.longValue), own(emit(xs), okaySide), s"over ${xs.size}")
  }

  test("a Clojure stage that performs: a filter asking its threshold from Reader") {
    val stage = Program.stageWith[Long, java.lang.Long, Reader % Long](fn("above-threshold").invoke())
    val out = through[Long, java.lang.Long, Reader % Long, Unit, Unit](emitIn[Long, Reader % Long](List(3L, 9L, 5L, 12L, 7L)))(stage)
    assertEquals(!.run(Reader.run(6L)(Writer.run(out)))._1.map(_.longValue), Seq(9L, 12L, 7L))
  }

  test("a hundred thousand Clojure steps on the default stack") {
    val out = own(emit(List[Long]()), Program.stage[Long, java.lang.Long](fn("count-to").invoke(Long.box(0L), Long.box(100000L))))
    assertEquals((out.size, out.last.longValue), (100000, 99999L))
  }

  // ------------------------------------------------------------------ run

  test("Reader and State performed from Clojure, in mlet order") {
    val prog = Program.run[Reader % Long + State % Long, java.lang.Long](value("reader-state"))(
      using summon, Program.Row.of[Reader % Long] | Program.Row.of[State % Long])
    val (s, a) = !.run(State.handle(5L)(Reader.run(7L)(prog)))
    assertEquals((s, a.longValue), (6L, 7006L))
  }

  test("a Throws raised from Clojure reaches runEither as a Left") {
    assertEquals(!.run(runEither(Program.run[Throws % Any, java.lang.Long](value("raising")))), Left("boom from clojure"))
  }

  test("MULTI-SHOT: Choose resumes the Clojure continuation once per branch") {
    val all = !.run(runChoice(Program.run[Choose, java.lang.Long](value("choosing"))))
    assertEquals(all.map(_.longValue).sorted, Seq(11L, 12L, 21L, 22L))
  }

  test("Async from Clojure: two 20 ms sleeps take at least 40 ms and answer 40") {
    val t0 = System.nanoTime()
    val slept = Async.run(Program.run[Async, java.lang.Long](value("sleep-twice"))).runWith
    assertEquals(slept.longValue, 40L)
    assert((System.nanoTime() - t0) / 1_000_000 >= 40)
  }

  test("refused by name: an operation outside the row, await/tell in a run") {
    val e1 = intercept[IllegalArgumentException](!.run(Reader.run(1L)(Program.run[Reader % Long, java.lang.Long](value("raising")))))
    assert(e1.getMessage.contains("not an operation of this program's row"), e1.getMessage)
    val e2 = intercept[IllegalStateException](!.run(Reader.run(1L)(Program.run[Reader % Long, java.lang.Long](fn("running-sum").invoke(Long.box(0L))))))
    assert(e2.getMessage.contains("await outside a stage"), e2.getMessage)
  }

  // ------------------------------------------------------------------ seqs

  test("an INFINITE Clojure (range) read partially as okay Chunks") {
    val range = Clj.eval("(range)").fold(e => fail(e), identity)
    val firstFive = Chunks.foldLeft(Chunks.take(Program.chunks[java.lang.Long](range))(5))(Vector.empty[Long])(_ :+ _.longValue)
    assertEquals(firstFive, Vector(0L, 1L, 2L, 3L, 4L))
  }

  test("a Clojure seq is realised only as far as okay pulls (counted; bounded, so an eager bridge FAILS)") {
    // (range 1000) mapped through a counter; Clojure's chunked seqs realise
    // 32 at a time, so reading 5 in chunks of 8 realises at most one block
    Clj.eval("(def okay-test-realised (atom 0))").fold(e => fail(e), _ => ())
    val coll = Clj.eval("(map (fn [x] (swap! okay-test-realised inc) x) (range 1000))").fold(e => fail(e), identity)
    val firstFive = Chunks.foldLeft(Chunks.take(Program.chunks[java.lang.Long](coll, size = 8))(5))(Vector.empty[Long])(_ :+ _.longValue)
    assertEquals(firstFive, Vector(0L, 1L, 2L, 3L, 4L))
    val count = Clj.eval("@okay-test-realised").fold(e => fail(e), identity)
    assert(count.toString.toLong <= 32L, s"$count of 1000 realised for five elements")
  }

  def countedNats(produced: AtomicInteger, size: Int): Chunks[java.lang.Long] =
    Chunks.map(Chunks.nats[Long](size))(x => { produced.incrementAndGet(): Unit; Long.box(x) })

  test("an INFINITE okay source as a Clojure lazy seq: (take 10 …) produces at most one chunk") {
    val produced = AtomicInteger()
    val sum = Clj.fn("clojure.core", "reduce").fold(e => fail(e), identity)
      .invoke(Clj.fn("clojure.core", "+").fold(e => fail(e), identity),
        Clj.fn("clojure.core", "take").fold(e => fail(e), identity).invoke(Long.box(10L), Program.seq(countedNats(produced, 16))))
    assertEquals(sum, Long.box(45L))
    assert(produced.get <= 16, s"okay produced ${produced.get} for (take 10) (chunk 16)")
  }

  test("a seq round trip: okay -> Clojure (map inc) -> okay") {
    val inc = Clj.fn("clojure.core", "map").fold(e => fail(e), identity)
      .invoke(Clj.fn("clojure.core", "inc").fold(e => fail(e), identity), Program.seq(Chunks.map(Chunks.range(0, 5))(Long.box)))
    assertEquals(Chunks.foldLeft(Program.chunks[java.lang.Long](inc))(Vector.empty[Long])(_ :+ _.longValue), Vector(1L, 2L, 3L, 4L, 5L))
  }
}
