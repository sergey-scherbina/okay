package okay

import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

/**
 * `Source.zip` (specs/source-zip.md): two live sources paired in
 * lockstep, each side on a fiber of its own, the pairing on the
 * consumer's thread — `Chunks.zip`'s laws on the async carrier, plus
 * the release law a fiber per side brings with it.
 */
class TestSourceZip extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private type R = Writer % Int + Async

  private def pairs[A, B](s: Source[(A, B)]): Vector[(A, B)] = s.runCollect.runWith

  test("lockstep pairs, ending at the shorter side — whichever side that is, at every buffer size") {
    for capacity <- List(1, 4, 64) do
      note(s"capacity $capacity")
      assertEquals(
        pairs(Source.of(List(1, 2, 3)).zip(Source.of(List("a", "b")), capacity)),
        Vector((1, "a"), (2, "b")), s"capacity $capacity: right shorter")
      assertEquals(
        pairs(Source.of(List("a", "b")).zip(Source.of(List(1, 2, 3)), capacity)),
        Vector(("a", 1), ("b", 2)), s"capacity $capacity: left shorter")
      assertEquals(pairs(Source.of(List.empty[Int]).zip(Source.of(List(1)), capacity)), Vector.empty)
      assertEquals(pairs(Source.of(List(1)).zip(Source.of(List.empty[Int]), capacity)), Vector.empty)
  }

  test("each side keeps its own order, however the two sides' buffers align") {
    // one side told from a counter (`Source.range`), the other walked
    // out of a LazyList, buffers of different depths: the pairs are
    // (i, i) for every i whatever the fibers' interleaving was
    val n = 2000L
    val z = Source.range(0, n).zip(Source.of(LazyList.range(0L, n)), capacity = 7)
    val out = pairs(z)
    assertEquals(out.size, n.toInt)
    assert(out.forall((a, b) => a == b), "a pair out of step")
    assertEquals(out.map(_._1), Vector.range(0L, n))
  }

  test("zipWith folds the pair as it is told") {
    val s = Source.of(List(1, 2, 3)).zipWith(Source.of(List(10, 20, 30)))(_ + _)
    assertEquals(s.runCollect.runWith, Vector(11, 22, 33))
  }

  test("two infinite sources zip lazily under an early stop, which releases both sides once; a full run releases nothing") {
    for (sname, sch) <- List("loom" -> summon[Scheduler], "own" -> Schedulers.own.build) do
      val before = Source.mergeReleases.get
      val z = Source.of(LazyList.from(0)).zip(Source.of(LazyList.from(100)), capacity = 4)
      val f = sch.fork(() => z.runFoldUntil(using FoldUntil.take[(Int, Int)](5)))
      assertEquals(f.joinEither(), Right(Vector.tabulate(5)(i => (i, 100 + i))), sname)
      assertEquals(Source.mergeReleases.get - before, 1L, s"$sname: the early stop released the zip once")
      val before2 = Source.mergeReleases.get
      val full = Source.of(List(1, 2)).zip(Source.of(List(3, 4)))
      assertEquals(sch.fork(() => full.runCollect).joinEither(), Right(Vector((1, 3), (2, 4))), sname)
      assertEquals(Source.mergeReleases.get - before2, 0L, s"$sname: a zip that ran to its end released nothing")
  }

  test("the side that outlives the other is closed at the end: its feeder, parked on the full buffer, ends") {
    // the right side is endless and its feeder parks on a buffer of 4;
    // the left side ends after one element. The feeder is a virtual
    // thread on the default scheduler — remembered by a Run at the
    // source's front, which executes on the feeder's own fiber — and
    // the proof it ended is that the thread is gone
    val produced = AtomicInteger(0)
    @volatile var feeder: Thread | Null = null
    val endless: Source[Int] =
      okay.effect[R, Unit](Async.Run(() => feeder = Thread.currentThread()))
        .flatMap(_ => Source.of(LazyList.from(0).map(i => { produced.incrementAndGet(); i })))
    val out = pairs(Source.of(List(7)).zip(endless, capacity = 4))
    assertEquals(out, Vector((7, 0)))
    val t = feeder
    assert(t != null, "the feeder never ran")
    t.nn.join(TimeUnit.SECONDS.toMillis(10))
    assert(!t.nn.isAlive, "the survivor's feeder is still parked after the zip ended")
    onFailure(s"produced ${produced.get} on the endless side")
    // the one element paired, the buffer of 4, the slot the pairing
    // freed and refilled, and the element the full ring then refused:
    // 7, measured exactly that on the first run — not the endless rest
    assert(produced.get <= 1 + 4 + 1 + 1, s"the endless side ran on after the zip ended: ${produced.get}")
  }

  test("a side that fails fails the zip, after every pair told before the failure") {
    object Boom extends RuntimeException("boom")
    val failing: Source[Int] =
      Source.of(List(1, 2)).flatMap(_ => okay.effect[R, Unit](Async.Run[Unit](() => throw Boom)))
    val seen = scala.collection.mutable.ArrayBuffer.empty[(Int, Int)]
    val z = Source.of(LazyList.from(10)).zip(failing, capacity = 64)
    val thrown = intercept[RuntimeException](z.runForeach(p => okay.pure[Async, Unit] { seen += p; () }).runWith)
    assert(thrown eq Boom, s"wrong failure: $thrown")
    assertEquals(seen.toVector, Vector((10, 1), (11, 2)))
  }
}
