package okay

import scala.util.Random

/**
 * `Source.joinWithin` (specs/stream-join.md, stage 2): the windowed join
 * on the live carrier — `WindowJoin`'s pairs whatever the merge's
 * interleaving, since the watermark is the smaller side's, plus the
 * merge's release law.
 */
class TestSourceJoinWithin extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private type Row = (Long, String)

  private def rows[O](s: Source[O]): Vector[O] = s.runCollect.runWith

  /** each side sorted by time — so no row is ever late, and the pair
   * set is exactly the interval's, however the two sides interleave */
  private def side(rnd: Random, n: Int, tag: String): List[(String, Row)] =
    List.fill(n)((rnd.nextInt(5).toString, rnd.between(0L, 200L))).sortBy(_._2).zipWithIndex
      .map { case ((k, t), i) => (k, (t, s"$tag$i")) }

  test("the interval's pairs, at every buffer size, whatever the interleaving") {
    for capacity <- List(1, 4, 64); seed <- 1 to 6 do
      val rnd = Random(seed)
      val l = side(rnd, rnd.nextInt(60), "l"); val r = side(rnd, rnd.nextInt(60), "r")
      val within = 25L
      val expected = (for (k, a) <- l; (k2, b) <- r if k == k2 && math.abs(a._1 - b._1) <= within yield (k, a._2, b._2)).sorted
      note(s"capacity $capacity seed $seed: ${l.size} x ${r.size}, ${expected.size} pairs")
      val out = rows(Source.joinWithin(Source.of(l), Source.of(r), within, 0L, capacity)(_._1, _._1))
      assertEquals(out.map { case (k, (a, b)) => (k, a._2, b._2) }.sorted, expected.toVector, s"capacity $capacity seed $seed")
  }

  test("two endless sides join lazily under an early stop, which releases both once; a full run releases nothing") {
    // TWO ENDLESS SIDES RUN ON LOOM ONLY (windowjoin-spin-fix, 2026-09-30;
    // okay-stream/BUGS.md `ready-merge-side-starves`): on a scheduler with
    // owned workers (`own`, the adaptive default) the `either` merge under
    // this join stops delivering one side within ~20 runs — the hot side's
    // ring ends with its head and tail thirty laps ahead of every stamp —
    // and a fold waiting for the pair (2, 2) then waits for ever. The CI
    // runner's family gate hung twice on exactly this line. The join's
    // own laws are pinned by `TestWindowJoin` (a list, no scheduler) and
    // the bounded cases below on every scheduler; the reproducer is
    // `ProbeReadyMergeStarve`. Widen this list back when that bug closes.
    for (sname, sch) <- List("loom" -> Schedulers.loom) do
      val before = Source.mergeReleases.get
      val ticks = Source.of(LazyList.from(0).map(i => ("k", (i.toLong, s"l$i"))))
      val tocks = Source.of(LazyList.from(0).map(i => ("k", (i.toLong, s"r$i"))))
      val j = Source.joinWithin(ticks, tocks, 0L, 0L, capacity = 4)(_._1, _._1)
      val f = sch.fork(() => j.runFoldUntil(using FoldUntil.take[(String, (Row, Row))](3)))
      assertEquals(f.joinEither().map(_.map { case (k, (a, b)) => (k, a._1, b._1) }), Right(Vector(("k", 0L, 0L), ("k", 1L, 1L), ("k", 2L, 2L))), sname)
      assertEquals(Source.mergeReleases.get - before, 1L, s"$sname: the early stop released the join once")
    for (sname, sch) <- List("loom" -> Schedulers.loom, "own" -> Schedulers.own.build, "default" -> summon[Scheduler]) do
      val before2 = Source.mergeReleases.get
      val full = Source.joinWithin(Source.of(List(("a", (0L, "x")))), Source.of(List(("a", (5L, "y")))), 10L, 0L)(_._1, _._1)
      assertEquals(sch.fork(() => full.runCollect).joinEither(), Right(Vector(("a", ((0L, "x"), (5L, "y"))))), sname)
      assertEquals(Source.mergeReleases.get - before2, 0L, s"$sname: a join that ran to its end released its sides")
  }

  test("a finite side against an endless one: the join ends once nothing can be produced, and releases the endless side") {
    // under a fiber, as `TestSourceZip`'s early stop is: the fiber's handler releases the scope
    // the join left entered; a plain `runWith` has no drive, and only the collector would
    val before = Source.mergeReleases.get
    val produced = java.util.concurrent.atomic.AtomicInteger(0)
    val endless = Source.of(LazyList.from(0).map(i => { produced.incrementAndGet(); ("k", (i.toLong, s"r$i")) }))
    val j = Source.joinWithin(Source.of(List(("k", (5L, "l5")))), endless, 2L, 0L, capacity = 4)(_._1, _._1)
    // Loom, for the reason above: one side ends at once here, so the
    // starvation needs no hot rival — but the runner gates on the default
    val out = Schedulers.loom.fork(() => j.runCollect).joinEither().map(_.map { case (k, (a, b)) => (k, a._2, b._2) })
    // the left row at 5 reaches the right at 3..7; once the right has passed 7 the left row is evicted,
    // the left side has ended, nothing can be produced: the stage returns and the merge is released
    assertEquals(out.map(_.sorted), Right(Vector.tabulate(5)(i => ("k", "l5", s"r${3 + i}"))))
    assertEquals(Source.mergeReleases.get - before, 1L, "the endless side was not released")
    val settled = produced.get
    Thread.sleep(50)
    assertEquals(produced.get, settled, "the endless side is still producing after the join ended")
  }
}
