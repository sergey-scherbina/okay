package okay

import Chunks.elements
import java.util.concurrent.atomic.AtomicInteger
import scala.util.Random

/**
 * `Source.joinSorted` (specs/stream-join.md): the sort-merge join on
 * the live carrier — `Chunks.joinSorted`'s answer, each side on a
 * fiber of its own, plus the release law a fiber per side brings
 * (`TestSourceZip`'s, since the join has `zip`'s shape).
 */
class TestSourceJoin extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private type R = Writer % (Int, String) + Async

  private def rows[O](s: Source[O]): Vector[O] = s.runCollect.runWith

  private def sortedRows(rnd: Random, n: Int, keys: Int, tag: String): List[(Int, String)] =
    List.fill(n)(rnd.nextInt(keys)).sorted.zipWithIndex.map((k, i) => (k, s"$tag$i"))

  private val l3 = List((1, "a"), (2, "b"), (4, "d"))
  private val r4 = List((2, "x"), (3, "y"), (3, "z"), (4, "w"))

  test("the same pairs as Chunks.joinSorted, at every buffer size") {
    for capacity <- List(1, 4, 64); seed <- 1 to 6 do
      val rnd = Random(seed)
      val l = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "l")
      val r = sortedRows(rnd, rnd.nextInt(80), 1 + rnd.nextInt(10), "r")
      note(s"capacity $capacity seed $seed: ${l.size} x ${r.size}")
      val expected = Chunks.joinSorted(Chunks.fromIterator(l.iterator), Chunks.fromIterator(r.iterator)).elements.toVector
      assertEquals(rows(Source.joinSorted(Source.of(l), Source.of(r), capacity)), expected, s"capacity $capacity seed $seed")
    assertEquals(rows(Source.leftJoinSorted(Source.of(l3), Source.of(r4))),
      Vector((1, ("a", None)), (2, ("b", Some("x"))), (4, ("d", Some("w")))))
    assertEquals(rows(Source.fullJoinSorted(Source.of(l3), Source.of(r4))),
      Vector((1, (Some("a"), None)), (2, (Some("b"), Some("x"))),
             (3, (None, Some("y"))), (3, (None, Some("z"))), (4, (Some("d"), Some("w")))))
  }

  test("an inner join ends at the left's end and closes the endless right, whose feeder stops producing") {
    val produced = AtomicInteger(0)
    val endless: Source[(Int, Int)] = Source.of(LazyList.from(0).map(i => { produced.incrementAndGet(); (i, i) }))
    val out = rows(Source.joinSorted(Source.of(List((1, "a"))), endless, capacity = 4))
    assertEquals(out, Vector((1, ("a", 1))))
    onFailure(s"produced ${produced.get} on the endless side")
    // the run at 1 closed by the row at 2, the buffer of 4, a refill and the refused one
    assert(produced.get <= 3 + 4 + 1 + 1, s"the endless side ran on after the join ended: ${produced.get}")
    val settled = produced.get
    Thread.sleep(50)
    assertEquals(produced.get, settled, "the endless side is still producing after the join ended")
  }

  test("an inner join ends at the right's end too; a left join ends at the left's; a full join drains both") {
    val endlessL: Source[(Int, String)] = Source.of(LazyList.from(0).map(i => (i, s"l$i")))
    assertEquals(rows(Source.joinSorted(endlessL, Source.of(List((0, "x"), (2, "y"))), capacity = 4)),
      Vector((0, ("l0", "x")), (2, ("l2", "y"))))
    val endlessR: Source[(Int, String)] = Source.of(LazyList.from(0).map(i => (i, s"r$i")))
    assertEquals(rows(Source.leftJoinSorted(Source.of(List((1, "a"), (3, "b"))), endlessR, capacity = 4)),
      Vector((1, ("a", Some("r1"))), (3, ("b", Some("r3")))))
    assertEquals(rows(Source.fullJoinSorted(Source.of(List((1, "a"))), Source.of(List((0, "x"), (1, "y"), (2, "z"))))),
      Vector((0, (None, Some("x"))), (1, (Some("a"), Some("y"))), (2, (None, Some("z")))))
  }

  test("two endless sorted sources join lazily under an early stop, which releases both sides once; a full run releases nothing") {
    for (sname, sch) <- List("loom" -> Schedulers.loom, "own" -> Schedulers.own.build, "default" -> summon[Scheduler]) do
      val before = Source.mergeReleases.get
      val j = Source.joinSorted(Source.of(LazyList.from(0).map(i => (i, i))), Source.of(LazyList.from(0).map(i => (i * 2, i))), capacity = 4)
      val f = sch.fork(() => j.runFoldUntil(using FoldUntil.take[(Int, (Int, Int))](3)))
      assertEquals(f.joinEither(), Right(Vector((0, (0, 0)), (2, (2, 1)), (4, (4, 2)))), sname)
      assertEquals(Source.mergeReleases.get - before, 1L, s"$sname: the early stop released the join once")
      val before2 = Source.mergeReleases.get
      val full = Source.joinSorted(Source.of(l3), Source.of(r4))
      assertEquals(sch.fork(() => full.runCollect).joinEither(), Right(Vector((2, ("b", "x")), (4, ("d", "w")))), sname)
      assertEquals(Source.mergeReleases.get - before2, 0L, s"$sname: a join that ran to its end released its sides")
  }

  test("a side that fails fails the join, after every pair told before the failure") {
    object Boom extends RuntimeException("boom")
    val failing: Source[(Int, String)] =
      Source.of(List((0, "x"), (1, "y"))).flatMap(_ => okay.effect[R, Unit](Async.Run[Unit](() => throw Boom)))
    val seen = scala.collection.mutable.ArrayBuffer.empty[(Int, (Int, String))]
    val j = Source.joinSorted(Source.of(LazyList.from(0).map(i => (i, i))), failing, capacity = 64)
    val thrown = intercept[RuntimeException](j.runForeach(p => okay.pure[Async, Unit] { seen += p; () }).runWith)
    assert(thrown eq Boom, s"wrong failure: $thrown")
    // the run at 1 was never closed, so its row was never matched: the failure reached it first
    assertEquals(seen.toVector, Vector((0, (0, "x"))))
  }

  test("a key out of order fails the join, after the pairs before it") {
    val seen = scala.collection.mutable.ArrayBuffer.empty[(Int, (String, String))]
    val j = Source.joinSorted(Source.of(List((0, "a"), (2, "b"))), Source.of(List((0, "x"), (2, "y"), (1, "z"))))
    val e = intercept[IllegalArgumentException](j.runForeach(p => okay.pure[Async, Unit] { seen += p; () }).runWith)
    assert(e.getMessage.contains("right side") && e.getMessage.contains("key 1 after 2"), e.getMessage)
    assertEquals(seen.toVector, Vector((0, ("a", "x"))))
  }
}
