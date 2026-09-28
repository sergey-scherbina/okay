package okay

/** `Source.mergeReady` needs no fiber and no blocking, so it runs where
 * only a callback drive exists — JS's event loop included
 * (specs/ready-merge.md). The JVM suite, TestReadyMerge, covers the
 * laws that need a second thread. */
class TestReadyMergeCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  private type R = Writer % Int + Async

  test("a strict round-robin with no scheduler, on every platform") {
    val m = Source.mergeReady(Source.of(List(1, 2, 3)), Source.of(List(10)), Source.of(List(100, 200)))
    Async.runAsync(m.runCollect).map(v => assertEquals(v, Vector(1, 10, 100, 2, 200, 3)))
  }

  test("a callback source merges with a ready one, on every platform") {
    def answered(x: Int): Source[Int] =
      okay.effect[R, Int](Async.Await[Int] { k => k(Right(x)); () => () })
        .flatMap(v => okay.effect[R, Unit](Writer(v)))
    val m = answered(1).flatMap(_ => answered(2)) mergeReady Source.of(List(10, 20))
    Async.runAsync(m.runCollect).map(v => assertEquals(v, Vector(1, 10, 2, 20)))
  }

  test("an early stop releases a parked source when the program ends, on the callback drive of every platform") {
    // ready-merge-cancel-under-consumer-ops: `runAsync` is the Drive `own`
    // and JS run on; the consumer takes two elements and ends while the
    // second source is still parked — the drive's end runs the merge's
    // cancel scope, which it never exited
    var cancelled = false
    val parked: Source[Int] = okay.effect[R, Int](Async.Await[Int](_ => () => cancelled = true))
      .flatMap(v => okay.effect[R, Unit](Writer(v)))
    val m = Source.mergeReady(Source.of(List(1, 2, 3, 4)), parked)
    Async.runAsync(m.runFoldUntil(using FoldUntil.take[Int](2))).map { got =>
      assertEquals(got.size, 2)
      assert(cancelled, "the parked source outlived the program")
    }
  }

  test("a dry ring climbs the wait only where producers are threads: JS registers at once") {
    type R = Writer % Int + Async
    val polls = java.util.concurrent.atomic.AtomicInteger(0)
    val registered = java.util.concurrent.atomic.AtomicInteger(0)
    val side: Source[Int] =
      okay.effect[R, Int](Async.Await[Int](
        k => { registered.incrementAndGet(); k(Right(7)); () => () },
        () => { polls.incrementAndGet(); null })).flatMap(x => okay.effect[R, Unit](Writer(x)))
    Async.runAsync(Source.mergeReady(side, Source.of(List(1, 2, 3))).runCollect).map { out =>
      assertEquals(out, Vector(1, 2, 3, 7))
      assertEquals(registered.get, 1)
      // 3 polls as the ready side's turns pass; the ladder only with threads
      assertEquals(polls.get, if summon[Pause].threads then 3 + 100 + 50 + 4 else 3)
    }
  }
}
