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
}
