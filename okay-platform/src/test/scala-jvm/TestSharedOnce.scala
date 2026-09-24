package okay

import okay.given
import java.util.concurrent.atomic.AtomicInteger

/**
 * once-across-fibres: one `!.once` value demanded from two fibres.
 * Under `Once.run` each fibre has its own cells and the program runs
 * twice; under one `SharedOnce` it runs once and the second fibre
 * waits for the first's answer. JVM: `par` needs a Scheduler and the
 * run needs `CanBlock`.
 */
class TestSharedOnce extends munit.FunSuite:

  /** counts its runs, and takes long enough for the second demand to
   * arrive while the first is in flight */
  def slow(runs: AtomicInteger): Int ! Once + Async =
    !.once[Int, Async](effect[Once + Async, Unit](Async.Run(() => { runs.incrementAndGet(); Thread.sleep(30) })).map(_ => 42))

  test("one handle, two fibres, one store: the program runs once and the second fibre waits for its answer") {
    val runs = AtomicInteger(0)
    val p = slow(runs)
    val store = SharedOnce()
    val (a, b) = Async.par(store.run(p), store.run(p)).runWith
    assertEquals((a, b), (42, 42))
    assertEquals(runs.get, 1, "the shared store ran the program more than once")
  }

  test("the threaded reading, for contrast: Once.run per fibre runs the same value twice") {
    val runs = AtomicInteger(0)
    val p = slow(runs)
    val (a, b) = Async.par(Once.run[Int, Async](p), Once.run[Int, Async](p)).runWith
    assertEquals((a, b), (42, 42))
    assertEquals(runs.get, 2, "each fibre's Once.run has its own cells")
  }

  test("a demand after the answer is stored answers from the store; the first store wins") {
    val runs = AtomicInteger(0)
    val p = slow(runs)
    val store = SharedOnce()
    assertEquals(store.run(p).runWith, 42)
    assertEquals(store.run(p).runWith, 42)
    assertEquals(runs.get, 1)
  }

  test("runIn: the rest of the row is forwarded past the store") {
    val runs = AtomicInteger(0)
    val store = SharedOnce()
    val p: Int ! (Once + (Async + Writer % String)) =
      !.once[Int, Async + Writer % String](
        effect[Once + (Async + Writer % String), Unit](Async.Run(() => { runs.incrementAndGet(); () }))
          .flatMap(_ => effect[Once + (Async + Writer % String), Unit](Writer("ran"))).map(_ => 7))
    val twice = p.flatMap(x => p.map(y => x + y))
    val (log, n) = Writer.run[String, Int, Async](store.runIn[Int, Writer % String](twice)).runWith
    assertEquals(n, 14)
    assertEquals(log, Seq("ran"), "the program's own tell happened once, with its one run")
    assertEquals(runs.get, 1)
  }
