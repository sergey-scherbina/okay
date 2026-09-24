package okay2.platform

import java.util.concurrent.atomic.AtomicInteger
import okay2._
import okay2.async._

/**
 * once-across-fibres: one `!.once` value demanded from two fibres.
 * Under `Once.run` each fibre has its own cells and the program runs
 * twice; under one `SharedOnce` it runs once and the second fibre
 * waits for the first's answer.
 */
class TestSharedOnce extends munit.FunSuite {

  /** counts its runs, and takes long enough for the second demand to
   * arrive while the first is in flight */
  def slow(runs: AtomicInteger): Int ! (Once + Async) =
    !.once[Int, Async](Async { runs.incrementAndGet(); Thread.sleep(30) }.at[Once + Async].map(_ => 42))

  test("one handle, two fibres, one store: the program runs once and the second fibre waits for its answer") {
    val runs = new AtomicInteger(0)
    val p = slow(runs)
    val store = new SharedOnce
    val (a, b) = Async.par(store.run(p), store.run(p)).runWith
    assertEquals((a, b), (42, 42))
    assertEquals(runs.get, 1, "the shared store ran the program more than once")
  }

  test("the threaded reading, for contrast: Once.run per fibre runs the same value twice") {
    val runs = new AtomicInteger(0)
    val p = slow(runs)
    val (a, b) = Async.par(Once.run(p), Once.run(p)).runWith
    assertEquals((a, b), (42, 42))
    assertEquals(runs.get, 2, "each fibre's Once.run has its own cells")
  }

  test("a demand after the answer is stored answers from the store; the first store wins") {
    val runs = new AtomicInteger(0)
    val p = slow(runs)
    val store = new SharedOnce
    assertEquals(store.run(p).runWith, 42)
    assertEquals(store.run(p).runWith, 42)
    assertEquals(runs.get, 1)
  }

  test("runIn: the rest of the row is forwarded past the store") {
    val runs = new AtomicInteger(0)
    val store = new SharedOnce
    type Rw = Once + (Async + Writer[String])
    val p: Int ! Rw =
      !.once[Int, Async + Writer[String]](
        Async { runs.incrementAndGet(); () }.at[Rw]
          .flatMap(_ => Writer.tell("ran").at[Rw]).map(_ => 7))
    val twice = p.flatMap(x => p.map(y => x + y))
    val (log, n) = Writer.run(store.runIn[Int, Writer[String]](twice)).runWith
    assertEquals(n, 14)
    assertEquals(log, Seq("ran"), "the program's own tell happened once, with its one run")
    assertEquals(runs.get, 1)
  }
}
