package okay.clojure

import okay.{Async, Member, Scheduler, Schedulers, given}

/**
 * interop-lift-cancellation for Clojure: `(ok/lift f)` is the blocking
 * step, and a cancel must STOP it, not just report the fiber finished.
 */
class TestClojureCancel extends munit.FunSuite {

  private val markAfter = Clj.fn("okay.clojure.programs", "mark-after").fold(e => throw IllegalStateException(e), identity)
  private given Member[Async] = Member.of[Async]
  private var n = 0
  private def key(): String = { n += 1; s"okay.clj.lift.test.$n" }

  private def run(s: Scheduler, k: String, sleepMs: Long, cancelAt: Option[Long]): (Long, Boolean) =
    val t0 = System.nanoTime
    val f = s.fork(() => Program.run[Async, String](markAfter.invoke(k, java.lang.Long.valueOf(sleepMs))))
    cancelAt.foreach { at => Thread.sleep(at); f.cancel() }
    val _ = f.joinEither()
    val ms = (System.nanoTime - t0) / 1000000
    Thread.sleep(sleepMs + 500)
    (ms, System.getProperty(k) == "woke")

  test("control: uncancelled, the lifted action sleeps its full time and leaves its mark") {
    val (ms, marked) = run(summon[Scheduler], key(), 1500, None)
    assert(ms >= 1400 && marked, s"$ms ms, marked=$marked")
  }

  // The property is the MARK, not the wall clock: the cancel's effect
  // (the lifted sleep never reaches its mark) is what load cannot fake.
  // A `ms < 1000` bound read 1337ms on a loaded box (py-arrow's gate,
  // 2026-09-25, load ~200) with the mark correctly absent — the cancel
  // had landed, only late. backlog: clojure-cancel-wall-clock.
  test("the default scheduler: a cancel interrupts (ok/lift f)") {
    val (ms, marked) = run(summon[Scheduler], key(), 1500, Some(200))
    assert(!marked, s"$ms ms, marked=$marked")
  }

  test("a pool-threaded scheduler: the same") {
    val (ms, marked) = run(Schedulers.drive(), key(), 1500, Some(200))
    assert(!marked, s"$ms ms, marked=$marked")
  }
}
