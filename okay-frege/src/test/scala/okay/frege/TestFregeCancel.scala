package okay.frege

import okay.{Async, Scheduler, Schedulers, given}
import okay.frege.{Programs as P}
import frege.run8.Thunk

/**
 * interop-lift-cancellation (backlog polyglot): does cancelling the
 * fiber STOP a lifted Frege IO that blocks? The lifted action runs as
 * ONE step of the walk, so only an interrupt of the thread running it can
 * end it early. The action leaves a mark (a system property) when its
 * sleep finishes, which tells "the work stopped" apart from "the fiber was
 * reported finished while its thread slept on".
 */
class TestFregeCancel extends munit.FunSuite {

  private var n = 0
  private def key(): String = { n += 1; s"okay.lift.test.$n" }

  private def run(s: Scheduler, k: String, sleepMs: Long, cancelAt: Option[Long]): (Long, Boolean) =
    val t0 = System.nanoTime
    val f = s.fork(() => Frege.run[Async, String](P.markAfter(Thunk.`lazy`(k), Thunk.`lazy`(sleepMs)).call()))
    cancelAt.foreach { at => Thread.sleep(at); f.cancel() }
    val _ = f.joinEither()
    val ms = (System.nanoTime - t0) / 1000000
    Thread.sleep(sleepMs + 500)            // time enough for a sleep nobody interrupted
    (ms, System.getProperty(k) == "woke")

  test("control: uncancelled, the lifted action sleeps its full time and leaves its mark") {
    val (ms, marked) = run(summon[Scheduler], key(), 1500, None)
    assert(ms >= 1400, s"$ms ms: the instrument does not see the sleep")
    assert(marked, "the action did not finish")
  }

  test("the default scheduler: a cancel INTERRUPTS the lifted action; it never leaves its mark") {
    val (ms, marked) = run(summon[Scheduler], key(), 1500, Some(200))
    assert(ms < 1000, s"the cancelled fiber ended after $ms ms")
    assert(!marked, "the lifted action ran on to completion after the cancel")
  }

  test("a pool-threaded scheduler: the same") {
    val (ms, marked) = run(Schedulers.drive(), key(), 1500, Some(200))
    assert(ms < 1000, s"the cancelled fiber ended after $ms ms")
    assert(!marked, "the lifted action ran on to completion after the cancel")
  }
}
