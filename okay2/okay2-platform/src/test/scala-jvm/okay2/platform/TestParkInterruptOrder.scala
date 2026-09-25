package okay2.platform

import java.util.concurrent.atomic.AtomicBoolean
import okay2.async.CanBlock

/**
 * Every park site reads the interrupt BEFORE the answer — the Scala 3
 * core's TestParkInterruptOrder (park-interrupt-order), ported with
 * okay2-scheduler-laws-channel, which found okay2 had no direct check of
 * it: the scheduler laws state the consequence but cannot always force
 * the window, and okay2's `blockAccepted` was covered by nothing.
 *
 * Here it is exact: a caller that is ALREADY interrupted, handed an answer
 * that is ALREADY available, must refuse it. Each case clears the flag
 * afterwards, so a failure cannot poison the suites that follow.
 */
class TestParkInterruptOrder extends munit.FunSuite {

  private val cb: CanBlock = implicitly[CanBlock]

  private def interrupted[A](body: => A): Either[Throwable, A] = {
    Thread.currentThread().interrupt()
    try Right(body)
    catch { case e: Throwable => Left(e) }
    finally { val _ = Thread.interrupted() }
  }

  test("block: an interrupted caller does not take an answer that is already there") {
    val got = interrupted(cb.block[Int](k => { k(7); () => () }))
    assert(got.isLeft, s"took the answer while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
  }

  test("blockAccepted: the same, for the blocking send's own park") {
    val unregistered = new AtomicBoolean(false)
    val got = interrupted(cb.blockAccepted(k => { k(true); () => { unregistered.set(true) } }))
    assert(got.isLeft, s"took the acceptance while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
    assert(unregistered.get(), "refusing must also withdraw the registration")
  }

  test("await(handoff): the same, for the handoff park") {
    val h = cb.handoff[Int]()
    h.got(7)
    val got = interrupted(cb.await(h))
    assert(got.isLeft, s"returned from await while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
  }

  test("and none of this leaks the flag: the thread is clean afterwards") {
    assert(!Thread.currentThread().isInterrupted, "the interrupt flag outlived a case")
    assertEquals(cb.block[Int](k => { k(7); () => () }), 7)
    assert(cb.blockAccepted(k => { k(true); () => () }))
  }
}
