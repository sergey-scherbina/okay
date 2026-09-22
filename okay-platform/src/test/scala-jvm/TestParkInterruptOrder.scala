package okay

import java.util.concurrent.atomic.AtomicBoolean

/**
 * Every park site reads the interrupt BEFORE the answer
 * (park-interrupt-order; the rule scheduler-cancel-wins established
 * for `block`).
 *
 * The scheduler law states the consequence — a cancelled fiber never
 * takes an answer that arrived after the cancel — but it cannot FORCE
 * the window: the fiber must be asleep while both the cancel and the
 * answer land, and nothing in a test can hold a thread there. So the
 * rule is checked where it lives instead, and there it is exact: a
 * caller that is ALREADY interrupted, handed an answer that is
 * ALREADY available, must refuse the answer rather than take it.
 *
 * That is the same state the racing fiber is in when it wakes, and it
 * is reachable in one line. Each case clears the flag afterwards, so
 * a failure here cannot poison the suites that follow.
 */
class TestParkInterruptOrder extends munit.FunSuite {

  private def interrupted[A](body: => A): Either[Throwable, A] =
    Thread.currentThread().interrupt()
    try Right(body)
    catch case e: Throwable => Left(e)
    finally { val _ = Thread.interrupted() }   // clear, whatever happened

  test("block: an interrupted caller does not take an answer that is already there") {
    val cb = summon[CanBlock]
    val got = interrupted(cb.block[Int](k => { k(7); () => () }))
    assert(got.isLeft, s"took the answer while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
  }

  test("blockAccepted: the same, for the blocking send's own park") {
    val cb = summon[CanBlock]
    val unregistered = AtomicBoolean(false)
    val got = interrupted(cb.blockAccepted(k => { k(true); () => { unregistered.set(true) } }))
    assert(got.isLeft, s"took the acceptance while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
    assert(unregistered.get(), "refusing must also withdraw the registration")
  }

  test("await(handoff): the same, for the handoff park") {
    val cb = summon[CanBlock]
    val h = cb.handoff[Int]()
    h.got(7)
    val got = interrupted(cb.await(h))
    assert(got.isLeft, s"returned from await while interrupted: $got")
    assert(got.swap.exists(_.isInstanceOf[InterruptedException]), got.toString)
  }

  test("and none of this leaks the flag: the thread is clean afterwards") {
    assert(!Thread.currentThread().isInterrupted, "the interrupt flag outlived a case")
    // the ordinary path still works, uninterrupted
    val cb = summon[CanBlock]
    assertEquals(cb.block[Int](k => { k(7); () => () }), 7)
    assert(cb.blockAccepted(k => { k(true); () => () }))
  }
}
