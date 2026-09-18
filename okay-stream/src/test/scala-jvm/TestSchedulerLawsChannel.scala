package okay

/**
 * The two scheduler laws whose blocking device is a CHANNEL, so they
 * moved here with the channels (core-modules stage 1). They are the
 * same laws, run against the same family — `SchedulerFamily` is
 * shared from the core's test sources rather than copied, so a
 * scheduler added there reaches these two as well.
 */
class TestSchedulerLawsChannel extends SchedulerFamily {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  /**
   * The same law where the race is FORCED rather than hoped for
   * (scheduler-cancel-wins).
   *
   * The window is between the registration returning and the block
   * reading `filled` for the first time: a cancel and a value that
   * both land in it are seen by a fiber that has not yet looked at
   * its interrupt. It is nanoseconds wide, which is why the sibling
   * law below caught it about one run in three under gate load and
   * never in a quiet one.
   *
   * Here the registration SPINS — a plain spin, not a park, so an
   * interrupt does not end it — until the test has cancelled AND
   * delivered. The fiber then returns into that window every time.
   */
  /**
   * The same rule at the OTHER park sites (park-interrupt-order).
   *
   * `block` was fixed by scheduler-cancel-wins; `blockAccepted` (a
   * blocking channel SEND) still read `filled` before the interrupt
   * in both its fast path and its loop, and `await(Handoff)` in its
   * fast path. The consequence is the law again, one layer down: a
   * cancelled fiber parked on a full channel could take an
   * acceptance that arrived after the cancel.
   *
   * Forced the same way: the send parks on a full channel, the test
   * cancels, then makes room so the send is accepted, and only then
   * lets the fiber look.
   */
  each("cancel wins on a blocking send too: an acceptance after cancel is not the answer") { sch =>
    given Scheduler = sch
    val ch = Channel[Int](1)
    assert(Async.run(ch.send(1)).runWith, "the first send fills the channel")
    val answers = java.util.concurrent.ConcurrentLinkedQueue[Either[Throwable, Boolean]]()
    val f = Async.spawn(ch.send(2))          // parks: the channel is full
    Thread.sleep(20)                          // let it reach the park
    f.cancel()
    val _ = Async.run(ch.receive).runWith     // makes room: the send would now be accepted
    f.onComplete(r => { val _ = answers.offer(r) })
    Thread.sleep(50)
    assert(!answers.contains(Right(true)),
      "an acceptance that arrived after the cancel became the fiber's answer")
    assert(answers.size <= 1, s"answered ${answers.size} times")
  }

  test("adaptive — a fiber that BLOCKS inside a worker does not stop the program") {
    // one worker, and the first fiber blocks on a channel the second fills:
    // without the stuck-check this is a deadlock, which is why `own`
    // is not the default and `adaptive` exists
    given Scheduler = Schedulers.adaptive.workers(1).watched(scala.concurrent.duration.Duration(50, "ms")).build
    val ch = Channel[Int](4)
    val reader = Async.spawn(async {
      val filler = Async.spawn(async { val _ = ch.sendBlocking(9); 0 })
      val got = ch.receiveBlocking()
      val _ = filler.join()
      got
    })
    assertEquals(reader.joinEither().fold(e => throw e, identity), Some(9))
  }
}
