package okay


import okay.freer.*


import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}

/**
 * A sender that finds ANOTHER sender's waiter ahead of it must park, not
 * spin — on every bounded channel, under both callback schedulers
 * (adaptive-merge-early-stop-livelock, abrupt-sender-head-recheck;
 * 2026-09-28).
 *
 * The shape is the merge's: two producers feeding one ring of 4 the way
 * `Channel.merge`'s feed does (offer while the ring takes, then one
 * `send`), and a consumer that reads in BATCHES (`drained`). A batch pop
 * wakes a parked sender per element it took, and on a callback drive
 * (`own`, `adaptive`) the first wake resumes producer A INLINE on the
 * consumer's thread. A's next send then meets producer B's waiter at the
 * head of the queue while the ring has room: the old recheck retried
 * until the head left — which only the consumer's NEXT wake could make
 * happen, on the thread A was spinning on. Loom hides it (a wake is an
 * unpark, B runs on its own carrier), so the law names the schedulers.
 *
 * Bounded, and the bound is the verdict: a consumer that has not taken
 * 200 elements in 10 s is the spin, not a slow box — the run takes
 * milliseconds.
 */
class TestSendBehindWaiter extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  private val channels: List[(String, () => Channel[Int])] = List(
    "SentinelChannel" -> (() => SentinelChannel[Int](4)),
    "AbruptChannel" -> (() => AbruptChannel[Int](4)))

  private val schedulers: List[(String, () => Scheduler)] = List(
    "own" -> (() => Schedulers.own.build),
    "adaptive" -> (() => Schedulers.adaptive.build))

  /** the merge's feed (`Channel.feed`): offer while the ring takes, and
   * park with ONE send on the element it refused */
  private def produce(c: Channel[Int], from: Int): Unit ! Async =
    okay.freer.pure[Async, Unit](()).flatMap { _ =>
      var i = from
      while c.offer(i) do i += 2
      c.send(i).flatMap(ok => if ok then produce(c, i + 2) else okay.freer.pure[Async, Unit](()))
    }

  /** the fiber's answer within `secs`, or None */
  private def within[A](f: Fiber[A], secs: Int): Option[Either[Throwable, A]] =
    val done = CompletableFuture[Either[Throwable, A]]()
    f.onComplete(r => { done.complete(r): Unit })
    try Some(done.get(secs.toLong, TimeUnit.SECONDS))
    catch case _: TimeoutException => None

  for (cname, mk) <- channels; (sname, sch) <- schedulers do
    test(s"two producers parked on a full $cname, a batch consumer, on $sname: the one woken inline parks behind the other, it does not spin") {
      val s = sch()
      for round <- 0 until 20 do
        val c = mk()
        val ps = List(0, 1).map(p => s.fork(() => produce(c, p)))
        val consumer = s.fork(() => c.drained.runFoldUntil(using FoldUntil.take[Int](200)))
        val got = within(consumer, 10)
        note(s"$cname/$sname round $round: consumer ${got.fold("never answered")(_.fold(e => s"failed: $e", v => s"took ${v.size}"))}")
        c.close()
        got match
          case None =>
            consumer.cancel()
            fail(s"$cname/$sname round $round: the consumer never took 200 — a sender is spinning in attemptSend")
          case Some(r) =>
            val out = r.fold(e => fail(s"$cname/$sname round $round: the consumer failed", e), identity)
            assertEquals(out.size, 200, s"$cname/$sname round $round")
            // parking behind a waiter must not reorder a producer's own sends
            (0 to 1).foreach { p =>
              val own = out.filter(_ % 2 == p)
              assertEquals(own, own.sorted, s"$cname/$sname round $round: producer $p out of its own order")
            }
        // the close answers every parked sender false, so both producers end
        ps.zipWithIndex.foreach { (f, p) =>
          assert(within(f, 10).isDefined, s"$cname/$sname round $round: producer $p never ended after the close")
        }
    }
}
