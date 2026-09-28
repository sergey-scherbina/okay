package okay2.stream

import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}
import okay2._
import okay2.async._
import okay2.platform._
import okay2.stream.Source.SourceOps
import okay2.stream.Channel.ChannelOps

/**
 * A sender that finds ANOTHER sender's waiter ahead of it must park, not
 * spin — the Scala 3 core's TestSendBehindWaiter (okay-stream), ported
 * with the fix (okay2-sender-head-recheck, 2026-09-28), over every member
 * of the scheduler family.
 *
 * The merge's shape: two producers feeding one ring of 4 the way
 * `Channel.feed` does (offer while the ring takes, then one `send`), and a
 * consumer that reads in BATCHES (`drained`). A batch pop wakes a parked
 * sender per element it took, and on a callback drive (`own`, `adaptive`)
 * the first wake resumes producer A INLINE on the consumer's thread; A's
 * next send then met producer B's waiter at the head while the ring had
 * room, and the old recheck retried until the head left — which only the
 * consumer's NEXT wake, on the thread A was spinning on, could do.
 *
 * Bounded, and the bound is the verdict: 200 elements take milliseconds.
 */
class TestSendBehindWaiter extends SchedulerFamily {

  private val channels: List[(String, () => Channel[Int])] = List(
    "SentinelChannel" -> (() => new SentinelChannel[Int](4)),
    "AbruptChannel" -> (() => new AbruptChannel[Int](4)))

  /** `Channel.feed`'s loop: offer while the ring takes, then ONE send */
  private def produce(c: Channel[Int], from: Int): Unit ! Async =
    pure[Async, Unit](()).flatMap { _ =>
      var i = from
      while (c.offer(i)) i += 2
      c.send(i).flatMap(ok => if (ok) produce(c, i + 2) else pure[Async, Unit](()))
    }

  /** the fiber's answer within `secs`, or None */
  private def within[A](f: Fiber[A], secs: Int): Option[Either[Throwable, A]] = {
    val done = new CompletableFuture[Either[Throwable, A]]()
    f.onComplete(r => { val _ = done.complete(r) })
    try Some(done.get(secs.toLong, TimeUnit.SECONDS))
    catch { case _: TimeoutException => None }
  }

  channels.foreach { case (cname, mk) =>
    each(s"two producers parked on a full $cname, a batch consumer: the one woken inline parks behind the other, it does not spin") { sch =>
      (0 until 20).foreach { round =>
        val c = mk()
        val ps = List(0, 1).map(p => sch.fork(() => produce(c, p)))
        val consumer = sch.fork(() => c.drained.runFoldUntil(FoldUntil.take[Int](200)))
        val got = within(consumer, 10)
        c.close()
        got match {
          case None =>
            consumer.cancel()
            fail(s"$cname round $round: the consumer never took 200 — a sender is spinning in attemptSend")
          case Some(r) =>
            val out = r.fold(e => fail(s"$cname round $round: the consumer failed", e), identity)
            assertEquals(out.size, 200, s"$cname round $round")
            (0 to 1).foreach { p =>
              val mine = out.filter(_ % 2 == p)
              assertEquals(mine, mine.sorted, s"$cname round $round: producer $p out of its own order")
            }
        }
        ps.zipWithIndex.foreach { case (f, p) =>
          assert(within(f, 10).isDefined, s"$cname round $round: producer $p never ended after the close")
        }
      }
    }
  }
}
