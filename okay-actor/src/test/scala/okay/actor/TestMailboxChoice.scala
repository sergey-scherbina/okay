package okay.actor

import okay.*
import okay.given

/**
 * THE MAILBOX CHOICE `ActorRef`'s header offers, COMPILED AND RUN.
 *
 * The default mailbox is the library's `growing` buffer, which gives
 * up ONE displacement of a sender's own order across its one-shot
 * swap (operator's decision, 2026-09-18; `TestChannelLaws` states the
 * weakened law and `ProbeGrowingOrder` names the mechanism). That is
 * the right default, and the wrong one for a protocol whose messages
 * mean something in sequence — so the header hands the reader two
 * spellings that take the exact order back, and these are those
 * spellings, run.
 *
 * A DOCUMENTED SPELLING THAT DOES NOT COMPILE IS A LIE THAT READS
 * WELL. The first cut of that comment said
 * `Queues.strong[M].fifo(256)`, which is a BUILDER and not a channel;
 * nothing would ever have caught it, because a comment is not code.
 * It lives beside `TestActorCross` and borrows its shape: programs
 * driven by `Async.runAsync`, so it runs on every platform the module
 * cross-builds for.
 */
class TestMailboxChoice extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  private def sendAll(ref: ActorRef[Int], ms: Iterable[Int]): Unit ! Async =
    ms.foldLeft(async(())): (acc, m) =>
      acc.flatMap(_ => ref.tell(m).map(_ => ()))

  /** one sender, a hundred messages whose order means something */
  private def sequence(mailbox: Channel[Int]) =
    val seen = scala.collection.mutable.ArrayBuffer.empty[Int]
    val prog =
      for
        ref <- Actor.spawn(0, mailbox, Supervise.Stop)(
                 (n: Int, m: Int) => async { seen += m; n + 1 })
        _ <- sendAll(ref, 0 to 99)
        _ <- ref.stop()
      yield assertEquals(seen.toList, (0 to 99).toList)
    Async.runAsync(prog)

  test("the DEFAULT mailbox — the spelling the header shows, and it runs") {
    // one sender never grows the buffer, so the default is exact here
    // too; what it gives up needs a SECOND sender, which is
    // `TestChannelLaws`' two-producer law and not this file's job
    sequence(Channel[Int](256))
  }

  test("`adaptive` — the many-sender answer, and it never adopts a buffer") {
    sequence(Queues.strong[Int].adaptive.each(256).build)
  }

  test("`fifo` — the ring, strongest, and the one the header got wrong at first") {
    sequence(Queues.strong[Int].fifo(256).build)
  }
}
