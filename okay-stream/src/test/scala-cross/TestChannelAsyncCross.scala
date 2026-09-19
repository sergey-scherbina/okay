package okay

import !.*
import okay.given

/** The cross-platform channel bridge: the one test that left
 * `TestAsyncCross` in the core with the channels (core-modules stage
 * 1). It runs on JVM, JS and Native, as it did before. */
class TestChannelAsyncCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  test("a channel bridges sent values into an Async stream on every platform") {
    val c = Channel[Int]()
    assert(c.offer(1)); assert(c.offer(2)); c.close()
    assert(!c.offer(3), "send after close must be refused on every platform")
    val ch = summon[Stream[Channel, Async]]
    def drain(acc: List[Int]): List[Int] ! Async =
      ch.uncons(c).flatMap {
        case Some((a, _)) => drain(a :: acc)
        case None => pure(acc.reverse)
      }
    Async.runAsync(drain(Nil)).map(v => assertEquals(v, List(1, 2)))
  }
}
