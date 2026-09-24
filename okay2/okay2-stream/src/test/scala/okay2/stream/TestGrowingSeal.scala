package okay2.stream

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import okay2.async.CanBlock
import okay2.platform._

/**
 * THE CLOSE-WAKEUP HANG, made deterministic (okay2-channel-close-wakeup).
 * TestChannelLaws law 1b hung twice under load with a receiver parked
 * after close: `growing` sealed its end mark into the RING (one part),
 * and a producer already past its open check then grew it into an
 * AdaptiveFifo whose extra part never got an end mark — the receiver
 * waits for `parts` end marks and parks for good.
 */
class TestGrowingSeal extends munit.FunSuite {

  private val cb: CanBlock = implicitly[CanBlock]

  test("a close that sealed the ring is not undone by a growth after it: the receiver reaches the end") {
    val g = new Growing[Any](new Ring[Any](2), 4, () => new Ring[Any](2))
    val c = new SentinelChannel[Int](g)
    assert(c.offer(1))
    c.close()                                  // the end mark lands in the ring: [1, end]
    // two producers that passed their open check BEFORE the close: a
    // deciding push publishes a void, and the second one, finding the
    // ring full behind another thread, is what grows a `Growing`
    val closing = new AtomicBoolean(true)
    val void: Any = new Mark(false)
    def inFlight(): Unit = { val t = new Thread(() => { val _ = g.pushDecidingAt(0, 7, closing, void) }); t.start(); t.join() }
    inFlight(); inFlight()
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val drained = Future {
      Iterator.continually(c.receiveBlocking()(cb)).takeWhile(_.isDefined).flatten.toList
    }
    assertEquals(Await.result(drained, 5.seconds), List(1), s"parts=${g.parts}")
  }
}
