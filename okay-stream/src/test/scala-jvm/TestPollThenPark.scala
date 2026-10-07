package okay


import okay.freer.*
import okay.freer.given
import java.util.concurrent.atomic.AtomicInteger

/**
 * POLL, THEN PARK at the RUNNERS (drive-poll-then-park,
 * specs/ready-merge.md): an `Async.Await` that carries a poll is asked
 * before it is registered — by the given `Wait` on a blocking runner's
 * own thread, once and without a wait on the callback drive, whose
 * thread after its first callback is whoever woke it.
 */
class TestPollThenPark extends munit.FunSuite {

  /** an Await whose poll says "nothing yet" `misses` times, then
   * answers; its registration answers at once and counts itself */
  private final class Counted(misses: Int, x: Int):
    val polls = AtomicInteger(0)
    val registered = AtomicInteger(0)
    val op: Int ! Async = okay.freer.effect[Async, Int](Async.Await[Int](
      k => { registered.incrementAndGet(); k(Right(x)); () => () },
      () => if polls.incrementAndGet() > misses then Right(x) else null))

  /** a test's platform: the rungs counted, none of them slept */
  private final class Counting extends Pause:
    var spins, yields, nanos, blocks = 0
    def threads = true
    def spin(): Unit = spins += 1
    def yieldNow(): Unit = yields += 1
    def nano(): Unit = nanos += 1
    def block(): Unit = blocks += 1

  test("the blocking runner asks the poll by the given Wait: answered on the yield rung, nothing registered") {
    val c = Counted(120, 7)
    assertEquals(c.op.runWith, 7)
    assertEquals((c.registered.get, c.polls.get), (0, 121))
  }

  test("the blocking runner climbs the whole ladder, then parks: a counting platform sees 100/50/4/1") {
    val rungs = Counting()
    given Pause = rungs
    val c = Counted(Int.MaxValue, 7)
    assertEquals(c.op.runWith, 7)
    assertEquals((c.registered.get, c.polls.get), (1, 100 + 50 + 4))
    assertEquals((rungs.spins, rungs.yields, rungs.nanos, rungs.blocks), (100, 50, 4, 1))
  }

  test("the blocking runner's wait is the given: Register parks at once, Spin(10) polls ten times") {
    locally {
      given Wait = Wait.Register
      val c = Counted(Int.MaxValue, 7)
      assertEquals(c.op.runWith, 7)
      assertEquals((c.registered.get, c.polls.get), (1, 0))
    }
    locally {
      given Wait = Wait.Spin(10)
      val c = Counted(Int.MaxValue, 7)
      assertEquals(c.op.runWith, 7)
      assertEquals((c.registered.get, c.polls.get), (1, 10))
    }
  }

  test("the callback drive polls ONCE and never waits: an answer in place is taken, a miss registers") {
    import scala.concurrent.Await as SAwait
    import scala.concurrent.duration.*
    val hit = Counted(0, 7)
    assertEquals(SAwait.result(Async.runAsync(hit.op), 5.seconds), 7)
    assertEquals((hit.registered.get, hit.polls.get), (0, 1))
    val miss = Counted(Int.MaxValue, 7)
    assertEquals(SAwait.result(Async.runAsync(miss.op), 5.seconds), 7)
    assertEquals((miss.registered.get, miss.polls.get), (1, 1))
  }

  test("a drained channel consumed by the blocking runner takes what a producer sends during the wait, without a registration") {
    val ch = Channel[Int](16)
    val registered = AtomicInteger(0)
    // a spy on the channel's own registration path: the drained source
    // registers only when its poll came back empty for the whole wait
    val spied: Source[Int] =
      okay.freer.effect[Writer % Int + Async, Chunk[Int]](Async.Await[Chunk[Int]](
        k => { registered.incrementAndGet(); ch.receiveManyAsync(64)(k); () => () },
        () => ch.receiveManyNow(64))).flatMap { got =>
          if got.isEmpty then okay.freer.pure(())
          else okay.freer.effect[Writer % Int + Async, Unit](Writer(got(0)))
        }
    (1 to 3).foreach(i => assert(ch.offer(i)))
    val out = spied.runCollect.runWith
    assertEquals(out, Vector(1))
    assertEquals(registered.get, 0)
  }
}
