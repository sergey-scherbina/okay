package okay2.platform

import java.util.concurrent.atomic.AtomicInteger
import okay2._
import okay2.async._

/** okay2-forklong (specs/adaptive-chunked-merge-cost.md, the Scala 3
 * core's `Scheduler.forkLong`): a fork the caller declares long gets a
 * worker at once. okay2's `own` has no monitor, so without it a second
 * long fiber forked from outside waits for the first to END */
class TestForkLong extends munit.FunSuite {

  /** two fibers forked from OUTSIDE, the second once the first's worker
   * is awake and busy, each spinning until the other has started (at
   * most `patience` ns): whether they MET */
  private def outsidePairMeets(sch: Scheduler, long: Boolean, patience: Long): Boolean = {
    val started = new AtomicInteger()
    val met = new AtomicInteger()
    def body(): Unit = {
      val _ = started.incrementAndGet()
      val end = System.nanoTime() + patience
      while (started.get < 2 && System.nanoTime() < end) Thread.onSpinWait()
      if (started.get >= 2) { val _ = met.incrementAndGet() }
    }
    def go(): Fiber[Unit] =
      if (long) sch.forkLong(() => Async(body())) else sch.fork(() => Async(body()))
    Thread.sleep(20) // every worker parked
    val a = go()
    Thread.sleep(10) // a's worker is awake and inside `body`
    val b = go()
    a.join(); b.join()
    met.get == 2
  }

  test("own: a long fiber forked from outside beside a busy one gets a worker at once (forkLong)") {
    val sch = Schedulers.own.workers(4).build
    try assert(outsidePairMeets(sch, long = true, patience = 2000000000L), "forkLong: the second fiber waited behind the first")
    finally sch.close()
  }

  test("own: the same pair forked with plain fork does not meet (the case forkLong exists for)") {
    val sch = Schedulers.own.workers(4).build
    try assert(!outsidePairMeets(sch, long = false, patience = 300000000L), "fork alone woke a second worker: the law above proves nothing")
    finally sch.close()
  }

  test("forkLong is fork on a scheduler that does not override it") {
    var forked = 0
    val plain = new Scheduler {
      def fork[A](prog: () => A ! Async): Fiber[A] = { forked += 1; Schedulers.threads.fork(prog) }
    }
    assertEquals(plain.forkLong(() => Async(7)).join(), 7)
    assertEquals(forked, 1)
  }
}
