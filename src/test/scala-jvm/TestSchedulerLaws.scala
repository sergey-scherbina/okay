package okay

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}

/** The laws every member of the scheduler family owes
 * (specs/schedulers.md), run against each member the way
 * `TestManyToMany` runs its law against each buffer. */
class TestSchedulerLaws extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private val members: List[(String, Scheduler)] = List(
    "loom" -> Schedulers.loom,
    "drive" -> Schedulers.drive(),
    "own" -> Schedulers.own.workers(4).build,
    "own.forShortTasks" -> Schedulers.own.workers(4).forShortTasks.build,
    "own.forLongTasks" -> Schedulers.own.workers(4).forLongTasks.build,
    "adaptive" -> Schedulers.adaptive.workers(2).build)

  private def each(name: String)(law: Scheduler => Unit): Unit =
    members.foreach { case (member, sch) => test(s"$name — $member") { law(sch) } }

  each("every answer joined exactly once") { sch =>
    given Scheduler = sch
    val n = 10000
    val sum = (0 until n).map(i => Async.spawn(async(i.toLong))).foldLeft(0L)((a, f) => a + f.join())
    assertEquals(sum, (0 until n).map(_.toLong).sum)
  }

  each("a failing fiber fails its join, as Left") { sch =>
    given Scheduler = sch
    val f = Async.spawn(async[Int](throw new IllegalStateException("boom")))
    assert(f.joinEither().left.exists(_.getMessage == "boom"))
  }

  each("onComplete fires once, before or after the answer") { sch =>
    given Scheduler = sch
    val f = Async.spawn(async(1))
    assertEquals(f.join(), 1)
    val after = AtomicInteger()
    f.onComplete(_ => { val _ = after.incrementAndGet() })
    assertEquals(after.get, 1)
    val gate = CountDownLatch(1)
    val g = Async.spawn(async { gate.await(); 2 })
    val before = AtomicInteger()
    val fired = CountDownLatch(1)
    g.onComplete { _ => val _ = before.incrementAndGet(); fired.countDown() }
    gate.countDown()
    assertEquals(g.join(), 2)
    // the waiters are a stack, so a join can return before a callback
    // registered EARLIER has run — the law is that it runs, once
    assert(fired.await(5, TimeUnit.SECONDS), "a callback registered before completion never fired")
    Thread.sleep(20)
    assertEquals(before.get, 1)
  }

  each("cancel of a parked fiber: the late answer never becomes the fiber's") { sch =>
    given Scheduler = sch
    val k = AtomicReference[Either[Throwable, Int] => Unit](null)
    val f = Async.spawn(Async.await[Int](cb => { k.set(cb); () => () }))
    while k.get() == null do Thread.onSpinWait()
    f.cancel()
    // a cancelled fiber may finish as a failure (loom interrupts the
    // thread) or never finish at all (the drive stops between
    // operations); what no member may do is ANSWER with the value that
    // arrived after the cancel, or answer twice
    val answers = java.util.concurrent.ConcurrentLinkedQueue[Either[Throwable, Int]]()
    f.onComplete(r => { val _ = answers.offer(r) })
    k.get()(Right(5))
    Thread.sleep(80)
    assert(answers.size <= 1, s"answered ${answers.size} times")
    assert(!answers.contains(Right(5)), "the late answer became the fiber's")
  }

  each("par: both sides, on their own thread of control") { sch =>
    given Scheduler = sch
    assertEquals(Async.par(async(1), async(2)).runWith, (1, 2))
  }

  test("no lost wake — one worker, a fork after it parked") {
    given Scheduler = Schedulers.own.workers(1).build
    Thread.sleep(200)                      // the worker gives up and parks
    val f = Async.spawn(async(42))
    assertEquals(f.join(), 42)             // a park that swallowed the wake would hang here
  }

  test("fairness — a submitter is answered while a worker runs a burst") {
    given Scheduler = Schedulers.own.workers(4).build
    val started = CountDownLatch(1)
    val burst = Async.spawn {
      pure[Async, Unit](started.countDown()).flatMap { _ =>
        val fs = (0 until 2000).map(i => Async.spawn(async { var s = 0; var j = 0; while j < 20000 do { s += (i ^ j); j += 1 }; s }))
        fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
      }
    }
    started.await()
    val mine = Async.spawn(async(7))
    val answered = CountDownLatch(1)
    mine.onComplete(_ => answered.countDown())
    assert(answered.await(20, TimeUnit.SECONDS), "a submitter's fiber waited behind the whole burst")
    val _ = burst.join()
  }

  test("adaptive — a fiber that BLOCKS inside a worker does not stop the program") {
    // one worker, and the first fiber blocks on a channel the second fills:
    // without the stuck-check this is a deadlock, which is why `own`
    // is not the default and `adaptive` exists
    given Scheduler = Schedulers.adaptive.workers(1).watched(scala.concurrent.duration.Duration(50, "ms")).build
    val ch = Channel[Int](4)
    val reader = Async.spawn(async {
      val filler = Async.spawn(async { ch.sendBlocking(9); 0 })
      val got = ch.receiveBlocking()
      val _ = filler.join()
      got
    })
    assertEquals(reader.joinEither().fold(e => throw e, identity), Some(9))
  }
}
