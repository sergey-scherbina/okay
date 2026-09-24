package okay2.platform

import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters._
import okay2._
import okay2.async._

/** The laws every member of the scheduler family owes, and the soaks
 * of the defects they were written for — the Scala 3 core's
 * TestSchedulerLaws (backlog okay2-scheduler-laws) */
class TestSchedulerLaws extends SchedulerFamily {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  each("every answer joined exactly once") { sch =>
    implicit val S: Scheduler = sch
    val n = 10000
    val sum = (0 until n).map(i => Async.spawn(Async(i.toLong))).foldLeft(0L)((a, f) => a + f.join())
    assertEquals(sum, (0 until n).map(_.toLong).sum)
  }

  each("a failing fiber fails its join, as Left") { sch =>
    implicit val S: Scheduler = sch
    val f = Async.spawn(Async[Int](throw new IllegalStateException("boom")))
    assert(f.joinEither().left.exists(_.getMessage == "boom"))
  }

  each("onComplete fires once, before or after the answer") { sch =>
    implicit val S: Scheduler = sch
    val f = Async.spawn(Async(1))
    assertEquals(f.join(), 1)
    val after = new AtomicInteger()
    f.onComplete(_ => { val _ = after.incrementAndGet() })
    assertEquals(after.get, 1)
    val gate = new CountDownLatch(1)
    val g = Async.spawn(Async { gate.await(); 2 })
    val before = new AtomicInteger()
    val fired = new CountDownLatch(1)
    g.onComplete { _ => val _ = before.incrementAndGet(); fired.countDown() }
    gate.countDown()
    assertEquals(g.join(), 2)
    // the waiters are a stack: a join can return before a callback
    // registered EARLIER has run — the law is that it runs, once
    assert(fired.await(5, TimeUnit.SECONDS), "a callback registered before completion never fired")
    Thread.sleep(20)
    assertEquals(before.get, 1)
  }

  each("cancel wins the race it is in: a value delivered after cancel is never the answer") { sch =>
    implicit val S: Scheduler = sch
    val k = new AtomicReference[Either[Throwable, Int] => Unit](null)
    val go = new AtomicBoolean(false)
    val f = Async.spawn(Async.await[Int] { cb =>
      k.set(cb)
      while (!go.get()) Thread.onSpinWait()   // uninterruptible on purpose
      () => ()
    })
    while (k.get() == null) Thread.onSpinWait()
    f.cancel()
    k.get()(Right(5))
    go.set(true)
    val answers = new ConcurrentLinkedQueue[Either[Throwable, Int]]()
    f.onComplete(r => { val _ = answers.offer(r) })
    Thread.sleep(50)
    assert(!answers.contains(Right(5)), "a value delivered after cancel became the fiber's answer")
    assert(answers.size <= 1, s"answered ${answers.size} times")
  }

  each("cancel of a parked fiber: the late answer never becomes the fiber's") { sch =>
    implicit val S: Scheduler = sch
    val k = new AtomicReference[Either[Throwable, Int] => Unit](null)
    val f = Async.spawn(Async.await[Int](cb => { k.set(cb); () => () }))
    while (k.get() == null) Thread.onSpinWait()
    f.cancel()
    val answers = new ConcurrentLinkedQueue[Either[Throwable, Int]]()
    f.onComplete(r => { val _ = answers.offer(r) })
    k.get()(Right(5))
    Thread.sleep(80)
    assert(answers.size <= 1, s"answered ${answers.size} times")
    assert(!answers.contains(Right(5)), "the late answer became the fiber's")
  }

  each("cancel ANSWERS the fiber: a join on a cancelled fiber returns") { sch =>
    implicit val S: Scheduler = sch
    val f = Async.spawn(Async.await[Int](_ => () => ()))
    Thread.sleep(20)
    f.cancel()
    val answered = new CountDownLatch(1)
    f.onComplete(_ => answered.countDown())
    assert(answered.await(5, TimeUnit.SECONDS), "a cancelled fiber never answered — a join on it would wait for ever")
    assert(f.joinEither().isLeft, "a cancelled fiber answered with a value")
  }

  each("par: both sides, on their own thread of control") { sch =>
    implicit val S: Scheduler = sch
    assertEquals(Async.par(Async(1), Async(2)).runWith, (1, 2))
  }

  test("close stops the workers: a scheduler owns threads and gives them back") {
    val sch = Schedulers.own.workers(3).build
    val mine = s"okay-own-${sch.id}-"
    implicit val S: Scheduler = sch
    assertEquals(Async.spawn(Async(1)).join(), 1)
    sch.close()
    val deadline = System.nanoTime() + 5000000000L
    var live = 3
    while (live > 0 && System.nanoTime() < deadline) {
      live = Thread.getAllStackTraces.keySet.asScala.count(t => t.getName.startsWith(mine) && t.isAlive)
      if (live > 0) Thread.sleep(20)
    }
    assert(clue(live) == 0, "workers still running after close()")
  }

  /** CONSERVATION of the work-stealing deque, stated where the defect of
   * own-long-join-deadlock lived: every task pushed comes out exactly
   * once — from the owner's `pop` or a thief's `steal` — while the owner
   * grows the buffer from 4 many times over under six thieves */
  test("work-stealing deque — nothing is lost while it grows under thieves") {
    val rounds = 60
    val n = 4000
    val thieves = 6
    var round = 0
    while (round < rounds) {
      round += 1
      val d = new Schedulers.Deque(4)
      val out = java.util.concurrent.ConcurrentHashMap.newKeySet[Schedulers.DriveTask[_]]()
      val dup = new AtomicInteger(0)
      val go = new CountDownLatch(1)
      val done = new CountDownLatch(thieves)
      def record(t: Schedulers.DriveTask[_]): Unit = if (!out.add(t)) { val _ = dup.incrementAndGet() }
      val ts = (0 until thieves).map { _ =>
        val th = new Thread(() => {
          go.await()
          var idle = 0
          while (idle < 2000) {
            val t = d.steal()
            if (t != null) { record(t); idle = 0 } else idle += 1
          }
          done.countDown()
        })
        th.start(); th
      }
      val pushed = (0 until n).map(_ => new Schedulers.DriveTask[Int](() => pure[Async, Int](1)))
      go.countDown()
      var i = 0
      while (i < n) { d.push(pushed(i)); i += 1 }
      // the owner drains its own end too, exactly as a worker does
      var t = d.pop()
      while (t != null) { record(t); t = d.pop() }
      done.await()
      ts.foreach(_.join())
      var s = d.steal()
      while (s != null) { record(s); s = d.steal() }
      assertEquals(dup.get, 0, s"round $round: a task came out twice")
      assertEquals(out.size, n, s"round $round: ${n - out.size} task(s) LOST — a steal consumed an index without delivering it")
    }
  }

  test("no lost wake — one worker, a fork after it parked") {
    val sch = Schedulers.own.workers(1).build
    implicit val S: Scheduler = sch
    Thread.sleep(200)                      // the worker gives up and parks
    assertEquals(Async.spawn(Async(42)).join(), 42)   // a swallowed wake would hang here
    sch.close()
  }

  /** own-lost-wakeup: a worker blocked for good in a raw call must not
   * count as awake for the forks that come after it */
  test("platform: a fiber blocked for good in a raw call does not hide a later fork from outside") {
    val sch = Schedulers.platform
    implicit val S: Scheduler = sch
    val gate = new CountDownLatch(1)
    val blocker = Async.spawn(Async { gate.await(); 0 })
    Thread.sleep(50)
    val done = new CountDownLatch(1)
    Async.spawn(Async(1)).onComplete(_ => done.countDown())
    assert(done.await(3, TimeUnit.SECONDS), "a fork from outside was never run: the blocked worker counted as awake, so no sleeper was woken")
    gate.countDown(); val _ = blocker.join(); sch.close()
  }

  test("platform: a child forked by a fiber that then blocks for good still runs") {
    val sch = Schedulers.platform
    implicit val S: Scheduler = sch
    val gate = new CountDownLatch(1)
    val done = new CountDownLatch(1)
    Thread.sleep(50)
    val parent = Async.spawn {
      val _ = Async.spawn(Async(done.countDown()))   // onto the parent's own deque, no signal
      Async { gate.await(); 0 }                       // and the parent never comes back for it
    }
    assert(done.await(3, TimeUnit.SECONDS), "a child on a blocked worker's deque was never stolen")
    gate.countDown(); val _ = parent.join(); sch.close()
  }

  test("stuck-check: a parked worker is woken before a new one is started") {
    // one worker, one overflow slot: the first stall may start the
    // overflow worker, the second must WAKE it
    val sch = Schedulers.adaptive.workers(1).watched(scala.concurrent.duration.Duration(20, "ms"), overflow = 1).build
    implicit val S: Scheduler = sch
    val gate = new CountDownLatch(1)
    val blocker = Async.spawn(Async { gate.await(); 0 })
    def answered(): Boolean = {
      val done = new CountDownLatch(1)
      Async.spawn(Async(1)).onComplete(_ => done.countDown())
      done.await(3, TimeUnit.SECONDS)
    }
    assert(answered(), "the overflow worker was never started")
    assert(answered(), "the overflow worker parked and was never woken: the stuck-check grew instead of unparking")
    gate.countDown(); val _ = blocker.join(); sch.close()
  }

  test("fairness — a submitter is answered while a worker runs a burst") {
    val sch = Schedulers.own.workers(4).build
    implicit val S: Scheduler = sch
    val started = new CountDownLatch(1)
    val burst = Async.spawn {
      pure[Async, Unit](started.countDown()).flatMap { _ =>
        val fs = (0 until 2000).map(i => Async.spawn(Async { var s = 0; var j = 0; while (j < 20000) { s += (i ^ j); j += 1 }; s }))
        fs.foldLeft(pure[Async, Long](0L))((acc, f) => acc.flatMap(a => f.joinAsync.map(a + _)))
      }
    }
    started.await()
    val answered = new CountDownLatch(1)
    Async.spawn(Async(7)).onComplete(_ => answered.countDown())
    assert(answered.await(20, TimeUnit.SECONDS), "a submitter's fiber waited behind the whole burst")
    val _ = burst.join()
    sch.close()
  }
}
