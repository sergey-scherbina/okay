package okay

import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}
import scala.jdk.CollectionConverters.*

/** The laws every member of the scheduler family owes
 * (specs/schedulers.md), run against each member the way
 * `TestManyToMany` runs its law against each buffer. */
class TestSchedulerLaws extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(3, "min")

  private val owned: List[(String, Schedulers.Running)] = List(
    "own" -> Schedulers.own.workers(4).build,
    "own.forShortTasks" -> Schedulers.own.workers(4).forShortTasks.build,
    "own.forLongTasks" -> Schedulers.own.workers(4).forLongTasks.build,
    "adaptive" -> Schedulers.adaptive.workers(2).build)

  private val members: List[(String, Scheduler)] =
    List("loom" -> Schedulers.loom, "drive" -> Schedulers.drive()) ++ owned

  override def afterAll(): Unit = owned.foreach(_._2.close())

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

  each("cancel wins the race it is in: a value delivered after cancel is never the answer") { sch =>
    given Scheduler = sch
    val k = AtomicReference[Either[Throwable, Int] => Unit](null)
    val go = java.util.concurrent.atomic.AtomicBoolean(false)
    val f = Async.spawn(Async.await[Int] { cb =>
      k.set(cb)
      while !go.get() do Thread.onSpinWait()   // uninterruptible on purpose
      () => ()
    })
    while k.get() == null do Thread.onSpinWait()
    f.cancel()
    k.get()(Right(5))
    go.set(true)
    val answers = java.util.concurrent.ConcurrentLinkedQueue[Either[Throwable, Int]]()
    f.onComplete(r => { val _ = answers.offer(r) })
    Thread.sleep(50)
    assert(!answers.contains(Right(5)),
      "a value delivered after cancel became the fiber's answer")
    assert(answers.size <= 1, s"answered ${answers.size} times")
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

  each("cancel ANSWERS the fiber: a join on a cancelled fiber returns") { sch =>
    given Scheduler = sch
    val f = Async.spawn(Async.await[Int](_ => () => ()))
    Thread.sleep(20)
    f.cancel()
    val answered = CountDownLatch(1)
    f.onComplete(_ => answered.countDown())
    assert(answered.await(5, TimeUnit.SECONDS), "a cancelled fiber never answered — a join on it would wait for ever")
    assert(f.joinEither().isLeft, "a cancelled fiber answered with a value")
  }

  each("par: both sides, on their own thread of control") { sch =>
    given Scheduler = sch
    assertEquals(Async.par(async(1), async(2)).runWith, (1, 2))
  }

  test("close stops the workers: a scheduler owns threads and gives them back") {
    val sch = Schedulers.own.workers(3).build
    val mine = s"okay-own-${sch.id}-"
    given Scheduler = sch
    assertEquals(Async.spawn(async(1)).join(), 1)
    sch.close()
    val deadline = System.nanoTime() + 5_000_000_000L
    var live = 3
    while live > 0 && System.nanoTime() < deadline do
      live = Thread.getAllStackTraces.keySet.asScala.count(t => t.getName.startsWith(mine) && t.isAlive)
      if live > 0 then Thread.sleep(20)
    assert(clue(live) == 0, "workers still running after close()")
  }

  /** The law the own-long-join-deadlock violated (2026-09-07).
   *
   * A whole-scheduler test cannot gate this: measured, the hang shows
   * up about once in twenty thousand fork/join operations, and only
   * with several JVMs contending — a soak, not a gate. The defect
   * itself is one line of the work-stealing deque, so the law is
   * stated where it lives.
   *
   * CONSERVATION: every task pushed comes out exactly once, from the
   * owner's `pop` or from a thief's `steal`. The shape is the one
   * that broke: an owner pushing far past the initial capacity, so
   * the buffer grows repeatedly WHILE thieves are reading through it.
   * Before the fix a thief could win its `top` CAS having read a slot
   * another steal had cleared through a since-replaced array — the
   * index consumed, the task never delivered, and the fiber waiting
   * on it parked for ever.
   */
  test("work-stealing deque — nothing is lost while it grows under thieves") {
    val rounds  = 60
    val n       = 4000          // 4 -> 8 -> ... many grows from a tiny buffer
    val thieves = 6
    var round = 0
    while round < rounds do
      round += 1
      val d    = Schedulers.Deque(4)
      val out  = java.util.concurrent.ConcurrentHashMap.newKeySet[Schedulers.DriveTask[?]]()
      val dup  = AtomicInteger(0)
      val go   = CountDownLatch(1)
      val done = CountDownLatch(thieves)

      def record(t: Schedulers.DriveTask[?]): Unit =
        if !out.add(t) then { val _ = dup.incrementAndGet() }

      val ts = (0 until thieves).map { _ =>
        val th = Thread.ofPlatform().unstarted(() => {
          go.await()
          var idle = 0
          while idle < 2000 do
            val t = d.steal()
            if t != null then { record(t); idle = 0 } else idle += 1
          done.countDown()
        })
        th.start(); th
      }

      val pushed = (0 until n).map(_ => Schedulers.DriveTask[Int](() => pure[Async, Int](1)))
      go.countDown()
      var i = 0
      while i < n do { d.push(pushed(i)); i += 1 }
      // the owner drains its own end too, exactly as Worker.run does
      var t = d.pop()
      while t != null do { record(t.nn); t = d.pop() }
      done.await()
      ts.foreach(_.join())
      // whatever the thieves were still holding when they gave up
      var s = d.steal()
      while s != null do { record(s.nn); s = d.steal() }

      assertEquals(dup.get, 0, s"round $round: a task came out twice")
      assertEquals(out.size, n, s"round $round: ${n - out.size} task(s) LOST — a steal consumed an index without delivering it")
    ()
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
      val filler = Async.spawn(async { val _ = ch.sendBlocking(9); 0 })
      val got = ch.receiveBlocking()
      val _ = filler.join()
      got
    })
    assertEquals(reader.joinEither().fold(e => throw e, identity), Some(9))
  }
}
