package okay

/** `Schedulers.drive`: fibers as continuations on the pool. The laws
 * a Fiber owes — every answer once, a failure as Left, onComplete
 * before and after, cancel drops a late answer — on the fused
 * `DriveTask`. */
class TestDriveScheduler extends munit.FunSuite {
  given Scheduler = Schedulers.drive()

  test("10 000 fibers, every answer joined once") {
    val sum = (0 until 10000).map(i => Async.spawn(async(i.toLong))).foldLeft(0L)((a, f) => a + f.join())
    assertEquals(sum, (0 until 10000).map(_.toLong).sum)
  }

  test("a failing fiber fails its join, as Left") {
    val f = Async.spawn(async[Int](throw new IllegalStateException("boom")))
    assert(f.joinEither().left.exists(_.getMessage == "boom"))
  }

  test("onComplete fires once, whether registered before or after the answer") {
    val f = Async.spawn(async(1))
    assertEquals(f.join(), 1)
    var after = 0
    f.onComplete(_ => after += 1)
    assertEquals(after, 1)
    val gate = new java.util.concurrent.CountDownLatch(1)
    val g = Async.spawn(async { gate.await(); 2 })
    val before = new java.util.concurrent.atomic.AtomicInteger
    g.onComplete(_ => { val _ = before.incrementAndGet() })
    gate.countDown()
    assertEquals(g.join(), 2)
    assertEquals(before.get, 1)
  }

  test("cancel of a parked fiber: the late answer is dropped, nobody is resumed") {
    val k = new java.util.concurrent.atomic.AtomicReference[Either[Throwable, Int] => Unit](null)
    val f = Async.spawn(Async.await[Int](cb => { k.set(cb); () => () }))
    while k.get == null do Thread.onSpinWait()
    f.cancel()
    val done = new java.util.concurrent.atomic.AtomicBoolean(false)
    f.onComplete(_ => done.set(true))
    k.get(Right(5))
    Thread.sleep(50)
    assert(!done.get)
  }

  test("par on the pool: both sides on their own task") {
    val (a, b) = Async.par(async(1), async(2)).runWith
    assertEquals((a, b), (1, 2))
  }
}
