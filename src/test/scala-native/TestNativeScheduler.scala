package okay

/**
 * specs/cross-platform-async.md, native-scheduler-pool — Native
 * only (CanBlock-based; the cross suite deliberately never blocks).
 */
class TestNativeScheduler extends munit.FunSuite {

  test("pool: more fibers than workers all complete — a suspended fiber frees its worker") {
    given Scheduler = Schedulers.pool(2)
    // Async.sleep suspends via Timer, not a real block: its
    // continuation resumes on the TIMER's own thread, so the pool
    // worker that started the fiber is free again almost at once —
    // 20 fibers on 2 workers must not deadlock
    val fibers = (1 to 20).map(i => Async.spawn(Async.sleep(5).map(_ => i)))
    val results = fibers.map(_.join())
    assertEquals(results, (1 to 20).toVector)
  }

  test("pool: par runs both sides to completion") {
    given Scheduler = Schedulers.pool(2)
    assertEquals(Async.par(async(1), async(2)).runWith, (1, 2))
  }

  test("threads: one fiber per thread still works, unchanged") {
    given Scheduler = Schedulers.threads
    assertEquals(Async.spawn(async(3)).join(), 3)
  }

  test("pool: a fiber cancelled before its turn never runs") {
    given Scheduler = Schedulers.pool(1)
    // occupy the single worker so the second fork stays QUEUED
    val block = new Object
    var release = false
    val occupying = Async.spawn(async {
      block.synchronized { while !release do block.wait() }
      0
    })
    @volatile var ran = false
    val queued = Async.spawn(async { ran = true; 1 })
    queued.cancel()
    block.synchronized { release = true; block.notifyAll() }
    occupying.join(): Unit
    // give the worker a moment to reach (and skip) the cancelled task
    Thread.sleep(50)
    assert(!ran, "a task cancelled while still queued must never run")
  }
}
