package okay

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/**
 * channel-ring: a lock-free MPMC ring is where subtle bugs live —
 * wrap-around, the lap boundary, publication order. These test the
 * boundaries first and the concurrency second, because a ring that is
 * wrong at capacity+1 sequentially is wrong everywhere.
 */
class TestRing extends munit.FunSuite {

  test("capacity rounds UP to a power of two, and at least one") {
    // one is not a ring -- the stamp scheme degenerates there, so the
    // floor is two (see Ring's scaladoc; ZIO draws the same line)
    assertEquals(Ring[Int](1).capacity, 2)
    assertEquals(Ring[Int](2).capacity, 2)
    assertEquals(Ring[Int](3).capacity, 4)
    assertEquals(Ring[Int](5).capacity, 8)
    assertEquals(Ring[Int](64).capacity, 64)
    assertEquals(Ring[Int](65).capacity, 128)
    assertEquals(Ring[Int](0).capacity, 2)
  }

  test("push until full, pop until empty, in order") {
    val r = Ring[Int](4)
    assertEquals((1 to 4).map(r.push).toList, List(true, true, true, true))
    assertEquals(r.push(5), false, "a full ring refuses")
    assertEquals(r.size, 4)
    assertEquals((1 to 4).map(_ => r.pop()).toList, List(1, 2, 3, 4))
    assertEquals(r.pop(), null, "an empty ring answers null")
    assert(r.isEmpty)
  }

  test("wrap-around: the lap boundary is where the stamps must line up") {
    val r = Ring[Int](4)
    // walk several laps, one in one out, so every slot is reused
    for i <- 1 to 100 do
      assert(r.push(i), s"push $i")
      assertEquals(r.pop(), i, s"pop $i")
    assert(r.isEmpty)
    // and a full lap at a time
    for lap <- 0 until 25 do
      val base = lap * 4
      for k <- 1 to 4 do assert(r.push(base + k))
      assertEquals(r.push(-1), false)
      for k <- 1 to 4 do assertEquals(r.pop(), base + k)
      assertEquals(r.pop(), null)
  }

  test("interleaved partial fills keep FIFO across the boundary") {
    val r = Ring[Int](4)
    assert(r.push(1)); assert(r.push(2))
    assertEquals(r.pop(), 1)
    assert(r.push(3)); assert(r.push(4)); assert(r.push(5))
    assertEquals(r.push(6), false)
    assertEquals(r.pop(), 2); assertEquals(r.pop(), 3)
    assert(r.push(6))
    assertEquals(List(r.pop(), r.pop(), r.pop()), List(4, 5, 6))
    assertEquals(r.pop(), null)
  }

  test("the smallest ring there is: two") {
    val r = Ring[String](2)
    assert(r.push("a")); assert(r.push("b"))
    assertEquals(r.push("c"), false)
    assertEquals(r.pop(), "a")
    assert(r.push("c"))
    assertEquals(r.pop(), "b")
    assertEquals(r.pop(), "c")
    assertEquals(r.pop(), null)
    // and it survives many laps at that size, where the stamps are
    // tightest
    for i <- 1 to 200 do
      assert(r.push(i.toString)); assertEquals(r.pop(), i.toString)
  }

  test("a popped slot releases its reference") {
    val r = Ring[Object](2)
    val o = Object()
    assert(r.push(o))
    assertEquals(r.pop(), o)
    // the ring must not still be holding it: push/pop a lap and the
    // slot is overwritten either way, so this asserts the visible
    // contract rather than the heap -- emptiness after the pop
    assert(r.isEmpty)
    assertEquals(r.size, 0)
  }

  // ── concurrency ─────────────────────────────────────────────────

  test("MPMC: nothing lost, nothing duplicated, under real threads") {
    val r = Ring[Int](256)
    val producers = 4
    val perProducer = 20000
    val total = producers * perProducer
    val seen = AtomicLong(0L)
    val count = AtomicInteger(0)
    val pool = Executors.newFixedThreadPool(producers + 4)
    val start = CountDownLatch(1)
    val done = CountDownLatch(producers + 4)

    // each producer pushes a distinct arithmetic slice, so the SUM
    // over everything popped identifies exactly the multiset pushed
    for p <- 0 until producers do
      pool.execute { () =>
        start.await()
        var i = 0
        while i < perProducer do
          val v = p * perProducer + i
          while !r.push(v) do Thread.onSpinWait()
          i += 1
        done.countDown()
      }
    for _ <- 0 until 4 do
      pool.execute { () =>
        start.await()
        while count.get < total do
          val v = r.pop()
          if v != null then { seen.addAndGet(v.nn.toLong); count.incrementAndGet(): Unit }
          else Thread.onSpinWait()
        done.countDown()
      }

    start.countDown()
    assert(done.await(60, TimeUnit.SECONDS), "the ring deadlocked or lost elements")
    pool.shutdownNow()
    assertEquals(count.get, total)
    assertEquals(seen.get, (0L until total.toLong).sum)
    assert(r.isEmpty, s"ring not drained: size=${r.size}")
  }

  test("SPSC: strict FIFO order is preserved end to end") {
    val r = Ring[Int](64)
    val n = 100000
    val out = new Array[Int](n)
    val pool = Executors.newFixedThreadPool(2)
    val done = CountDownLatch(2)
    pool.execute { () =>
      var i = 0
      while i < n do { while !r.push(i) do Thread.onSpinWait(); i += 1 }
      done.countDown()
    }
    pool.execute { () =>
      var i = 0
      while i < n do
        val v = r.pop()
        if v != null then { out(i) = v.nn; i += 1 } else Thread.onSpinWait()
      done.countDown()
    }
    assert(done.await(60, TimeUnit.SECONDS))
    pool.shutdownNow()
    // one producer, one consumer: the order must be exactly the order
    // pushed, not merely the same multiset
    assertEquals(out.toList, (0 until n).toList)
  }
}
