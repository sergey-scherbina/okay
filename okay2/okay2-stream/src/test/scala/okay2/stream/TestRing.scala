package okay2.stream

import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/**
 * okay-stream's TestRing (okay2 spec stage 29): the boundaries first and
 * the concurrency second, because a ring that is wrong at capacity+1
 * sequentially is wrong everywhere. `Ring[Any]` where Scala 3 writes
 * `Ring[Int]`: a buffer's element type admits null here (`A >: Null`).
 */
class TestRing extends munit.FunSuite {

  test("capacity rounds UP to a power of two, and at least two") {
    assertEquals(new Ring[Any](1).capacity, 2)
    assertEquals(new Ring[Any](2).capacity, 2)
    assertEquals(new Ring[Any](3).capacity, 4)
    assertEquals(new Ring[Any](5).capacity, 8)
    assertEquals(new Ring[Any](64).capacity, 64)
    assertEquals(new Ring[Any](65).capacity, 128)
    assertEquals(new Ring[Any](0).capacity, 2)
  }

  test("push until full, pop until empty, in order") {
    val r = new Ring[Any](4)
    assertEquals((1 to 4).map(r.push).toList, List(true, true, true, true))
    assertEquals(r.push(5), false, "a full ring refuses")
    assertEquals(r.size, 4)
    assertEquals((1 to 4).map(_ => r.pop()).toList, List[Any](1, 2, 3, 4))
    assertEquals(r.pop(), null, "an empty ring answers null")
    assert(r.isEmpty)
  }

  test("wrap-around: the lap boundary is where the stamps must line up") {
    val r = new Ring[Any](4)
    for (i <- 1 to 100) { assert(r.push(i), s"push $i"); assertEquals(r.pop(), i, s"pop $i") }
    assert(r.isEmpty)
    for (lap <- 0 until 25) {
      val base = lap * 4
      for (k <- 1 to 4) assert(r.push(base + k))
      assertEquals(r.push(-1), false)
      for (k <- 1 to 4) assertEquals(r.pop(), base + k)
      assertEquals(r.pop(), null)
    }
  }

  test("interleaved partial fills keep FIFO across the boundary") {
    val r = new Ring[Any](4)
    assert(r.push(1)); assert(r.push(2))
    assertEquals(r.pop(), 1)
    assert(r.push(3)); assert(r.push(4)); assert(r.push(5))
    assertEquals(r.push(6), false)
    assertEquals(r.pop(), 2); assertEquals(r.pop(), 3)
    assert(r.push(6))
    assertEquals(List(r.pop(), r.pop(), r.pop()), List[Any](4, 5, 6))
    assertEquals(r.pop(), null)
  }

  test("the smallest ring there is: two") {
    val r = new Ring[String](2)
    assert(r.push("a")); assert(r.push("b"))
    assertEquals(r.push("c"), false)
    assertEquals(r.pop(), "a")
    assert(r.push("c"))
    assertEquals(r.pop(), "b")
    assertEquals(r.pop(), "c")
    assertEquals(r.pop(), null)
    for (i <- 1 to 200) { assert(r.push(i.toString)); assertEquals(r.pop(), i.toString) }
  }

  test("a popped slot releases its reference") {
    val r = new Ring[Object](2)
    val o = new Object()
    assert(r.push(o))
    assertEquals(r.pop(), o)
    assert(r.isEmpty)
    assertEquals(r.size, 0)
  }

  test("batched push and pop move a run with one claim each, in order") {
    val r = new Ring[Any](8)
    assertEquals(r.pushMany(5)(i => i * 10), 5)
    assertEquals(r.pushMany(10)(i => 100 + i), 3, "only the room left is taken")
    val out = List.newBuilder[Any]
    assertEquals(r.popMany(6)(out += _), 6)
    assertEquals(r.popMany(6)(out += _), 2)
    assertEquals(out.result(), List[Any](0, 10, 20, 30, 40, 100, 101, 102))
    assertEquals(r.popMany(6)(_ => ()), 0)
  }

  test("MPMC: nothing lost, nothing duplicated, under real threads") {
    val r = new Ring[Any](256)
    val producers = 4
    val perProducer = 20000
    val total = producers * perProducer
    val seen = new AtomicLong(0L)
    val count = new AtomicInteger(0)
    val pool = Executors.newFixedThreadPool(producers + 4)
    val start = new CountDownLatch(1)
    val done = new CountDownLatch(producers + 4)
    for (p <- 0 until producers) pool.execute { () =>
      start.await()
      var i = 0
      while (i < perProducer) {
        val v = p * perProducer + i
        while (!r.push(v)) Thread.onSpinWait()
        i += 1
      }
      done.countDown()
    }
    for (_ <- 0 until 4) pool.execute { () =>
      start.await()
      while (count.get < total) {
        r.pop() match {
          case null => Thread.onSpinWait()
          case v: Int => val _ = seen.addAndGet(v.toLong); val _ = count.incrementAndGet()
          case other => throw new IllegalStateException(s"not an Int: $other")
        }
      }
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
    val r = new Ring[Any](64)
    val n = 100000
    val out = new Array[Int](n)
    val pool = Executors.newFixedThreadPool(2)
    val done = new CountDownLatch(2)
    pool.execute { () =>
      var i = 0
      while (i < n) { while (!r.push(i)) Thread.onSpinWait(); i += 1 }
      done.countDown()
    }
    pool.execute { () =>
      var i = 0
      while (i < n) r.pop() match {
        case null => Thread.onSpinWait()
        case v: Int => out(i) = v; i += 1
        case other => throw new IllegalStateException(s"not an Int: $other")
      }
      done.countDown()
    }
    assert(done.await(60, TimeUnit.SECONDS))
    pool.shutdownNow()
    assertEquals(out.toList, (0 until n).toList)
  }
}
