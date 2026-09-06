package okay

/**
 * actor-receive-fused: `receiveBlocking` through a `Handoff`. The laws
 * are that every answer `receive`'s program gives arrives through the
 * handoff too — an element now, an element later, the end, a failure
 * — and that a hit really is the fast path: `receiveInto` answers true
 * and nobody parks.
 */
class TestHandoff extends munit.FunSuite:

  test("law: an element that is ready is a HIT -- receiveInto answers true, the handoff is filled, no park") {
    val c = Channel[Int](8)
    assert(c.offer(7))
    val h = summon[CanBlock].handoff[Int]()
    assert(c.receiveInto(h), "a buffered element must be a synchronous hit")
    assert(h.filled)
    assertEquals(h.answer, Some(7))
    // and a second handoff on the now-empty channel is a MISS: registered, not filled
    val h2 = summon[CanBlock].handoff[Int]()
    assert(!c.receiveInto(h2))
    assert(!h2.filled)
    assert(c.offer(8))                            // the registered handoff receives it
    Thread.sleep(20)
    assert(h2.filled)
    assertEquals(h2.answer, Some(8))
  }

  test("law: receiveBlocking answers the buffered elements in order, then parks, then wakes on a send") {
    val c = Channel[Int](8)
    assert(c.offer(1)); assert(c.offer(2))
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), Some(2))
    @volatile var got: Option[Int] = null
    val waiter = Async.spawn(okay.pure(()).map(_ => { got = c.receiveBlocking(); () }))
    Thread.sleep(50)                              // parked on an empty channel
    assert(c.offer(3))
    waiter.join()
    assertEquals(got, Some(3))
  }

  test("law: the end arrives through the handoff -- synchronously when already ended, and to a parked receiver on close") {
    val c = Channel[Int](8)
    assert(c.offer(1))
    c.close()
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), None)       // ended: filled synchronously, await returns at once
    assertEquals(c.receiveBlocking(), None)
    val d = Channel[Int](8)
    @volatile var got: Option[Int] = Some(-1)
    val waiter = Async.spawn(okay.pure(()).map(_ => { got = d.receiveBlocking(); () }))
    Thread.sleep(50)
    d.close()
    waiter.join()
    assertEquals(got, None)
  }

  test("law: a failure arrives through the handoff as receive's program would throw it, after the buffered elements") {
    val c = Channel[Int](8)
    assert(c.offer(1))
    c.fail(new RuntimeException("boom"))
    c.close()
    assertEquals(c.receiveBlocking(), Some(1))
    val e = intercept[RuntimeException](c.receiveBlocking())
    assertEquals(e.getMessage, "boom")
  }

  test("law: a receiver parked on the handoff is woken by fail+close with the failure") {
    val c = Channel[Int](8)
    @volatile var seen: Throwable | Null = null
    val waiter = Async.spawn(okay.pure(()).map(_ =>
      try { val _ = c.receiveBlocking() } catch case t: RuntimeException => seen = t))
    Thread.sleep(50)
    c.fail(new RuntimeException("late"))
    c.close()
    waiter.join()
    assert(seen != null && seen.nn.getMessage == "late", s"expected the failure, got $seen")
  }

  test("law: the default receiveInto (STM channel) is correct before it is fast -- it registers, and the answer still arrives") {
    val c = StmChannel[Int](8)
    assert(c.offer(9))
    val h = summon[CanBlock].handoff[Int]()
    val hit = c.receiveInto(h)                    // default: registers; receiveAsync fills synchronously
    assert(!hit)
    assert(h.filled, "the default path must have filled the handoff synchronously")
    assertEquals(h.answer, Some(9))
    assert(c.offer(10))
    assertEquals(c.receiveBlocking(), Some(10))
  }

  test("law: many elements through many blocking receives, none lost, order kept, across a ring of 2") {
    val c = Channel[Int](2)
    val n = 3000
    val producer = Async.spawn(okay.pure(()).map(_ => { for i <- 0 until n do assert(c.sendBlocking(i)); c.close() }))
    val out = Vector.newBuilder[Int]
    var more = true
    while more do c.receiveBlocking() match
      case Some(a) => out += a
      case None => more = false
    producer.join()
    assertEquals(out.result(), (0 until n).toVector)
  }
