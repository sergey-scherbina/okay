package okay

/**
 * Every entry in the menu, built and run.
 *
 * A builder is a promise that these combinations exist and work. The
 * only way that promise stays true as mechanisms are added is if
 * something builds each one and pushes elements through it — a menu
 * nobody ordered from is a menu nobody checked.
 */
class TestQueues extends munit.FunSuite {

  /** every variant, with what it promises about close */
  private val menu: List[(String, () => Channel[Int], Boolean)] = List(
    ("strong.bounded",           () => Queues.strong[Int].bounded(16).build, true),
    ("strong.unbounded",         () => Queues.strong[Int].unbounded.build, true),
    ("strong.relaxed",           () => Queues.strong[Int].relaxed.parts(4).each(8).build, true),
    ("strong.relaxedUnbounded",  () => Queues.strong[Int].relaxed.parts(3).unbounded.build, true),
    ("strong.on(ring)",          () => Queues.strong[Int].on([T] => (_: Int) => Ring[T](16)).build, true),
    ("strong.default",           () => Queues.strong[Int].build, true),
    ("weak.bounded",             () => Queues.weak[Int].bounded(16).build, false),
    ("weak.unbounded",           () => Queues.weak[Int].unbounded.build, false),
    ("weak.relaxed",             () => Queues.weak[Int].relaxed.parts(4).each(8).build, false),
    ("weak.relaxedUnbounded",    () => Queues.weak[Int].relaxed.parts(3).unbounded.build, false),
    ("weak.adaptive",            () => Queues.weak[Int].adaptive.parts(4).each(8).build, false),
    ("weak.on(segments)",        () => Queues.weak[Int].on(Segments[Int]()).build, false),
    ("composable.array",         () => Queues.composable[Int](16).arrayBuffer.build, true),
    ("composable.list",          () => Queues.composable[Int](16).listBuffer.build, true),
    ("composable.default",       () => Queues.composable[Int](16).build, true),
  )

  menu.foreach: (name, mk, drains) =>
    test(s"$name: elements go through, and close ends it") {
      val c = mk()
      val n = 12
      val p = Thread.ofVirtual().start { () =>
        var i = 0
        while i < n do { val _ = c.sendBlocking(i); i += 1 }
        if !drains then Thread.sleep(50)
        c.close()
      }
      val got = scala.collection.mutable.ArrayBuffer.empty[Int]
      var go = true
      while go do
        c.receiveBlocking() match
          case Some(v) => got += v
          case None => go = false
      p.join()
      if drains then
        assertEquals(got.toSet, (0 until n).toSet, s"$name: a drain-on-close channel lost elements")
      else
        assert(got.toSet.subsetOf((0 until n).toSet), s"$name: invented elements")
      assert(c.isClosed, s"$name: not closed")
    }

  test("rendezvous: a sender waits for a receiver") {
    val c = Queues.rendezvous[Int].build
    assert(!c.offer(1), "a rendezvous buffers nothing, so offer cannot succeed")
    val p = Thread.ofVirtual().start { () => val _ = c.sendBlocking(7) }
    assertEquals(c.receiveBlocking(), Some(7))
    p.join()
    c.close()
  }

  test("the relaxed ones say how many orders they keep") {
    assertEquals(Ring[Int](8).parts, 1)
    assertEquals(Segments[Int]().parts, 1)
    assertEquals(AdaptiveFifo[Int](5, () => Ring[Int](8), eager = true).parts, 5)
  }
}
