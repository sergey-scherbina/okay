package okay

/**
 * specs/generators.md — a generator is a program that tells; the laws
 * are Python's: the body runs as far as it is read and no further,
 * a generation ends three ways, and a plain for-comprehension over a
 * `Gen` is a generator with no macro.
 */
class TestGen extends munit.FunSuite:

  /** a body that counts its own steps: the witness of laziness */
  class Counted:
    var steps = 0
    def gen: Gen[Int] =
      def go(i: Int): Unit ! Gen.Row[Int] =
        steps += 1
        Gen.emit(i).program.flatMap(_ => go(i + 1))
      // a hand-written body is lazy at construction only if it says so
      // — `Free.delay` is Python's "nothing runs before next()"; a
      // generator block and Gen.from/unfold say it for you
      Gen.fromProgram(Free.delay(() => go(1)))

  test("LAZINESS: after k next() calls the body has run to its k-th yield and no further") {
    val c = Counted()
    val it = c.gen.iterator
    assertEquals(c.steps, 0, "nothing runs at construction")
    assertEquals(it.next(), 1)
    assertEquals(c.steps, 1, "one step: the first yield")
    assertEquals(it.next(), 2)
    assertEquals(it.next(), 3)
    assertEquals(c.steps, 3, "the code between yields ran only when the next value was asked")
  }

  test("LAZINESS through the stopping readers: take, first, find, exists") {
    val c = Counted()
    assertEquals(c.gen.take(3).toList, List(1, 2, 3))
    assertEquals(c.steps, 3, "take(3) ran the body exactly to its third yield")
    val c2 = Counted()
    assertEquals(c2.gen.first, Some(1)); assertEquals(c2.steps, 1)
    val c3 = Counted()
    assertEquals(c3.gen.find(_ > 4), Some(5)); assertEquals(c3.steps, 5)
    val c4 = Counted()
    assertEquals(c4.gen.exists(_ == 2), true); assertEquals(c4.steps, 2)
  }

  test("TERMINATION 1: the body ends — the iterator is exhausted, toList has everything") {
    val g = Gen(1, 2, 3)
    assertEquals(g.toList, List(1, 2, 3))
    val it = g.iterator
    assertEquals(it.toList, List(1, 2, 3))
    assertEquals(it.hasNext, false)
    intercept[NoSuchElementException](it.next())
  }

  test("TERMINATION 2: Gen.stop in the middle ends it there, whatever follows") {
    var after = 0
    val tail: Gen[Int] = Gen.fromProgram(Gen.stop[Int].program.flatMap(_ => { after += 1; Gen.emit(3).program }))
    val g: Gen[Int] = Gen.emit(1) ++ Gen.emit(2) ++ tail
    assertEquals(g.toList, List(1, 2))
    assertEquals(after, 0, "nothing past the stop ran")
    assertEquals(g.map(_ * 10).toList, List(10, 20), "a stop survives an element-wise map")
    assertEquals(Gen.from(1 to 10).flatMap(i => if i == 4 then Gen.stop else Gen.emit(i)).toList, List(1, 2, 3))
  }

  test("TERMINATION 3: a reader that stops never runs the rest of the body") {
    val c = Counted()
    var seen = 0
    try c.gen.foreach(i => { seen += 1; if i == 3 then throw new RuntimeException("enough") })
    catch case _: RuntimeException => ()
    assertEquals(seen, 3)
    assertEquals(c.steps, 3)
  }

  test("an INFINITE generator composes with map/filter/take and terminates") {
    val evens = Gen.unfold(1)(i => Some((i, i + 1))).map(_ * 2).filter(_ % 3 == 0)
    assertEquals(evens.take(3).toList, List(6, 12, 18))
    assertEquals(Gen.unfold(1)(i => Some((i, i + 1))).takeWhile(_ < 4).toList, List(1, 2, 3))
    assertEquals(Gen.unfold(1)(i => Some((i, i + 1))).drop(2).take(2).toList, List(3, 4))
  }

  test("a plain for-comprehension over Gen is a generator — no macro, lazy, nested") {
    val inner = Counted()
    val pairs: Gen[(Int, Int)] =
      for
        x <- Gen(1, 2, 3)
        y <- inner.gen.take(2) if (x + y) % 2 == 0
      yield (x, y)
    assertEquals(pairs.take(2).toList, List((1, 1), (2, 2)))
    assert(inner.steps <= 4, s"the inner generator ran only as far as read: ${inner.steps}")
    assertEquals(pairs.toList, List((1, 1), (2, 2), (3, 1)))
  }

  test("NON-MEMOISING: reading twice runs the body twice; toLazyList memoises") {
    val c = Counted()
    val g = c.gen.take(2)
    assertEquals(g.toList, List(1, 2)); assertEquals(g.toList, List(1, 2))
    assertEquals(c.steps, 4, "two reads, two runs")
    val c2 = Counted()
    val ll = c2.gen.take(2).toLazyList
    assertEquals(ll.toList, List(1, 2)); assertEquals(ll.toList, List(1, 2))
    assertEquals(c2.steps, 2, "the lazy list remembered")
  }

  test("zipWithIndex, ++, of, and a Writer program as a generator") {
    assertEquals(Gen("a", "b").zipWithIndex.toList, List(("a", 0), ("b", 1)))
    assertEquals((Gen(1) ++ Gen(2, 3)).toList, List(1, 2, 3))
    val w: Unit ! Writer % Int = Writer.tell(7).flatMap(_ => Writer.tell(8))
    assertEquals(Gen.of(w).toVector, Vector(7, 8))
    assertEquals(Gen.empty[Int].toList, Nil)
    assertEquals(Gen(1, 2, 3).forall(_ > 0), true)
  }

  test("a deep generator is flat on the stack: 100 000 elements through take and toList") {
    val g = Gen.unfold(0)(i => if i < 100000 then Some((i, i + 1)) else None)
    assertEquals(g.toList.size, 100000)
    assertEquals(g.drop(99990).toList.size, 10)
    assertEquals(g.iterator.size, 100000)
  }
