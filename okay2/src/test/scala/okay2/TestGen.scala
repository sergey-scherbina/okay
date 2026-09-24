package okay2

/**
 * A generator is a program that tells; the laws are Python's: the
 * body runs as far as it is read and no further, a generation ends
 * three ways, and a plain for-comprehension over a `Gen` is a
 * generator with no macro.
 */
class TestGen extends munit.FunSuite {

  /** a body that counts its own steps: the witness of laziness */
  class Counted {
    var steps = 0
    def gen: Gen[Int] = {
      def go(i: Int): Unit ! Gen.Row[Int] = {
        steps += 1
        Gen.emit(i).program.flatMap(_ => go(i + 1))
      }
      // a hand-written body is lazy at construction only if it says so
      Gen.fromProgram(Free.delay(() => go(1)))
    }
  }

  test("LAZINESS: after k next() calls the body has run to its k-th yield and no further") {
    val c = new Counted
    val it = c.gen.iterator
    assertEquals(c.steps, 0, "nothing runs at construction")
    assertEquals(it.next(), 1)
    assertEquals(c.steps, 1, "one step: the first yield")
    assertEquals(it.next(), 2)
    assertEquals(it.next(), 3)
    assertEquals(c.steps, 3, "the code between yields ran only when the next value was asked")
  }

  test("LAZINESS through the stopping readers: take, first, find, exists") {
    val c = new Counted
    assertEquals(c.gen.take(3).toList, List(1, 2, 3))
    assertEquals(c.steps, 3, "take(3) ran the body exactly to its third yield")
    val c2 = new Counted
    assertEquals(c2.gen.first, Some(1)); assertEquals(c2.steps, 1)
    val c3 = new Counted
    assertEquals(c3.gen.find(_ > 4), Some(5)); assertEquals(c3.steps, 5)
    val c4 = new Counted
    assertEquals(c4.gen.exists(_ == 2), true); assertEquals(c4.steps, 2)
  }

  test("TERMINATION 1: the body ends — the iterator is exhausted, toList has everything") {
    val g = Gen(1, 2, 3)
    assertEquals(g.toList, List(1, 2, 3))
    val it = g.iterator
    assertEquals(it.toList, List(1, 2, 3))
    assertEquals(it.hasNext, false)
    val _ = intercept[NoSuchElementException](it.next())
  }

  test("TERMINATION 2: Gen.stop in the middle ends it there, whatever follows") {
    var after = 0
    val tail: Gen[Int] = Gen.fromProgram(Gen.stop[Int].program.flatMap(_ => { after += 1; Gen.emit(3).program }))
    val g: Gen[Int] = Gen.emit(1) ++ Gen.emit(2) ++ tail
    assertEquals(g.toList, List(1, 2))
    assertEquals(after, 0, "nothing past the stop ran")
    assertEquals(g.map(_ * 10).toList, List(10, 20), "a stop survives an element-wise map")
    assertEquals(Gen.from(1 to 10).flatMap(i => if (i == 4) Gen.stop[Int] else Gen.emit(i)).toList, List(1, 2, 3))
  }

  test("TERMINATION 3: a reader that stops never runs the rest of the body") {
    val c = new Counted
    var seen = 0
    try c.gen.foreach(i => { seen += 1; if (i == 3) throw new RuntimeException("enough") })
    catch { case _: RuntimeException => () }
    assertEquals(seen, 3)
    assertEquals(c.steps, 3)
  }

  test("an INFINITE generator composes with map/filter/take and terminates") {
    val evens = Gen.unfold(1)(i => Some((i, i + 1))).map(_ * 2).filter(_ % 3 == 0)
    assertEquals(evens.take(3).toList, List(6, 12, 18))
    assertEquals(Gen.unfold(1)(i => Some((i, i + 1))).takeWhile(_ < 4).toList, List(1, 2, 3))
    assertEquals(Gen.unfold(1)(i => Some((i, i + 1))).drop(2).take(2).toList, List(3, 4))
  }

  test("filter is a walk: lazy to the counter, and a long run of rejections is flat") {
    val c = new Counted
    assertEquals(c.gen.filter(_ % 2 == 0).take(2).toList, List(2, 4))
    assertEquals(c.steps, 4, "the body ran exactly to its fourth yield: two rejected, two kept")
    val c2 = new Counted
    val it = c2.gen.filter(_ > 2).iterator
    assertEquals(it.next(), 3); assertEquals(c2.steps, 3)
    val g = Gen.unfold(0)(i => if (i < 100000) Some((i, i + 1)) else None)
    assertEquals(g.filter(_ == 99999).toList, List(99999))
    assertEquals(g.filter(_ % 2 == 0).toList.size, 50000)
    assertEquals(g.withFilter(_ < 3).map(_ * 10).toList, List(0, 10, 20))
    // the walks (materialised) are flat on a run of rejections too
    assertEquals(Gen.fromProgram(g.filter(_ == 99999).program).toList, List(99999))
  }

  test("FUSED = MATERIALISED: generated chains of map/filter/take/takeWhile/drop read alike three ways") {
    val rnd = new scala.util.Random(20260927)
    for (_ <- 1 to 200) {
      val n = rnd.nextInt(30)
      var g: Gen[Int] = Gen.unfold(0)(i => if (i < n) Some((i, i + 1)) else None)
      for (_ <- 1 to rnd.nextInt(5)) {
        rnd.nextInt(5) match {
          case 0 => val k = rnd.nextInt(3) + 1; g = g.map(_ * k)
          case 1 => val m = rnd.nextInt(3) + 2; g = g.filter(_ % m != 0)
          case 2 => g = g.take(rnd.nextInt(8))
          case 3 => val lim = rnd.nextInt(40); g = g.takeWhile(_ < lim)
          case _ => g = g.drop(rnd.nextInt(4))
        }
      }
      val fused = g.toList
      assertEquals(fused, Gen.fromProgram(g.program).toList, "the walks")
      assertEquals(fused, g.iterator.toList, "the stepper over the walks")
      assertEquals(g.first, fused.headOption)
      assertEquals(g.exists(_ > 10), fused.exists(_ > 10))
    }
  }

  test("FUSED = MATERIALISED with flatMap, ++ and zipWithIndex in the chains") {
    val rnd = new scala.util.Random(20260928)
    for (_ <- 1 to 200) {
      val n = rnd.nextInt(12)
      var g: Gen[Int] = Gen.unfold(0)(i => if (i < n) Some((i, i + 1)) else None)
      for (_ <- 1 to rnd.nextInt(5)) {
        rnd.nextInt(7) match {
          case 0 => g = g.map(_ + 1)
          case 1 => g = g.filter(_ % 2 == 0)
          case 2 => g = g.take(rnd.nextInt(6))
          case 3 => g = g.flatMap(i => if (i % 3 == 0) Gen(i, i * 10) else Gen.emit(i))
          case 4 => g = g ++ Gen(100, 101)
          case 5 => g = g.zipWithIndex.map { case (x, i) => x + i }
          case _ => g = g.drop(1)
        }
      }
      val fused = g.toList
      assertEquals(fused, Gen.fromProgram(g.program).toList, "the walks")
      assertEquals(fused, g.iterator.toList, "the stepper")
      assertEquals(g.first, fused.headOption)
    }
  }

  test("fused ++: a take across counts through; each side's own take counts its own; indices continue; the right side is not run if the left is enough") {
    val c = new Counted
    assertEquals((Gen(1, 2) ++ c.gen).take(3).toList, List(1, 2, 1))
    assertEquals(c.steps, 1)
    assertEquals((Gen(1, 2, 3).take(2) ++ Gen(7, 8, 9).take(1)).toList, List(1, 2, 7))
    assertEquals((Gen("a") ++ Gen("b", "c")).zipWithIndex.toList, List(("a", 0), ("b", 1), ("c", 2)))
    val c2 = new Counted
    assertEquals((Gen(5, 6, 7) ++ c2.gen).take(2).toList, List(5, 6))
    assertEquals(c2.steps, 0, "the right side never ran")
    assertEquals((Gen.emit(1) ++ Gen.stop[Int] ++ Gen.emit(3)).map(_ * 2).toList, List(2))
  }

  test("fused flatMap: lazy to the inner counter; an inner Stop ends the whole generation") {
    val inner = new Counted
    assertEquals(Gen(1, 2, 3).flatMap(x => inner.gen.take(2).map(_ * x)).take(3).toList, List(1, 2, 2))
    assertEquals(inner.steps, 3, "two from the first inner, one from the second, then stopped")
    assertEquals(Gen.from(1 to 10).flatMap(i => if (i == 4) Gen.stop[Int] else Gen.emit(i)).toList, List(1, 2, 3))
    assertEquals(Gen(1, 2).flatMap(i => Gen.emit(i) ++ Gen.emit(i + 10)).zipWithIndex.toList,
      List((1, 0), (11, 1), (2, 2), (12, 3)))
  }

  test("a fused take(n) runs the body exactly to its n-th KEPT element; find stops where it finds") {
    val c = new Counted
    assertEquals(c.gen.map(_ * 3).filter(_ % 2 == 0).take(2).toList, List(6, 12))
    assertEquals(c.steps, 4, "kept 6 (step 2) and 12 (step 4): four steps, not one more")
    val c2 = new Counted
    assertEquals(c2.gen.map(_ + 1).find(_ == 4), Some(4))
    assertEquals(c2.steps, 3)
    val c3 = new Counted
    assertEquals(c3.gen.take(0).toList, Nil)
    assertEquals(c3.steps, 0, "take(0) runs nothing")
    assertEquals(Gen(1, 2).drop(5).toList, Nil)
    val stopped: Gen[Int] = Gen.emit(1) ++ Gen.emit(2) ++ Gen.stop[Int] ++ Gen.emit(3)
    assertEquals(stopped.map(_ * 10).filter(_ > 5).toList, List(10, 20), "a Stop ends a fused read")
  }

  test("a plain for-comprehension over Gen is a generator — no macro, lazy, nested") {
    val inner = new Counted
    val pairs: Gen[(Int, Int)] =
      for {
        x <- Gen(1, 2, 3)
        y <- inner.gen.take(2) if (x + y) % 2 == 0
      } yield (x, y)
    assertEquals(pairs.take(2).toList, List((1, 1), (2, 2)))
    assert(inner.steps <= 4, s"the inner generator ran only as far as read: ${inner.steps}")
    assertEquals(pairs.toList, List((1, 1), (2, 2), (3, 1)))
  }

  test("NON-MEMOISING: reading twice runs the body twice; toLazyList memoises") {
    val c = new Counted
    val g = c.gen.take(2)
    assertEquals(g.toList, List(1, 2)); assertEquals(g.toList, List(1, 2))
    assertEquals(c.steps, 4, "two reads, two runs")
    val c2 = new Counted
    val ll = c2.gen.take(2).toLazyList
    assertEquals(ll.toList, List(1, 2)); assertEquals(ll.toList, List(1, 2))
    assertEquals(c2.steps, 2, "the lazy list remembered")
  }

  test("zipWithIndex, ++, of, and a Writer program as a generator") {
    assertEquals(Gen("a", "b").zipWithIndex.toList, List(("a", 0), ("b", 1)))
    assertEquals((Gen(1) ++ Gen(2, 3)).toList, List(1, 2, 3))
    val w: Unit ! Writer[Int] = Writer.tell(7).flatMap(_ => Writer.tell(8))
    assertEquals(Gen.of(w).toVector, Vector(7, 8))
    assertEquals(Gen.empty[Int].toList, Nil)
    assertEquals(Gen(1, 2, 3).forall(_ > 0), true)
  }

  test("a deep generator is flat on the stack: 100 000 elements through take and toList") {
    val g = Gen.unfold(0)(i => if (i < 100000) Some((i, i + 1)) else None)
    assertEquals(g.toList.size, 100000)
    assertEquals(g.drop(99990).toList.size, 10)
    assertEquals(g.iterator.size, 100000)
    assertEquals(Gen.fromProgram(g.drop(99990).program).toList.size, 10, "the materialised drop is flat too")
  }
}

/**
 * `Gen.zip`, and strymonas's hard case (Kiselyov, Biboudis, Palladinos
 * & Smaragdakis, "Stream fusion, to completeness", POPL 2017) —
 * zipping a generator built by `flatMap` without materializing it first.
 */
class TestGenZip extends munit.FunSuite {

  class Counted {
    var steps = 0
    def gen(from: Int): Gen[Int] = {
      def go(i: Int): Unit ! Gen.Row[Int] = {
        steps += 1
        Gen.emit(i).program.flatMap(_ => go(i + 1))
      }
      Gen.fromProgram(Free.delay(() => go(from)))
    }
  }

  test("zip: two equal-length sources pair up in order") {
    assertEquals(Gen(1, 2, 3).zip(Gen("a", "b", "c")).toList, List((1, "a"), (2, "b"), (3, "c")))
  }

  test("zip: stops at the SHORTER side, either side; either side empty gives an empty zip") {
    assertEquals(Gen(1, 2, 3, 4, 5).zip(Gen("a", "b")).toList, List((1, "a"), (2, "b")))
    assertEquals(Gen(1, 2).zip(Gen("a", "b", "c", "d")).toList, List((1, "a"), (2, "b")))
    assertEquals(Gen.empty[Int].zip(Gen(1, 2, 3)).toList, Nil)
    assertEquals(Gen(1, 2, 3).zip(Gen.empty[String]).toList, Nil)
  }

  test("zipWith, and zip composes with a further stage and after ++") {
    assertEquals(Gen(1, 2, 3).zipWith(Gen(10, 20, 30))(_ + _).toList, List(11, 22, 33))
    assertEquals(Gen(1, 2, 3, 4).zip(Gen("a", "b", "c")).map(_._1 * 10).filter(_ > 10).toList, List(20, 30))
    val left = Gen(1, 2) ++ Gen(3, 4, 5)
    assertEquals(left.zip(Gen("a", "b", "c", "d")).toList, List((1, "a"), (2, "b"), (3, "c"), (4, "d")))
  }

  test("STRYMONAS'S HARD CASE: zip a flatMap-fused generator, without materializing it first") {
    val left: Gen[Int] = Gen(1, 2, 3, 4).flatMap(i => Gen.from(Vector.fill(i)(i)))
    val zipped = left.zip(Gen('a', 'b', 'c', 'd', 'e', 'f')).toList
    assertEquals(zipped, List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f'))
  }

  test("the hard case is LAZY: zip through a flatMap-fused side stops the OUTER source too") {
    val outer = new Counted
    val left: Gen[Int] = outer.gen(1).flatMap(i => Gen.from(Vector.fill(2)(i)))
    assertEquals(left.zip(Gen("x", "y", "z")).toList, List(1 -> "x", 1 -> "y", 2 -> "z"))
    assertEquals(outer.steps, 2, "3 zipped elements need only the 1st and 2nd outer steps")
  }

  test("the hard case against an INFINITE outer source; the right side may be the fused one") {
    val infiniteDoubled: Gen[Int] = Gen.unfold(1)(n => Some((n, n + 1))).flatMap(i => Gen.from(Vector(i, i)))
    assertEquals(infiniteDoubled.zip(Gen("p", "q", "r", "s", "t")).toList,
      List(1 -> "p", 1 -> "q", 2 -> "r", 2 -> "s", 3 -> "t"))
    val right: Gen[Int] = Gen(1, 2, 3).flatMap(i => Gen.from(Vector.fill(i)(i)))
    assertEquals(Gen("a", "b", "c", "d", "e", "f").zip(right).toList,
      List("a" -> 1, "b" -> 2, "c" -> 2, "d" -> 3, "e" -> 3, "f" -> 3))
  }
}
