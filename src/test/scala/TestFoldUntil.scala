package okay

/**
 * specs/fold-until.md — a fold that can stop. The instances agree
 * with the List methods of their names; `until` runs the operator's
 * `loop`; and the walk STOPS: a stream computing on demand computes
 * nothing past the element that satisfied the fold.
 */
class TestFoldUntil extends munit.FunSuite:

  private def run[A, S, R](xs: List[A])(fo: FoldUntil[A, S, R]): R =
    Stream.foldUntil(xs)(using fo)

  private val inputs = List(List.empty[Int], List(1), List(1, 2, 3, 4, 5), List(2, 4, 6), List(7, 5, 3))

  test("find / headOption / exists / forall / take answer what List answers, empty and never-firing included") {
    val even = (i: Int) => i % 2 == 0
    for xs <- inputs do
      assertEquals(run(xs)(FoldUntil.find(even)), xs.find(even), s"find $xs")
      assertEquals(run(xs)(FoldUntil.headOption), xs.headOption, s"headOption $xs")
      assertEquals(run(xs)(FoldUntil.exists(even)), xs.exists(even), s"exists $xs")
      assertEquals(run(xs)(FoldUntil.forall(even)), xs.forall(even), s"forall $xs")
      for n <- 0 to 6 do
        assertEquals(run(xs)(FoldUntil.take(n)), xs.take(n).toVector, s"take($n) $xs")
  }

  test("until: Right ends the loop, Left continues, finish sees the last Left when the input runs out") {
    // the running sum, stopping at the first sum over 6
    val fo = FoldUntil.until[Int, Int, String](0)((s, a) => if s + a > 6 then Right(s"stopped at $s") else Left(s + a))(s => s"ran out at $s")
    assertEquals(run(List(1, 2, 3, 4, 5))(fo), "stopped at 6")
    assertEquals(run(List(1, 2))(fo), "ran out at 3")
    assertEquals(run(List.empty[Int])(fo), "ran out at 0")
  }

  test("the walk stops: an on-demand stream computes 3 of infinitely many for take(3)") {
    var seen = 0
    val ll = LazyList.from(0).map(i => { seen += 1; i })
    assertEquals(Stream.foldUntil(ll)(using FoldUntil.take[Int](3)), Vector(0, 1, 2))
    assertEquals(seen, 3)
  }

  test("find pulls up to and including its match, nothing after") {
    var seen = 0
    val ll = LazyList.from(0).map(i => { seen += 1; i })
    assertEquals(Stream.foldUntil(ll)(using FoldUntil.find[Int](_ == 4)), Some(4))
    assertEquals(seen, 5)
  }

  test("done(init) is honoured: take(0) pulls nothing") {
    var seen = 0
    val ll = LazyList.from(0).map(i => { seen += 1; i })
    assertEquals(Stream.foldUntil(ll)(using FoldUntil.take[Int](0)), Vector.empty[Int])
    assertEquals(seen, 0)
  }

  // ------------------------------------------------------ unboxed shapes

  test("the four unboxed shapes agree with the generic fold on every walk, and stop") {
    val xs = (1 to 100).toList
    // a running sum that stops past 50: 1+..+10 = 55
    val sumL = FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ > 50)(identity)
    val sumI = FoldUntil.int[Int, Int](0)((s, a) => s + a)(_ > 50)(identity)
    val sumD = FoldUntil.double[Int, Double](0.0)((s, a) => s + a)(_ > 50)(identity)
    val anyB = FoldUntil.boolean[Int, Boolean](false)((s, a) => s || a == 7)(identity)(identity)
    val generic = FoldUntil[Int, Long, Long](0L)((s, a) => s + a)(_ > 50)(identity)
    for (name, fo, expected) <- List(("long", sumL, 55L), ("generic", generic, 55L)) do
      assertEquals(xs.foldUntilTo(using fo), expected, s"List $name")
      assertEquals(Stream.foldUntil(xs)(using fo), expected, s"Stream $name")
    assertEquals(xs.foldUntilTo(using sumI), 55)
    assertEquals(xs.foldUntilTo(using sumD), 55.0)
    assertEquals(xs.foldUntilTo(using anyB), true)
    assertEquals(List(1, 2, 3).foldUntilTo(using anyB), false)
    // the stop holds on the specialised arm: an infinite iterator ends
    assertEquals(Iterator.from(1).foldUntilTo(using sumL), 55L)
    assertEquals(Stream.foldUntil(LazyList.from(1))(using sumI), 55)
    // exists/forall are OfBoolean now and still answer what List does
    assertEquals(xs.foldUntilTo(using FoldUntil.exists[Int](_ == 42)), true)
    assertEquals(xs.foldUntilTo(using FoldUntil.forall[Int](_ < 42)), false)
  }

  // ------------------------------------------------------ Foldable (stage 4)

  test("foldUntilTo on a List, a Vector, an Iterator and a Producer answers the List method") {
    val xs = List(3, 1, 4, 1, 5, 9, 2, 6)
    val prod: Producer[Int] = xs.foldRight(pure[Produce, Int](-1))((i, rest) => produce(i).flatMap(_ => rest))
    def check[S, X](fo: FoldUntil[Int, S, X], expected: X, name: String): Unit =
      assertEquals(xs.foldUntilTo(using fo), expected, s"List $name")
      assertEquals(xs.toVector.foldUntilTo(using fo), expected, s"Vector $name")
      assertEquals(xs.iterator.foldUntilTo(using fo), expected, s"Iterator $name")
      assertEquals(prod.foldUntilTo(using fo), expected, s"Producer $name")
    check(FoldUntil.find[Int](_ > 4), xs.find(_ > 4), "find")
    check(FoldUntil.take(3), xs.take(3).toVector, "take(3)")
    check(FoldUntil.take(0), Vector.empty[Int], "take(0)")
    check(FoldUntil.exists[Int](_ == 9), xs.exists(_ == 9), "exists")
    check(FoldUntil.forall[Int](_ < 9), xs.forall(_ < 9), "forall")
    check(FoldUntil.headOption, xs.headOption, "headOption")
  }

  test("an Iterator is left positioned after the satisfying element") {
    val it = Iterator.from(1)
    assertEquals(it.foldUntilTo(using FoldUntil.find[Int](_ == 3)), Some(3))
    assertEquals(it.next(), 4)
    assertEquals(it.foldUntilTo(using FoldUntil.take[Int](2)), Vector(5, 6))
    assertEquals(it.next(), 7)
  }

  // ---------------------------------------------------- Producer.foldUntil

  /** 1, tell "after 1", 2, tell "after 2", ... — a G operation after every production */
  type P = Produce + Writer % String
  private def counted(n: Int): Int ! P =
    def go(i: Int): Int ! P =
      if i > n then pure(-1)
      else effect[P, Int](i).flatMap(_ => effect[P, Unit](Writer(s"after $i"))).flatMap(_ => go(i + 1))
    go(1)

  private def runP[R](p: R ! Writer % String): (Seq[String], R) = !.run(Writer.run(p))

  test("Producer.foldUntil agrees with Producer.fold over the prefix, on every instance") {
    val xs = (1 to 20).toList
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit =
      val expected = Stream.foldUntil(xs)(using fo)
      assertEquals(runP(Producer.foldUntil[Int, S, X, Int, Writer % String](counted(20))(using summon, fo))._2, expected, name)
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.take(100), "take(100)")
    check(FoldUntil.find[Int](_ > 7), "find")
    check(FoldUntil.find[Int](_ > 70), "find-none")
    check(FoldUntil.exists[Int](_ == 11), "exists")
    check(FoldUntil.forall[Int](_ < 5), "forall")
    check(FoldUntil.headOption, "headOption")
  }

  test("Producer.foldUntil performs the G op before the stop and not the one after it") {
    val (told, got) = runP(Producer.foldUntil[Int, Vector[Int], Vector[Int], Int, Writer % String](counted(1000))(using summon, FoldUntil.take[Int](3)))
    assertEquals(got, Vector(1, 2, 3))
    assertEquals(told, Seq("after 1", "after 2"))
    val (told0, got0) = runP(Producer.foldUntil[Int, Vector[Int], Vector[Int], Int, Writer % String](counted(1000))(using summon, FoldUntil.take[Int](0)))
    assertEquals(got0, Vector.empty[Int])
    assertEquals(told0, Seq.empty[String])
  }

  test("Producer.foldUntil is tail-recursive across productions: 100 000, the stop never firing") {
    val (_, got) = runP(Producer.foldUntil[Int, Boolean, Boolean, Int, Writer % String](counted(100_000))(using summon, FoldUntil.exists[Int](_ < 0)))
    assertEquals(got, false)
  }

  test("Producer.each is tail-recursive across productions: 200 000 back to back, no G op between") {
    // no forwarded operation resets the stack here, so a production
    // handled by a call INSIDE the split's closure is one frame each
    val n = 200_000
    def go(i: Int): Unit ! P = if i > n then pure(()) else effect[P, Int](i).flatMap(_ => go(i + 1))
    var sum = 0L
    val _ = runP(Producer.each[Int, Unit, Writer % String](go(1))(sum += _))
    assertEquals(sum, n.toLong * (n + 1) / 2)
  }
