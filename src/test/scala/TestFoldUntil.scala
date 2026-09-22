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
