package okay

import Aggregator as A

/** The aggregation algebra: one-pass composition, mergeable partials,
 * groups and sliding windows. */
class TestAggregate extends munit.FunSuite {

  val xs = List(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0)

  test("zip: two statistics in ONE pass (the source is consumed once)") {
    var steps = 0
    def probe(rest: List[Int]): Producer[Int] = rest match
      case Nil => pure(0)
      case x :: t => produce({ steps += 1; x }).flatMap(_ => probe(t))
    val agg = A.sum[Int].zip(A.count[Int])
    val acc = Stream.fold(probe(List(1, 2, 3, 4)))(using agg.fold)
    assertEquals(agg.present(acc), (10, 4L))
    assertEquals(steps, 4)
  }

  test("mean and variance agree with the references") {
    assertEquals(A.mean[Double].run(xs), xs.sum / xs.length)
    val m = xs.sum / xs.length
    val ref = xs.map(x => (x - m) * (x - m)).sum / xs.length
    assert(math.abs(A.variance[Double].run(xs) - ref) < 1e-9)
    assert(math.abs(A.stddev[Double].run(xs) - math.sqrt(ref)) < 1e-9)
  }

  test("zipLong: the flat pair answers what the tuple pair answers") {
    val ns = List(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)
    // the UNBOXED spellings: `sum[N]`'s declared return type is
    // `Aggregator[N, N, N]`, which hides the `OfLong` underneath it —
    // so the flat pair is reached through `sumLong`, and this line is
    // the whole usability cost of the specialization
    val tupled = A.count[Long].zip(A.sumLong)
    val flat = A.count[Long].zipLong(A.sumLong)
    assertEquals(flat.run(ns), tupled.run(ns))
    assertEquals(flat.run(Nil), tupled.run(Nil))
    // and it merges like the pair it replaces, at every cut
    for cut <- 0 to ns.length do
      def half[Acc](agg: Aggregator[Long, Acc, ?], part: List[Long]): Acc =
        part.foldLeft(agg.init)(agg.add)
      assertEquals(
        flat.present(flat.merge(half(flat, ns.take(cut)), half(flat, ns.drop(cut)))),
        tupled.present(tupled.merge(half(tupled, ns.take(cut)), half(tupled, ns.drop(cut)))),
        s"a cut at $cut disagrees")
  }

  test("summary: the flat accumulator answers what the zip answers") {
    val ns = List(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)
    val zipped = A.count[Long]
      .zip(A.sum[Long])
      .zip(A.min[Long])
      .zip(A.max[Long])
    val ((( n, sum), mn), mx) = zipped.run(ns)
    val s = A.summary[Long](identity).run(ns)
    assertEquals(s.count, n)
    assertEquals(s.sum, sum)
    assertEquals(s.min, mn.get)
    assertEquals(s.max, mx.get)
    assertEquals(s.mean, ns.sum.toDouble / ns.length)
  }

  test("summary: empty is the sentinels, and merging with it changes nothing") {
    val agg = A.summary[Long](identity)
    val empty = agg.present(agg.init)
    assertEquals(empty.count, 0L)
    assertEquals(empty.min, Long.MaxValue)
    assertEquals(empty.max, Long.MinValue)
    assert(empty.mean.isNaN)
    val some = agg.run(List(7L, -2L))
    assertEquals(agg.merge(some, agg.init), some)
    assertEquals(agg.merge(agg.init, some), some)
  }

  test("summary: split-and-combine equals the whole, at every cut") {
    val ns = (-20L to 20L).toList
    val agg = A.summary[Long](identity)
    val whole = agg.run(ns)
    for cut <- 0 to ns.length do
      val (l, r) = ns.splitAt(cut)
      val merged = agg.merge(ns.take(cut).foldLeft(agg.init)(agg.add),
        ns.drop(cut).foldLeft(agg.init)(agg.add))
      assertEquals(agg.present(merged), whole, s"a cut at $cut disagrees ($l | $r)")
  }

  test("merge: split-and-combine equals the whole (the distributed contract)") {
    val (l, r) = xs.splitAt(3)
    def half[Acc](agg: Aggregator[Double, Acc, ?], part: List[Double]): Acc =
      part.foldLeft(agg.init)(agg.add)
    for agg <- List[Aggregator[Double, ?, ?]](
      A.sum[Double], A.count[Double],
      A.min[Double], A.max[Double], A.topK[Double](3), A.distinct[Double])
    do
      def go[Acc](agg: Aggregator[Double, Acc, ?]) =
        assertEquals(agg.present(agg.merge(half(agg, l), half(agg, r))),
          agg.present(half(agg, xs)))
      go(agg)
    // variance merges by Chan/Golub/LeVeque: equal up to floating error
    val v = A.variance[Double]
    assert(math.abs(
      v.present(v.merge(half(v, l), half(v, r))) - v.present(half(v, xs))) < 1e-9)
  }

  test("the standard library aggregates correctly") {
    assertEquals(A.min[Double].run(xs), Some(1.0))
    assertEquals(A.max[Double].run(xs), Some(9.0))
    assertEquals(A.first[Double].run(xs), Some(3.0))
    assertEquals(A.last[Double].run(xs), Some(6.0))
    assertEquals(A.topK[Double](3).run(xs), List(9.0, 6.0, 5.0))
    assertEquals(A.distinct[Double].run(xs), 7L)
    assertEquals(A.count[Double].run(Nil), 0L)
    assertEquals(A.min[Double].run(Nil), None)
  }

  // ---- topK, once it stopped sorting every element

  final case class Row(id: Int, score: Double)
  given Ordering[Row] = Ordering.by(_.score)

  test("topK refuses an element that only TIES the k-th, so the FIRST seen survives") {
    // the old implementation consed the newcomer at the head and
    // re-sorted, and `sorted` is stable, so an equal score displaced
    // the record already held. Both answers carry the same scores;
    // this one does not depend on the corpus's order changing.
    val rows = Vector(Row(1, 5.0), Row(2, 9.0), Row(3, 5.0), Row(4, 1.0))
    assertEquals(A.topK[Row](2).run(rows).map(_.id), List(2, 1))
  }

  test("topK agrees with sorting the whole input, at every k") {
    val rnd = new scala.util.Random(7)
    val ys = Vector.fill(500)(rnd.nextDouble())
    for k <- List(0, 1, 2, 8, 64, 499, 500, 501) do
      assertEquals(A.topK[Double](k).run(ys), ys.sorted.reverse.take(k).toList,
        s"k = $k")
  }

  test("topK holds at most k however the input arrives") {
    val agg = A.topK[Double](3)
    for order <- List(xs, xs.reverse, xs.sorted, xs.sorted.reverse) do
      val acc = order.foldLeft(agg.init)(agg.add)
      assert(acc.length <= 3, s"held ${acc.length} for $order")
      assertEquals(acc, order.sorted.reverse.take(3))
  }

  test("groupBy: one aggregator per key, one pass, mergeable") {
    val agg = A.groupBy((x: Double) => x < 4)(A.sum[Double].zip(A.count[Double]))
    assertEquals(agg.run(xs), Map(true -> (7.0, 4L), false -> (24.0, 4L)))
    val (l, r) = xs.splitAt(5)
    assertEquals(
      agg.present(agg.merge(l.foldLeft(agg.init)(agg.add), r.foldLeft(agg.init)(agg.add))),
      agg.run(xs))
  }

  test("Group: combine with the inverse cancels") {
    assertEquals(5 |-| 5, 0)
    assertEquals(summon[Group[Double]].inverse(2.5), -2.5)
  }

  test("sliding window on a Group: subtract-on-age equals recompute") {
    val src = nats[Int, LazyList].take(20).toList
    val windows = sliding(nats[Int, Producer])(5).take(20).toList
    val ref = src.indices.map(i => src.slice((i - 4).max(0), i + 1).sum).toList
    assertEquals(windows, ref)
  }

  test("a window over a Monoid-only type is a compile error") {
    assert(compileErrors("sliding(fibs[Long, Producer].map?)").nonEmpty)
    assert(compileErrors(
      """val s: LazyList[String] = LazyList("a"); sliding(s)(2)""").nonEmpty)
  }

  test("contramap and fromMonoid") {
    case class P(name: String, age: Int)
    val ages = A.mean[Int].contramap[P](_.age)
    assertEquals(ages.run(List(P("a", 10), P("b", 20))), 15.0)
    assertEquals(A.fromMonoid[String].run(List("a", "b", "c")), "abc")
  }
}
