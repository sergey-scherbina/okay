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
