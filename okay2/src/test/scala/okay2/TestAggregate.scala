package okay2

import okay2.{Aggregator => A}
import Generate._

object TestAggregateRows {
  final case class Row(id: Int, score: Double)
  implicit val byScore: Ordering[Row] = Ordering.by(_.score)
  final case class P(name: String, age: Int)
}

/** The aggregation algebra: one-pass composition, mergeable partials,
 * groups and sliding windows — the Scala 3 core's TestAggregate */
class TestAggregate extends munit.FunSuite {
  import TestAggregateRows._

  val xs = List(3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0)

  def half[Acc](agg: Aggregator[Double, Acc, _], part: List[Double]): Acc = part.foldLeft(agg.init)(agg.add)

  test("zip: two statistics in ONE pass (the source is consumed once)") {
    var steps = 0
    def probe(rest: List[Int]): Producer[Int] = rest match {
      case Nil => pure(0)
      case x :: t => produce({ steps += 1; x }).flatMap(_ => probe(t))
    }
    val agg = A.sum[Int].zip(A.count[Int])
    val acc = Stream.fold[Producer, Pure, Int, (Int, Long)](probe(List(1, 2, 3, 4)))(agg.fold)
    assertEquals(agg.present(acc), (10, 4L))
    assertEquals(steps, 4)
  }

  test("an unboxed aggregator IS its own fold, so Stream.fold's dispatch keeps it primitive") {
    val c = A.count[Int]
    assert(c.fold[Int] eq c)
    assert(!A.mean[Double].fold[Double].isInstanceOf[Fold.OfDouble[_]])
    assert(A.sum[Long].fold[Long].isInstanceOf[Fold.OfLong[_]])
    assert(A.sum[Double].fold[Double].isInstanceOf[Fold.OfDouble[_]])
    assertEquals(Stream.fold[List, Pure, Long, Long](List(1L, 2L, 3L))(A.sum[Long].fold), 6L)
    assertEquals(A.sum[BigInt].run(List(BigInt(1), BigInt(2))), BigInt(3))
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
    val tupled = A.count[Long].zip(A.sum[Long])
    val flat = A.count[Long].zipLong(A.sum[Long])
    assertEquals(flat.run(ns), tupled.run(ns))
    assertEquals(flat.run(Nil), tupled.run(Nil))
    def part[Acc](agg: Aggregator[Long, Acc, _], p: List[Long]): Acc = p.foldLeft(agg.init)(agg.add)
    for (cut <- 0 to ns.length)
      assertEquals(
        flat.present(flat.merge(part(flat, ns.take(cut)), part(flat, ns.drop(cut)))),
        tupled.present(tupled.merge(part(tupled, ns.take(cut)), part(tupled, ns.drop(cut)))),
        s"a cut at $cut disagrees")
  }

  test("summary: the flat accumulator answers what the zip answers") {
    val ns = List(3L, 1L, 4L, 1L, 5L, 9L, 2L, 6L)
    val zipped = A.count[Long].zip(A.sum[Long]).zip(A.min[Long]).zip(A.max[Long])
    val (((n, sum), mn), mx) = zipped.run(ns)
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
    for (cut <- 0 to ns.length) {
      val merged = agg.merge(ns.take(cut).foldLeft(agg.init)(agg.add), ns.drop(cut).foldLeft(agg.init)(agg.add))
      assertEquals(agg.present(merged), whole, s"a cut at $cut disagrees")
    }
  }

  test("merge: split-and-combine equals the whole (the distributed contract)") {
    val (l, r) = xs.splitAt(3)
    def go[Acc](agg: Aggregator[Double, Acc, _]): Unit =
      assertEquals(agg.present(agg.merge(half(agg, l), half(agg, r))), agg.present(half(agg, xs)))
    go(A.sum[Double]); go(A.count[Double]); go(A.min[Double]); go(A.max[Double])
    go(A.topK[Double](3)); go(A.distinct[Double])
    // variance merges by Chan/Golub/LeVeque: equal up to floating error
    val v = A.variance[Double]
    assert(math.abs(v.present(v.merge(half(v, l), half(v, r))) - v.present(half(v, xs))) < 1e-9)
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

  test("topK refuses an element that only TIES the k-th, so the FIRST seen survives") {
    val rows = Vector(Row(1, 5.0), Row(2, 9.0), Row(3, 5.0), Row(4, 1.0))
    assertEquals(A.topK[Row](2).run(rows).map(_.id), List(2, 1))
  }

  test("topK agrees with sorting the whole input, at every k") {
    val rnd = new scala.util.Random(7)
    val ys = Vector.fill(500)(rnd.nextDouble())
    for (k <- List(0, 1, 2, 8, 64, 499, 500, 501))
      assertEquals(A.topK[Double](k).run(ys), ys.sorted.reverse.take(k).toList, s"k = $k")
  }

  test("topK holds at most k however the input arrives") {
    val agg = A.topK[Double](3)
    for (order <- List(xs, xs.reverse, xs.sorted, xs.sorted.reverse)) {
      val acc = order.foldLeft(agg.init)(agg.add)
      assert(acc.length <= 3, s"held ${acc.length} for $order")
      assertEquals(acc, order.sorted.reverse.take(3))
    }
  }

  test("groupBy: one aggregator per key, one pass, mergeable") {
    val agg = A.groupBy((x: Double) => x < 4)(A.sum[Double].zip(A.count[Double]))
    assertEquals(agg.run(xs), Map(true -> ((7.0, 4L)), false -> ((24.0, 4L))))
    val (l, r) = xs.splitAt(5)
    assertEquals(agg.present(agg.merge(l.foldLeft(agg.init)(agg.add), r.foldLeft(agg.init)(agg.add))), agg.run(xs))
  }

  test("sliding window on a Group: subtract-on-age equals recompute") {
    val src = nats[Int, LazyList].take(20).toList
    val windows = sliding(nats[Int, Producer])(5).take(20).toList
    val ref = src.indices.map(i => src.slice((i - 4).max(0), i + 1).sum).toList
    assertEquals(windows, ref)
  }

  test("a window over a Monoid-only type is a compile error") {
    assert(compileErrors("""val s: LazyList[String] = LazyList("a"); okay2.sliding(s)(2)""").nonEmpty)
  }

  test("contramap, map and fromMonoid") {
    val ages = A.mean[Int].contramap[P](_.age)
    assertEquals(ages.run(List(P("a", 10), P("b", 20))), 15.0)
    assertEquals(A.fromMonoid[String].run(List("a", "b", "c")), "abc")
    assertEquals(A.count[Int].map(_ * 2).run(List(1, 2, 3)), 6L)
  }
}
