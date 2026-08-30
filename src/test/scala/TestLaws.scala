package okay

import org.scalacheck.Prop.forAll

/** The algebra's laws, property-checked. */
class TestLaws extends munit.ScalaCheckSuite {

  property("Group[Int]: associativity, identity, inverse") {
    forAll { (a: Int, b: Int, c: Int) =>
      ((a |+| b) |+| c) == (a |+| (b |+| c))
        && (a |+| summon[Group[Int]].empty) == a
        && (a |-| a) == 0
    }
  }

  property("Group[Long]: inverse cancels through combines") {
    forAll { (a: Long, b: Long) => ((a |+| b) |-| b) == a }
  }

  property("Monoid[String]: associativity and identity") {
    forAll { (a: String, b: String, c: String) =>
      ((a |+| b) |+| c) == (a |+| (b |+| c)) && (a |+| "") == a && ("" |+| a) == a
    }
  }

  property("Aggregator merges are split-point-agnostic (sum, count, min)") {
    forAll { (xs: List[Int], at: Byte) =>
      val i = if xs.isEmpty then 0 else math.floorMod(at.toInt, xs.length + 1)
      val (l, r) = xs.splitAt(i)
      def both[Acc, Out](a: Aggregator[Int, Acc, Out]): Boolean =
        a.present(a.merge(l.foldLeft(a.init)(a.add), r.foldLeft(a.init)(a.add))) == a.run(xs)
      both(Aggregator.sum[Int]) && both(Aggregator.count[Int]) && both(Aggregator.min[Int])
    }
  }

  property("sliding window on the Int group equals recompute, any n") {
    forAll { (xs: List[Int], nRaw: Byte) =>
      val n = math.floorMod(nRaw.toInt, 5) + 1
      val s: LazyList[Int] = LazyList.from(xs)
      val ref = xs.indices.map(i => xs.slice((i - n + 1).max(0), i + 1).sum).toList
      sliding(s)(n).toList == ref
    }
  }
}
