package okay

import org.scalacheck.Prop.forAll

/**
 * The persistent FIFO, checked against the obvious model: a `List` in
 * FIFO order.
 *
 * The batched paths carry index arithmetic that the channel's own laws
 * barely reach — they ask for more than is buffered, so `take` equals
 * the size and the interesting branch never runs. The one that does
 * the work is a PARTIAL take that reaches into the back: the back is
 * newest-first, so the wanted elements are its tail, and both `fill`
 * and `drop` have to find that boundary from opposite ends.
 */
class TestFifo extends munit.ScalaCheckSuite {

  /** every implementation under test, by name — a structure with two
   * plausible shapes answers for the SAME model, or the choice between
   * them is not a performance question but a behaviour one */
  private val impls: List[(String, () => Fifo[Int])] =
    List("ListFifo" -> (() => Fifo.list[Int]), "ArrayFifo" -> (() => Fifo.array[Int]))

  private def each(name: String)(law: (String, () => Fifo[Int]) => Unit): Unit =
    impls.foreach((n, mk) => test(s"$name — $n")(law(n, mk)))

  private def eachProp(name: String)(law: (String, () => Fifo[Int]) => org.scalacheck.Prop): Unit =
    impls.foreach((n, mk) => property(s"$name — $n")(law(n, mk)))

  /** the same contents, as a plain list in FIFO order — read through
   * the interface, so it says nothing about how either one stores it */
  private def model[A](q: Fifo[A]): List[A] =
    var out = List.empty[A]
    var c = q
    while c.nonEmpty do
      val (a, rest) = c.dequeue
      out = a :: out
      c = rest
    out.reverse

  private def build(mk: () => Fifo[Int], xs: List[Int]): Fifo[Int] =
    xs.foldLeft(mk())(_.enqueue(_))

  /** a buffer whose front is non-empty too, which only happens after a
   * dequeue has refilled it */
  /** a buffer whose front chunk is non-empty AND part-way consumed —
   * the shape a refill leaves behind, and the one both `drop` and
   * `fill` have to get right from opposite ends */
  private def buildMixed(mk: () => Fifo[Int], xs: List[Int], split: Int): Fifo[Int] =
    val q = build(mk, xs)
    if xs.isEmpty then q
    else
      var out = q
      var i = 0
      while i < math.min(split, xs.length) do
        val (_, rest) = out.dequeue
        out = rest
        i += 1
      out

  each("enqueue then dequeue is FIFO") { (n, mk) =>
    val q = build(mk, (1 to 5).toList)
    var out = List.empty[Int]
    var c = q
    while c.nonEmpty do
      val (a, rest) = c.dequeue
      out = a :: out
      c = rest
    assertEquals(out.reverse, (1 to 5).toList, n)
  }

  eachProp("fill answers the n oldest, in order") { (_, mk) =>
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(mk, xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      val out = ChunkBuf[Int](math.max(k, 1))
      q.fill(out, k, m.length)
      out.take(k).toList == m.take(k)
    }
  }

  eachProp("drop answers what fill did not take") { (_, mk) =>
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(mk, xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      model(q.drop(k, m.length)) == m.drop(k)
    }
  }

  eachProp("fill and drop together are the whole buffer") { (_, mk) =>
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(mk, xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      val out = ChunkBuf[Int](math.max(k, 1))
      q.fill(out, k, m.length)
      (out.take(k).toList ++ model(q.drop(k, m.length))) == m
    }
  }

  each("a partial take that reaches into the back") { (nm, mk) =>
    // front chunk = 1,2  back holds 5,4,3 (newest first) -> 1..5
    // built through the interface so both shapes reach it: five
    // enqueued, two taken, so the front is part-way consumed
    val q0 = build(mk, (1 to 5).toList)
    val (_, q1) = q0.dequeue
    val q = q1
    assertEquals(model(q), List(2, 3, 4, 5), nm)
    val out = ChunkBuf[Int](3)
    q.fill(out, 3, 4)
    assertEquals(out.take(3).toList, List(2, 3, 4), nm)
    assertEquals(model(q.drop(3, 4)), List(5), nm)
  }
}
