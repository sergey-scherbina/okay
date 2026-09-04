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

  /** the same contents, as a plain list in FIFO order */
  private def model[A](q: Fifo[A]): List[A] = q.front ++ q.back.reverse

  private def build(xs: List[Int]): Fifo[Int] =
    xs.foldLeft(Fifo.empty[Int])(_.enqueue(_))

  /** a buffer whose front is non-empty too, which only happens after a
   * dequeue has refilled it */
  private def buildMixed(xs: List[Int], split: Int): Fifo[Int] =
    val q = build(xs)
    if xs.isEmpty then q
    else
      var out = q
      var i = 0
      // dequeue `split` and put them back, which moves the boundary
      var taken = List.empty[Int]
      while i < math.min(split, xs.length) do
        val (a, rest) = out.dequeue
        taken = a :: taken
        out = rest
        i += 1
      taken.foldRight(out)((a, acc) => Fifo(a :: acc.front, acc.back))

  test("enqueue then dequeue is FIFO") {
    val q = build((1 to 5).toList)
    var out = List.empty[Int]
    var c = q
    while c.nonEmpty do
      val (a, rest) = c.dequeue
      out = a :: out
      c = rest
    assertEquals(out.reverse, (1 to 5).toList)
  }

  property("fill answers the n oldest, in order") {
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      val out = ChunkBuf[Int](math.max(k, 1))
      q.fill(out, k, m.length)
      out.take(k).toList == m.take(k)
    }
  }

  property("drop answers what fill did not take") {
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      model(q.drop(k, m.length)) == m.drop(k)
    }
  }

  property("fill and drop together are the whole buffer") {
    forAll { (xs: List[Int], split: Int, n: Int) =>
      val q = buildMixed(xs, math.abs(split % 8))
      val m = model(q)
      val k = if m.isEmpty then 0 else math.abs(n % (m.length + 1))
      val out = ChunkBuf[Int](math.max(k, 1))
      q.fill(out, k, m.length)
      (out.take(k).toList ++ model(q.drop(k, m.length))) == m
    }
  }

  test("a partial take that reaches into the back") {
    // front = 1,2  back holds 5,4,3 (newest first) -> FIFO 1,2,3,4,5
    val q = Fifo(List(1, 2), List(5, 4, 3))
    assertEquals(model(q), List(1, 2, 3, 4, 5))
    val out = ChunkBuf[Int](4)
    q.fill(out, 4, 5)
    assertEquals(out.take(4).toList, List(1, 2, 3, 4))
    assertEquals(model(q.drop(4, 5)), List(5))
  }
}
