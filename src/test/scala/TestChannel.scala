package okay

import !.*

/** Channels: the concurrency primitive of streams — merge and buffer. */
class TestChannel extends munit.FunSuite {

  test("a channel is a linear async stream: send, close, drain") {
    val c = Channel[Int]()
    c.send(1); c.send(2); c.close()
    assertEquals(c.toLazyList.toList, List(1, 2))
    assertEquals(c.receive(), None)
  }

  test("merge combines two async streams by readiness") {
    type S = Produce + Async
    def ticks(xs: List[Int]): Int ! S = xs match
      case Nil => pure(0)
      case x :: t => effect[S, Unit](Async.Run(() => Thread.sleep(1))).flatMap: _ =>
        effect[S, Int](x).flatMap(_ => ticks(t))
    val merged = Channel.merge(ticks(List(1, 3, 5)), ticks(List(2, 4, 6)))
    assertEquals(merged.toLazyList.toList.sorted, List(1, 2, 3, 4, 5, 6))
  }

  test("buffer runs the producer ahead, at most capacity elements") {
    val c = Channel.buffer(2)(fibs[Long, Producer].take(10): LazyList[Long])
    assertEquals(c.toLazyList.toList, fibs[Long, LazyList].take(10).toList)
  }
}
