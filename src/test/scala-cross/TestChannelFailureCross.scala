package okay

import okay.given

/**
 * A producer's failure must reach the consumer — on both platforms,
 * from one source. The JVM channel parks and the JS channel leaves a
 * callback, so this is the same contract over two entirely different
 * mechanisms, which is exactly the kind of thing that drifts apart
 * unless one test holds both.
 */
class TestChannelFailureCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  final case class Boom[A](n: Int)
  given Stream[Boom, Pure] with
    def uncons[A](b: Boom[A]): Option[(A, Boom[A])] ! Pure =
      if b.n >= 3 then throw RuntimeException("the producer failed")
      else pure(Some(((b.n + 1).asInstanceOf[A], Boom[A](b.n + 1))))

  test("a failed producer fails the consumer's program, not silently") {
    val c = Channel.buffer[Int, Boom, Pure](8)(Boom[Int](0))

    // drain it as a Stream, which is how anyone actually consumes one
    val prog: List[Int] ! Async =
      def go(acc: List[Int]): List[Int] ! Async =
        summon[Stream[Channel, Async]].uncons(c).flatMap {
          case Some((a, rest)) => go(a :: acc)
          case None => pure(acc.reverse)
        }
      go(Nil)

    Async.runAsync(prog).failed.map { e =>
      assert(e.getMessage.contains("the producer failed"),
        s"the stream ended with ${e.getMessage} instead of the producer's error")
    }
  }

  test("a healthy stream still ends cleanly") {
    val c = Channel.buffer[Int, LazyList, Pure](8)(LazyList(1, 2, 3))
    val prog: List[Int] ! Async =
      def go(acc: List[Int]): List[Int] ! Async =
        summon[Stream[Channel, Async]].uncons(c).flatMap {
          case Some((a, _)) => go(a :: acc)
          case None => pure(acc.reverse)
        }
      go(Nil)
    Async.runAsync(prog).map(xs => assertEquals(xs, List(1, 2, 3)))
  }
}
