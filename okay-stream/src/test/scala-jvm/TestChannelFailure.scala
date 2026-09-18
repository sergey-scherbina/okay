package okay

import okay.given

/**
 * What a consumer sees when a PRODUCER fails.
 *
 * `Channel.merge` and `Channel.buffer` fork a fiber per source and
 * discard it — deliberately, because completion is signalled by the
 * channel closing rather than by a join. The question that raises is
 * whether a failure inside a producer reaches the consumer at all, or
 * whether the `finally` closes the channel and the consumer reads a
 * perfectly ordinary end of stream.
 *
 * Silent truncation is the worst possible answer here: a merge that
 * quietly returns half its elements is indistinguishable from one
 * that legitimately had that many.
 */
class TestChannelFailure extends munit.FunSuite {

  given Scheduler = Schedulers.loom

  /** a stream of three elements that throws instead of yielding a fourth */
  final case class Boom[A](n: Int)
  given Stream[Boom, Pure] with
    def uncons[A](b: Boom[A]): Option[(A, Boom[A])] ! Pure =
      if b.n >= 3 then throw RuntimeException("the producer failed")
      else pure(Some(((b.n + 1).asInstanceOf[A], Boom[A](b.n + 1))))

  test("a producer failure in buffer: what does the consumer see?") {
    val c = Channel.buffer[Int, Boom, Pure](8)(Boom[Int](0))
    val got = scala.collection.mutable.Buffer[Int]()
    var outcome = "drained cleanly"
    try
      var more = true
      while more do
        c.receiveBlocking() match
          case Some(a) => got += a
          case None => more = false
    catch case e: Throwable => outcome = s"threw: ${e.getMessage}"

    // the buffered elements still arrive; the failure is the END
    assertEquals(got.toList, List(1, 2, 3))
    assertEquals(outcome, "threw: the producer failed",
      "a failed producer looked like a stream that simply ended")
  }

  test("a producer failure in merge: the other source still completes") {
    val c = Channel.merge[Int, Boom, Pure, LazyList, Pure](Boom[Int](0), LazyList(10, 20))
    val got = scala.collection.mutable.Buffer[Int]()
    var failed = false
    try
      var more = true
      while more do
        c.receiveBlocking() match
          case Some(a) => got += a
          case None => more = false
    catch case _: Throwable => failed = true

    // the healthy source still delivered everything it had...
    assert(got.contains(10) && got.contains(20),
      s"the healthy source was lost: ${got.toList.sorted}")
    assertEquals(got.toList.sorted, List(1, 2, 3, 10, 20))
    // ...and the merge still reports that one side broke
    assert(failed, "a merge with a failed source ended as if all was well")
  }
}
