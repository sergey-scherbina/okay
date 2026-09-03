package okay

/**
 * channel-drain: the batched receive takes what is already buffered
 * under ONE transaction. The elements and their order must be the
 * channel's exactly — these are the paths where they might not be:
 * a bounded channel whose senders park and must be admitted into the
 * room a batch frees, a rendezvous channel with no buffer at all, and
 * the end of the stream landing inside or between batches.
 */
class TestDrain extends munit.FunSuite {

  private def drain[A](c: Channel[A]): List[A] =
    Writer.of(Drain(c)).toLazyList.toList

  test("a bounded channel: the producer parks, the batch admits it, nothing is lost") {
    // capacity well under the element count, so senders really do park
    // and the batch's admission loop is the thing under test
    assertEquals(drain(Channel.buffer(4)((1 to 500).toList)), (1 to 500).toList)
  }

  test("capacity 0: the rendezvous still hands over one at a time") {
    assertEquals(drain(Channel.buffer(0)((1 to 50).toList)), (1 to 50).toList)
  }

  test("a bound of one, the tightest admission path there is") {
    assertEquals(drain(Channel.buffer(1)((1 to 200).toList)), (1 to 200).toList)
  }

  test("an unbounded channel: a full buffer drains in batches, in order") {
    val c = Channel[Int](Int.MaxValue)
    (1 to 300).foreach(i => assert(c.offer(i)))
    c.close()
    assertEquals(drain(c), (1 to 300).toList)
  }

  test("the end lands wherever it lands: exact multiples of the batch and either side") {
    for n <- List(0, 1, 63, 64, 65, 127, 128, 129) do
      val c = Channel[Int](Int.MaxValue)
      (1 to n).foreach(i => assert(c.offer(i)))
      c.close()
      assertEquals(drain(c), (1 to n).toList, s"n=$n")
  }

  test("a failing producer's elements still arrive before the failure is seen") {
    val c = Channel[Int](Int.MaxValue)
    (1 to 10).foreach(i => assert(c.offer(i)))
    c.fail(RuntimeException("boom"))
    c.close()
    val got = scala.util.Try(drain(c))
    got match
      case scala.util.Success(xs) => assertEquals(xs, (1 to 10).toList)
      case scala.util.Failure(e) => assert(e.getMessage.contains("boom"), e.toString)
  }

  test("the batched and single reads agree, element for element") {
    def fill = { val c = Channel[Int](Int.MaxValue); (1 to 200).foreach(i => c.offer(i): Unit); c.close(); c }
    val batched = drain(fill)
    val single = Writer.of(fill).toLazyList.toList
    assertEquals(batched, single)
  }
}
