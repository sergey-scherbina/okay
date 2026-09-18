package okay


/** The one STM law that is about a CHANNEL: it left `TestStm` in the
 * core with the channels themselves (core-modules stage 1). */
class TestStmChannel extends munit.FunSuite {

  test("the channel's cell is a TRef: a transaction can read the channel's state") {
    val c = StmChannel[Int]()
    assert(c.offer(1)); assert(c.offer(2))
    val n = Stm[Async].atomically(Tx.read(c.cell).map(_.size)).runWith
    assertEquals(n, 2)
  }
}
