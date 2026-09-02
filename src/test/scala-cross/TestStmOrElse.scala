package okay

import okay.given

/** `Tx.orElse` (specs/stm.md, stm-orelse): the classic STM
 * combinator, behind every handler (tl2 on JVM/Native, direct on
 * JS) — a retried branch leaves nothing behind, a successful one's
 * writes are the transaction's own. */
class TestStmOrElse extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  test("a succeeds: its write commits, b's own write never happens") {
    val r = TRef(0)
    val bMark = TRef(0)
    val tx: Int ! Tx = Tx.orElse(
      Tx.write(r, 1).map(_ => 1),
      Tx.write(bMark, 1).flatMap(_ => Tx.write(r, 2)).map(_ => 2))
    Async.runAsync(Stm[Async].atomically(tx)).map { got =>
      assertEquals(got, 1)
      assertEquals(r.get, 1)
      assertEquals(bMark.get, 0, "b ran even though a succeeded")
    }
  }

  test("a retries: b runs instead, and a's write never lands") {
    val r = TRef(0)
    val gate = TRef(false)
    // a writes r, THEN discovers the gate is shut and retries — the
    // write must not survive even though it happened before the retry
    val a: Int ! Tx = Tx.write(r, 99).flatMap(_ => Tx.read(gate).flatMap(g => Tx.check(g)).map(_ => 1))
    val b: Int ! Tx = pure(2)
    Async.runAsync(Stm[Async].atomically(Tx.orElse(a, b))).map { got =>
      assertEquals(got, 2)
      assertEquals(r.get, 0, "a's write leaked past its own retry")
    }
  }

  test("both retry: the whole orElse retries, parked on EITHER branch's reads") {
    val left = TRef(false)
    val right = TRef(false)
    val tx: String ! Tx = Tx.orElse(
      Tx.read(left).flatMap(l => Tx.check(l)).map(_ => "left"),
      Tx.read(right).flatMap(r => Tx.check(r)).map(_ => "right"))
    val waiting = Async.runAsync(Stm[Async].atomically(tx))
    // only RIGHT ever becomes true: proves the wait covers the branch
    // that was never tried again after the first (both had retried,
    // so both were read and both are watched)
    val writer = Async.runAsync(Stm[Async].atomically(Tx.write(right, true)))
    writer.flatMap(_ => waiting).map(got => assertEquals(got, "right"))
  }

  test("nested orElse picks the first that does not retry, in order") {
    val tx: String ! Tx = Tx.orElse(
      Tx.retry[String],
      Tx.orElse(Tx.retry[String], pure("third")))
    Async.runAsync(Stm[Async].atomically(tx)).map(got => assertEquals(got, "third"))
  }

  test("a write made BEFORE orElse is visible inside a branch (the enclosing log, not lost)") {
    val r = TRef(0)
    val tx: Int ! Tx =
      Tx.write(r, 7).flatMap(_ => Tx.orElse(Tx.read(r), pure(-1)))
    Async.runAsync(Stm[Async].atomically(tx)).map(got => assertEquals(got, 7))
  }
}
