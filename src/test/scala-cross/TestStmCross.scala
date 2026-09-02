package okay

import okay.given

/** the transaction language behaves the same behind every handler:
 * tl2 on JVM/Native, direct on JS */
class TestStmCross extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  test("read, write, modify compose; the commit is atomic; the answer comes back") {
    val a = TRef(10)
    val b = TRef(0)
    val tx: Int ! Tx =
      Tx.read(a).flatMap { x =>
        Tx.write(a, x - 3).flatMap(_ => Tx.modify(b)(y => (y + 3, y + 3)))
      }
    Async.runAsync(Stm[Async].atomically(tx)).map { got =>
      assertEquals(got, 3)
      assertEquals(a.get, 7)
      assertEquals(b.get, 3)
    }
  }

  test("retry waits for the cell it read, on every platform") {
    val r = TRef(0)
    val take: Int ! Tx =
      Tx.read(r).flatMap(x => Tx.check(x > 0).flatMap(_ => Tx.write(r, x - 1).map(_ => x)))
    val waiting = Async.runAsync(Stm[Async].atomically(take))
    val writer = Async.runAsync(Stm[Async].atomically(Tx.write(r, 2)))
    writer.flatMap(_ => waiting).map { got =>
      assertEquals(got, 2)
      assertEquals(r.get, 1)
    }
  }

  test("a write followed by retry leaves nothing behind") {
    val r = TRef(0)
    val gate = TRef(false)
    val tx: Unit ! Tx =
      Tx.write(r, 99).flatMap(_ => Tx.read(gate).flatMap(g => Tx.check(g)))
    val waiting = Async.runAsync(Stm[Async].atomically(tx))
    assertEquals(r.get, 0, "a retried transaction's write leaked")
    Async.runAsync(Stm[Async].atomically(Tx.write(gate, true))).flatMap(_ => waiting).map { _ =>
      assertEquals(r.get, 99)
    }
  }
}
