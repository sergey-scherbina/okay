package okay2

import okay2.async._

/** the transaction language behaves the same behind every handler: tl2
 * and direct — okay-stm's TestStmCross, run over both runtimes */
class TestStmCross extends munit.FunSuite {
  implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext

  for ((name, stm) <- List("tl2" -> Stm.tl2, "direct" -> Stm.direct)) {
    test(s"$name: read, write, modify compose; the commit is atomic; the answer comes back") {
      val a = TRef(10)
      val b = TRef(0)
      val tx: Int ! Tx = Tx.read(a).flatMap(x => Tx.write(a, x - 3).flatMap(_ => Tx.modify(b)(y => (y + 3, y + 3))))
      Async.runAsync(stm.atomically(tx)).map { got =>
        assertEquals(got, 3)
        assertEquals(a.get, 7)
        assertEquals(b.get, 3)
      }
    }

    test(s"$name: retry waits for the cell it read") {
      val r = TRef(0)
      val take: Int ! Tx = Tx.read(r).flatMap(x => Tx.check(x > 0).flatMap(_ => Tx.write(r, x - 1).map(_ => x)))
      val waiting = Async.runAsync(stm.atomically(take))
      val writer = Async.runAsync(stm.atomically(Tx.write(r, 2)))
      writer.flatMap(_ => waiting).map { got =>
        assertEquals(got, 2)
        assertEquals(r.get, 1)
      }
    }

    test(s"$name: a write followed by retry leaves nothing behind") {
      val r = TRef(0)
      val gate = TRef(false)
      val tx: Unit ! Tx = Tx.write(r, 99).flatMap(_ => Tx.read(gate).flatMap(g => Tx.check(g)))
      val waiting = Async.runAsync(stm.atomically(tx))
      assertEquals(r.get, 0, "a retried transaction's write leaked")
      Async.runAsync(stm.atomically(Tx.write(gate, true))).flatMap(_ => waiting).map(_ => assertEquals(r.get, 99))
    }
  }
}

/** `Tx.orElse`, the classic STM combinator — okay-stm's TestStmOrElse */
class TestStmOrElse extends munit.FunSuite {
  implicit val ec: scala.concurrent.ExecutionContext = munitExecutionContext

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
    val a: Int ! Tx = Tx.write(r, 99).flatMap(_ => Tx.read(gate).flatMap(g => Tx.check(g)).map(_ => 1))
    val b: Int ! Tx = pure[Tx, Int](2)
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
    val writer = Async.runAsync(Stm[Async].atomically(Tx.write(right, true)))
    writer.flatMap(_ => waiting).map(got => assertEquals(got, "right"))
  }

  test("nested orElse picks the first that does not retry, in order") {
    val tx: String ! Tx = Tx.orElse(Tx.retry[String], Tx.orElse(Tx.retry[String], pure[Tx, String]("third")))
    Async.runAsync(Stm[Async].atomically(tx)).map(got => assertEquals(got, "third"))
  }

  test("a write made BEFORE orElse is visible inside a branch (the enclosing log, not lost)") {
    val r = TRef(0)
    val tx: Int ! Tx = Tx.write(r, 7).flatMap(_ => Tx.orElse(Tx.read(r), pure[Tx, Int](-1)))
    Async.runAsync(Stm[Async].atomically(tx)).map(got => assertEquals(got, 7))
  }
}

/** the same transaction code under the deterministic scheduler: every
 * interleaving by seed, the invariant at every one — okay-stm's TestStmSim */
class TestStmSim extends munit.FunSuite {

  def transfer(a: TRef[Long], b: TRef[Long], amt: Long): Unit ! Tx =
    Tx.read(a).flatMap(x => Tx.read(b).flatMap(y => Tx.write(a, x - amt).flatMap(_ => Tx.write(b, y + amt))))

  test("two fibers transferring both ways: the sum holds under every seed, and the scheduler DID interleave") {
    var interleaved = 0
    for (seed <- 1L to 60L) {
      val a = TRef(100L)
      val b = TRef(100L)
      def mover(amt: Long, n: Int): Unit ! Sim.Op =
        if (n == 0) pure[Sim.Op, Unit](())
        else Stm.sim.atomically(transfer(a, b, amt)).flatMap(_ => mover(amt, n - 1))
      val main: Unit ! Sim.Op = Sim.fork(mover(7, 5)).flatMap(_ => Sim.fork(mover(-3, 5))).map(_ => ())
      val trace = Sim.run(seed)(main)
      assertEquals(trace.outcome, Sim.Outcome.Done, s"seed $seed: ${trace.outcome}")
      assertEquals(a.get + b.get, 200L, s"seed $seed")
      val yields = trace.steps.filter(_.endsWith(":yield")).map(_.takeWhile(_ != ':'))
      if (yields.sliding(2).exists(p => p.length == 2 && p(0) != p(1))) interleaved += 1
    }
    assert(interleaved > 0, "no seed ever interleaved the two transactions")
  }

  test("retry under Sim: the waiter sleeps a virtual millisecond at a time until the writer commits") {
    val r = TRef(0)
    var got = -1
    val take: Int ! Tx = Tx.read(r).flatMap(x => Tx.check(x > 0).flatMap(_ => Tx.write(r, x - 1).map(_ => x)))
    val main: Unit ! Sim.Op =
      Sim.fork(Stm.sim.atomically(take).map(v => got = v)).flatMap { _ =>
        Sim.sleep(5).flatMap(_ => Stm.sim.atomically(Tx.write(r, 3)))
      }
    val trace = Sim.run(7)(main)
    assertEquals(trace.outcome, Sim.Outcome.Done)
    assertEquals(got, 3)
    assertEquals(r.get, 2)
    assert(trace.virtualMillis >= 5, s"virtual time ${trace.virtualMillis}")
  }

  test("orElse under Sim: a waiter races two writers, every seed picks whichever fires and only that one") {
    var picked = -1
    for (seed <- 1L to 60L) {
      val left = TRef(0)
      val right = TRef(0)
      picked = -1
      val waiter: String ! Tx = Tx.orElse(
        Tx.read(left).flatMap(x => Tx.check(x > 0)).map(_ => "left"),
        Tx.read(right).flatMap(x => Tx.check(x > 0)).map(_ => "right"))
      val main: Unit ! Sim.Op =
        Sim.fork(Stm.sim.atomically(waiter).map(v => picked = (if (v == "left") 1 else 2))).flatMap { _ =>
          Sim.sleep(1).flatMap(_ => Sim.fork(Stm.sim.atomically(Tx.write(left, 1))))
            .flatMap(_ => Sim.fork(Stm.sim.atomically(Tx.write(right, 1))))
        }.map(_ => ())
      val trace = Sim.run(seed)(main)
      assertEquals(trace.outcome, Sim.Outcome.Done, s"seed $seed: ${trace.outcome}")
      assert(picked == 1 || picked == 2, s"seed $seed: orElse never resolved")
      assertEquals(left.get, 1, s"seed $seed")
      assertEquals(right.get, 1, s"seed $seed")
    }
  }
}
