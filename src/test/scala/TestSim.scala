package okay

import Sim.*

/**
 * The simulator's own battery (specs/sim.md): reproducibility as
 * trace equality, seeds exploring different interleavings, the
 * virtual clock, deadlock as an outcome, the fault plan — and the
 * bug this spec was born from, reproduced ON PURPOSE: the runCmd
 * close race, modeled, loses an answer under SOME seed with the
 * old close rule and survives a whole sweep with the new one.
 */
class TestSim extends munit.FunSuite {

  /** producers race into one channel; the consumer's order is the
   * outcome — an order-sensitive scenario for seed tests */
  def racing(order: collection.mutable.Buffer[String]): Unit ! Op =
    for
      ch <- channel[String]()
      _ <- fork(for _ <- send(ch, "a1"); _ <- send(ch, "a2") yield ())
      _ <- fork(for _ <- send(ch, "b1"); _ <- send(ch, "b2") yield ())
      _ <- receive(ch).map(_.foreach(order += _))
      _ <- receive(ch).map(_.foreach(order += _))
      _ <- receive(ch).map(_.foreach(order += _))
      _ <- receive(ch).map(_.foreach(order += _))
    yield ()

  test("the same seed is the same run: trace, outcome, order — byte for byte") {
    val o1, o2 = collection.mutable.Buffer.empty[String]
    val t1 = Sim.run(42)(racing(o1))
    val t2 = Sim.run(42)(racing(o2))
    assertEquals(t1.steps, t2.steps)
    assertEquals(o1.toList, o2.toList)
    assertEquals(t1.outcome, Outcome.Done)
  }

  test("different seeds explore different interleavings") {
    val seen = (0L until 50L).map { s =>
      val o = collection.mutable.Buffer.empty[String]
      Sim.run(s)(racing(o))
      o.toList
    }.toSet
    assert(seen.size > 1, s"fifty seeds, one interleaving: $seen")
  }

  test("the virtual clock: order by wake time, zero wall cost, time in the trace") {
    val woke = collection.mutable.Buffer.empty[String]
    val t0 = System.nanoTime()
    val t = Sim.run(7) {
      for
        _ <- fork(sleep(5000).map(_ => woke += "slow"))
        _ <- fork(sleep(1000).map(_ => woke += "fast"))
      yield ()
    }
    assert((System.nanoTime() - t0) < 1_000_000_000L, "the virtual clock cost wall time")
    assertEquals(woke.toList, List("fast", "slow"))
    assertEquals(t.virtualMillis, 5000L)
  }

  test("a lease-expiry shape runs on simulated time alone") {
    var takeovers = 0
    val t = Sim.run(3) {
      for
        _ <- fork(for _ <- sleep(2000) yield ())        // the quiet leader
        _ <- fork(for
          _ <- sleep(5000 + 1001)                       // lease + skew
          _ <- now.map(n => if n >= 6001 then takeovers += 1)
        yield ())
      yield ()
    }
    assertEquals(takeovers, 1)
    assertEquals(t.virtualMillis, 6001L)
  }

  test("a deadlock is an OUTCOME, not a hung test") {
    val t = Sim.run(1) {
      for
        a <- channel[Int]()
        b <- channel[Int]()
        _ <- fork(receive(a).flatMap(_ => send(b, 1)))
        _ <- receive(b).flatMap(_ => send(a, 1))
      yield ()
    }
    assertEquals(t.outcome, Outcome.Deadlock(2))
  }

  test("the channel contract: capacity parks a sender; close drains then ends") {
    val got = collection.mutable.Buffer.empty[Option[Int]]
    val t = Sim.run(9) {
      for
        ch <- channel[Int](capacity = 1)
        _ <- fork(for
          _ <- send(ch, 1)
          _ <- send(ch, 2)      // parks until the first is received
          _ <- close(ch)
        yield ())
        _ <- receive(ch).map(got += _)
        _ <- receive(ch).map(got += _)
        _ <- receive(ch).map(got += _)
      yield ()
    }
    assertEquals(got.toList, List(Some(1), Some(2), None))
    assertEquals(t.outcome, Outcome.Done)
  }

  test("a fault plan from the seed changes the run, and replays exactly") {
    def scenario(order: collection.mutable.Buffer[String], plan: Plan): Trace =
      val o = order
      Sim.run(5, plan) {
        for
          ch <- channel[String]()
          _ <- fork(send(ch, "x"))
          _ <- fork(send(ch, "y"))
          _ <- receive(ch).map(_.foreach(o += _))
          _ <- receive(ch).map(_.foreach(o += _))
        yield ()
      }
    val plain, delayed1, delayed2 = collection.mutable.Buffer.empty[String]
    val tp = scenario(plain, Plan())
    val td1 = scenario(delayed1, Plan(delaySendAt = Set(1)))
    val td2 = scenario(delayed2, Plan(delaySendAt = Set(1)))
    assertEquals(td1.steps, td2.steps, "the fault plan did not replay")
    assert(tp.steps != td1.steps, "the fault plan changed nothing")
  }

  // ── the bug this spec was born from, on purpose ────────────────

  /** the runCmd close protocol, modeled: an upstream event makes
   * the loop launch a command whose ANSWER must come back through
   * the same channel; the close rule decides whether the answer
   * can be lost. `counted` says what the loop actually folded. */
  def runCmdModel(newRule: Boolean, counted: collection.mutable.Buffer[String]): Unit ! Op =
    var pending = 0
    var unprocessed = 0
    var upstreamDone = false
    def closable: Boolean =
      upstreamDone && pending == 0 && (!newRule || unprocessed == 0)
    for
      ch <- channel[String]()
      // the feeder: one upstream event, then done
      _ <- fork(for
        _ <- okay.pure { unprocessed += 1 }
        _ <- send(ch, "boom")
        _ <- okay.pure { upstreamDone = true }
        _ <- if closable then close(ch) else okay.pure(())
      yield ())
      // the loop
      _ <- {
        def loop: Unit ! Op = receive[String](ch).flatMap {
          case None => okay.pure(())
          case Some("boom") =>
            counted += "boom"
            pending += 1
            unprocessed -= 1
            for
              _ <- fork(for
                _ <- okay.pure { unprocessed += 1 }
                _ <- send(ch, "answer")
                _ <- okay.pure { pending -= 1 }
                _ <- if closable then close(ch) else okay.pure(())
              yield ())
              _ <- if closable then close(ch) else okay.pure(())
              _ <- loop
            yield ()
          case Some(other) =>
            counted += other
            unprocessed -= 1
            (if closable then close(ch) else okay.pure(())).flatMap(_ => loop)
        }
        loop
      }
    yield ()

  test("the runCmd close race: the OLD rule loses the answer under some seed; the fix survives the sweep") {
    val losing = (0L until 200L).filter { s =>
      val c = collection.mutable.Buffer.empty[String]
      Sim.run(s)(runCmdModel(newRule = false, c))
      c.toList != List("boom", "answer")
    }
    assert(losing.nonEmpty,
      "two hundred seeds never lost the answer — the model does not capture the race")
    // the found seed is a VALUE: it replays byte for byte
    val s0 = losing.head
    val c1, c2 = collection.mutable.Buffer.empty[String]
    val t1 = Sim.run(s0)(runCmdModel(newRule = false, c1))
    val t2 = Sim.run(s0)(runCmdModel(newRule = false, c2))
    assertEquals(t1.steps, t2.steps)
    assertEquals(c1.toList, c2.toList)

    // and the shipped rule survives every one of the same seeds
    for s <- 0L until 200L do
      val c = collection.mutable.Buffer.empty[String]
      Sim.run(s)(runCmdModel(newRule = true, c))
      assertEquals(c.toList, List("boom", "answer"), s"the FIXED rule lost the answer under seed $s")
  }
}
