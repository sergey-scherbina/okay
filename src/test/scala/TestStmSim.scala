package okay

/** the same transaction code under the deterministic scheduler:
 * every interleaving by seed, the invariant at every one */
class TestStmSim extends munit.FunSuite {

  def transfer(a: TRef[Long], b: TRef[Long], amt: Long): Unit ! Tx =
    Tx.read(a).flatMap(x => Tx.read(b).flatMap(y =>
      Tx.write(a, x - amt).flatMap(_ => Tx.write(b, y + amt))))

  test("two fibers transferring both ways: the sum holds under every seed, and the scheduler DID interleave") {
    var interleaved = 0
    for seed <- 1L to 60L do
      val a = TRef(100L)
      val b = TRef(100L)
      def mover(amt: Long, n: Int): Unit ! Sim.Op =
        if n == 0 then pure(())
        else Stm.sim.atomically(transfer(a, b, amt)).flatMap(_ => mover(amt, n - 1))
      val main: Unit ! Sim.Op =
        Sim.fork(mover(7, 5)).flatMap(_ => Sim.fork(mover(-3, 5))).map(_ => ())
      val trace = Sim.run(seed)(main)
      assertEquals(trace.outcome, Sim.Outcome.Done, s"seed $seed: ${trace.outcome}")
      assertEquals(a.get + b.get, 200L, s"seed $seed")
      // yields from both fibers alternate somewhere in the trace
      val yields = trace.steps.filter(_.endsWith(":yield")).map(_.takeWhile(_ != ':'))
      if yields.sliding(2).exists(p => p.length == 2 && p(0) != p(1)) then interleaved += 1
    assert(interleaved > 0, "no seed ever interleaved the two transactions")
  }

  test("retry under Sim: the waiter sleeps a virtual millisecond at a time until the writer commits") {
    val r = TRef(0)
    var got = -1
    val take: Int ! Tx =
      Tx.read(r).flatMap(x => Tx.check(x > 0).flatMap(_ => Tx.write(r, x - 1).map(_ => x)))
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
}
