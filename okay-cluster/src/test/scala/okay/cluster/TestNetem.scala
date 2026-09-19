package okay.cluster

import okay.given
import java.util.concurrent.atomic.AtomicLong

/**
 * THE NETWORK, ON ONE MACHINE (specs/dataflow.md, stage 12's
 * one-machine half; BACKLOG dataflow-netem).
 *
 * Stage 12 proper needs machines that are not this one and is not
 * pretended at. What CAN be asked here is the question that decides
 * whether the tolerance `dataflow-reconnect` chose — three
 * consecutive failures, a judgement rather than a measurement — is
 * the right number when a failure is a PACKET rather than a death:
 * at what loss rate does a run stop finishing?
 *
 * A `Serve` that fails by a SEEDED schedule stands in for the wire.
 * Seeded PER WORKER: the i-th request a worker receives is dropped or
 * not by its own splitmix stream. Which worker receives a partition's
 * i-th attempt is the fibres' order, so the counts in the tables move
 * by a run or two between runs (36 and 37 finished of 40 at 30%, in
 * two runs). The assertions below sit far from any edge for that
 * reason, and that is what lets the sweep live in the default gate
 * rather than in Live. What it does not model, said plainly: latency
 * (a failed request costs no time here), partial delivery, and a
 * partition of the network — all three are stage 12's and need the
 * machines.
 *
 * THE FINDING, in one line of the second table: with burial off, a
 * wire losing 70% of requests still finishes every run. On a lossy
 * wire the loss never ends a run — the BURIAL POLICY does. Tolerance
 * 3 reads three lost packets as a dead machine, and from 30% loss up
 * it is what turns a cluster of live machines into "no workers left".
 */
class TestNetem extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()
  val feed: Feed = Feed(20000, Late - 1)
  lazy val healthy: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, 8), FanJob.sink(feed)).runWith

  /**
   * A wire that loses a fraction of requests, decided per request by
   * a splitmix stream from `seed` — every worker its own stream, so
   * a loss on one is not a loss on all.
   */
  def lossy(base: Cluster.Serve, seed: Long, rate: Double): Cluster.Serve =
    val counter = AtomicLong(0)
    req =>
      val i = counter.incrementAndGet()
      val h = mix(seed * 1_000_003L + i)
      val u = (h >>> 11).toDouble / (1L << 53).toDouble      // [0, 1)
      if u < rate then throw java.io.IOException(f"lost (seed $seed, request $i, rate $rate%.2f)")
      else base(req)

  /** over `seeds` schedules at one loss rate: how many runs finish,
   * and what a finished one cost in lost attempts */
  def sweep(rate: Double, workers: Int, seeds: Int, tolerance: Int = 3): (Int, Long, Long) =
    var finished = 0
    var lost = 0L
    var buried = 0L
    for seed <- 1 to seeds do
      val ws = Vector.tabulate(workers)(w => lossy(Cluster.local, seed * 7919L + w, rate))
      try
        val got = Cluster.run(FanJob, feed, 8, ws, tolerance = tolerance).runWith
        assertEquals(got.value, healthy.value, s"seed $seed at $rate: a lossy wire changed the answer")
        finished += 1
        lost += got.failed
        buried += got.retried
      catch case e: IllegalStateException if e.getMessage.contains("no workers left") => ()
    (finished, lost, buried)

  test("a lossy wire never changes the answer — a run either finishes right or says it cannot") {
    for rate <- Vector(0.05, 0.2, 0.5) do sweep(rate, 4, 10): Unit
  }

  test("at what loss rate does a run stop finishing? — the tolerance, priced") {
    println(f"%n  four workers, eight partitions, 40 seeded schedules per rate, tolerance 3%n")
    println("  loss rate | finished/40 | lost attempts per finished run | workers buried")
    println("  ----------|-------------|--------------------------------|---------------")
    var lastFull = 0.0
    for rate <- Vector(0.0, 0.05, 0.10, 0.20, 0.30, 0.40, 0.50, 0.70) do
      val (f, lost, buried) = sweep(rate, 4, 40)
      if f == 40 then lastFull = rate
      println(f"  $rate%9.2f | $f%11d | ${if f == 0 then 0.0 else lost.toDouble / f}%30.1f | $buried%13d")
    println()
    // the two claims the sweep makes, and both are deterministic
    assertEquals(sweep(0.0, 4, 5)._1, 5, "a lossless wire failed to finish")
    assert(lastFull >= 0.10,
      f"tolerance 3 could not carry a 10%% loss rate over 40 schedules (held only to $lastFull%.2f)")
  }

  test("THE COUNT COUPLES TWO FAILURES: on a lossy wire, tolerance is what ends the run") {
    // a worker on a lossy wire is not dead. Burying it after three
    // consecutive lost packets treats a link as a machine, and once
    // enough workers are buried the run says "no workers left" about
    // a cluster in which every machine is fine. The second dimension
    // of the table says so: raise the count and the same wire carries.
    println(f"%n  the same wire, and how many consecutive losses are read as a death%n")
    println("  loss rate | tol 1 | tol 3 | tol 6 | tol 12 | tol 1000   (finished of 20)")
    println("  ----------|-------|-------|-------|--------|----------")
    val tols = Vector(1, 3, 6, 12, 1000)
    var at50 = Map.empty[Int, Int]
    for rate <- Vector(0.10, 0.30, 0.50, 0.70) do
      val row = tols.map(t => sweep(rate, 4, 20, t)._1)
      if rate == 0.50 then at50 = tols.zip(row).toMap
      println(f"  $rate%9.2f | ${row(0)}%5d | ${row(1)}%5d | ${row(2)}%5d | ${row(3)}%6d | ${row(4)}%8d")
    println()
    assert(at50(1000) == 20, s"with burial off, a 50% wire still failed ${20 - at50(1000)} runs — the loss is not the whole story")
    assert(at50(6) >= at50(3), s"tolerance 6 finished fewer runs than 3 at 50% loss (${at50(6)} < ${at50(3)})")
    assert(at50(3) > at50(1), "tolerance 3 did no better than 1 at 50% loss")
  }

  test("a lossy wire on EVERY worker at once, and tolerance is what carries it") {
    // the schedule stage 5 could not survive was one failure per
    // worker; a lossy wire is that, repeated — the count that decides
    // is consecutive failures, and a 20% wire rarely hands a worker
    // three in a row
    val (f, _, _) = sweep(0.20, 4, 20)
    assert(f >= 18, s"only $f of 20 runs finished at 20% loss on every worker")
  }
}
