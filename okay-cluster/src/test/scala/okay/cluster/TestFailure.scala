package okay.cluster

import okay.given
import java.util.concurrent.atomic.AtomicInteger

/**
 * A WORKER DIES AND THE JOB DOES NOT (specs/dataflow.md, stage 5).
 *
 * What makes this cheap is structural rather than clever: a partition
 * is a THUNK and its partial is a pure function of the four things
 * every worker is given — the parameters, the index, the count and
 * the bounds. So recovery is "ask someone else the same question".
 * There is no lineage graph to walk and no checkpoint to restore,
 * because nothing was mutated.
 *
 * The failures here are SEEDED. Which attempt on which worker dies is
 * drawn from a seed, so a sweep explores many schedules and a red run
 * names the seed that produced it — rather than a flake that proves
 * a bug exists and teaches nothing about where.
 */
class TestFailure extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()

  val feed: Feed = Feed(20000, Late - 1)

  lazy val healthy: Run[((Sum, Sum), Sum)] =
    Flows.fan(FanJob.flow(feed, 8), FanJob.sink(feed)).runWith

  /**
   * A worker that dies at a chosen request and stays dead.
   *
   * STAYS DEAD is the honest model, not a simplification: a `Serve`
   * built by `Served.connect` IS a connection, and a broken
   * connection does not heal — the burial in `Cluster.ask` matches
   * what the transport actually does. It also means a run cannot
   * survive a blip on EVERY worker, which the first version of the
   * seeded test below discovered by asking for exactly that. Named in
   * the spec rather than smoothed over.
   */
  def doomed(base: Cluster.Serve, at: Int): Cluster.Serve =
    val n = AtomicInteger(0)
    req =>
      if n.incrementAndGet() >= at then throw java.io.IOException(s"worker died at request $at")
      else base(req)

  test("a worker that dies is buried, and its partition is computed elsewhere") {
    // one of three workers throws on everything: the run must still
    // finish, on the other two
    val dead: Cluster.Serve = _ => throw java.io.IOException("this one is gone")
    val got = Cluster.run(FanJob, feed, 8, Vector(dead, Cluster.local, Cluster.local)).runWith
    assertEquals(got.value, healthy.value)
    assertEquals(got.merged, healthy.merged)
    assert(got.retried > 0, "nothing was retried — the failure did not happen")
    assertEquals(got.retried, 1L, "a dead worker is buried ONCE, not once per partition")
  }

  test("seeded failure schedules: forty of them, and the answer never moves") {
    // each seed dooms a different subset of the workers at a
    // different request — some machines die, some do not, which is
    // the shape a run actually meets. At least one survives, because
    // a run with no workers left has no answer to be right about.
    var buried = 0L
    for seed <- 1L to 40L do
      val n = 5
      val dying = 1 + math.floorMod(mix(seed), (n - 1).toLong).toInt   // 1 .. n-1 of them
      val workers = Vector.tabulate(n) { i =>
        if i < dying then doomed(Cluster.local, 1 + math.floorMod(mix(seed * 31 + i), 6L).toInt)
        else Cluster.local
      }
      val got = Cluster.run(FanJob, feed, 8, workers).runWith
      assertEquals(got.value, healthy.value, s"seed $seed, $dying of $n doomed")
      assertEquals(got.dropped, healthy.dropped, s"seed $seed")
      assertEquals(got.merged, healthy.merged, s"seed $seed")
      // NOT `== dying`: a worker doomed at its sixth request may
      // never be asked six times, because the run finishes first.
      // Seed 2 is where that assertion was too strong.
      assert(got.retried <= dying, s"seed $seed buried ${got.retried} of $dying doomed")
      buried += got.retried
    assert(buried > 0, "forty schedules and nothing ever died — the injection is not working")
  }

  test("one survivor is enough") {
    val workers = Vector.tabulate(4)(i => if i < 3 then doomed(Cluster.local, 1) else Cluster.local)
    val got = Cluster.run(FanJob, feed, 8, workers).runWith
    assertEquals(got.value, healthy.value)
    assertEquals(got.retried, 3L)
  }

  test("when every worker is gone the run says so, and names the first cause") {
    val dead: Cluster.Serve = _ => throw java.io.IOException("the machine room is on fire")
    val e = intercept[IllegalStateException](
      Cluster.run(WindowJob, feed, 4, Vector(dead, dead)).runWith)
    assert(e.getMessage.contains("no workers left"), e.getMessage)
    assert(e.getCause != null, "the original failure was thrown away")
    assert(e.getCause.getMessage.contains("machine room"), e.getCause.getMessage)
  }

  test("a considered refusal is returned, not retried on every worker in turn") {
    // an unknown job is a deterministic answer: every worker runs the
    // same build, so asking three more produces the identical
    // refusal. Retrying a deterministic 'no' is noise in front of the
    // same message.
    object Stranger extends Job[Feed, Sum] {
      type A = Ev
      def name: String = "test.nowhere"
      def params: okay.codec.Schema[Feed] = summon[okay.codec.Schema[Feed]]
      def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
      def sink(f: Feed): Wire[Ev, Sum] = WindowJob.sink(f)
    }
    val asked = AtomicInteger(0)
    val counting: Cluster.Serve = req => { asked.incrementAndGet(): Unit; Cluster.local(req) }
    val e = intercept[IllegalStateException](
      Cluster.run(Stranger, feed, 4, Vector.fill(4)(counting)).runWith)
    assert(e.getMessage.contains("no job named"), e.getMessage)
    assert(asked.get <= 4, s"a deterministic refusal was retried: ${asked.get} requests for 4 partitions")
  }

  test("A REAL WORKER PROCESS IS KILLED MID-RUN, and the job finishes") {
    val cp = System.getProperty("okay.cluster.cp")
    assume(cp != null, "the test classpath was not handed over (see build.sbt)")

    val procs = (0 until 4).map { _ =>
      val pb = ProcessBuilder("java", "-cp", cp, "okay.cluster.WorkerMain", "0",
        "okay.cluster.TestJobs$")
      pb.redirectErrorStream(true)
      pb.start()
    }
    try
      val ports = procs.map { pr =>
        val in = scala.io.Source.fromInputStream(pr.getInputStream)
        in.getLines().find(_.startsWith("worker listening"))
          .getOrElse(throw IllegalStateException("a worker never announced a port"))
          .split(' ')(2).toInt
      }
      val sockets = ports.toVector.map(p => Served.connect("127.0.0.1", p))

      // the kill happens DURING the run, at a chosen request: the
      // first worker's third request destroys its own process, so
      // every later request on that socket fails for real
      val n = AtomicInteger(0)
      val doomed: Cluster.Serve = req =>
        if n.incrementAndGet() == 3 then
          procs(0).destroyForcibly()
          procs(0).waitFor(): Unit
        sockets(0)(req)

      val got = Cluster.run(FanJob, feed, 8, doomed +: sockets.drop(1)).runWith
      assertEquals(got.value, healthy.value, "a killed process changed the answer")
      assertEquals(got.merged, healthy.merged)
      assert(got.retried > 0, "the killed worker was never noticed")
    finally procs.foreach(_.destroyForcibly(): Unit)
  }
}
