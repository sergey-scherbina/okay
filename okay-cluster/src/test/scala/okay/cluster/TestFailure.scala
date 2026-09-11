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

  /**
   * A worker that throws on its FIRST request and works afterwards —
   * a blip, not a death.
   */
  def blips(base: Cluster.Serve): Cluster.Serve =
    val first = java.util.concurrent.atomic.AtomicBoolean(true)
    req =>
      if first.getAndSet(false) then throw java.io.IOException("a blip")
      else base(req)

  test("A TRANSIENT BLIP ON EVERY WORKER, and the run survives it") {
    // THE SCHEDULE STAGE 5 COULD NOT SURVIVE. Its first seeded test
    // asked for exactly this and the run died: a `Serve` was buried
    // on its FIRST throw, so a hiccup that hit every worker left
    // nobody alive. It has been a named limit in three documents
    // since, and it is `dataflow-reconnect`'s first half: a worker is
    // buried after several CONSECUTIVE failures, and any answer
    // clears its count.
    val workers = Vector.fill(4)(blips(Cluster.local))
    val got = Cluster.run(FanJob, feed, 8, workers).runWith
    assertEquals(got.value, healthy.value, "a blip on every worker changed the answer")
    assertEquals(got.dropped, healthy.dropped)
    assertEquals(got.merged, healthy.merged)
    assert(got.failed >= 4, s"only ${got.failed} attempts were lost — the blips did not fire")
    assertEquals(got.retried, 0L,
      "a worker that hiccupped once and then answered was buried anyway")
  }

  test("a blip is forgiven, and forgetting is what makes it a blip") {
    // the count is CONSECUTIVE: a worker that fails, answers, fails
    // again is at one failure, not two. Without the reset a long
    // stream would bury every worker it ever hiccupped on, which is
    // the same limit in slow motion.
    val n = AtomicInteger(0)
    val flaky: Cluster.Serve = req =>
      // every third request throws: six failures over the run, never
      // two in a row
      if n.incrementAndGet() % 3 == 0 then throw java.io.IOException("again")
      else Cluster.local(req)
    val got = Cluster.run(FanJob, feed, 8, Vector(flaky, Cluster.local)).runWith
    assertEquals(got.value, healthy.value)
    assert(got.failed > 0, "the flake never fired")
    assertEquals(got.retried, 0L, "a worker that never failed twice in a row was buried")
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

  /**
   * A SERVER THAT SERVES ONE REQUEST PER CONNECTION AND HANGS UP.
   *
   * The cheapest honest model of a worker that restarts: the port
   * stays reachable, and the socket you were holding does not. It
   * speaks the framed protocol by hand rather than through
   * `Served.handle`, because `handle` loops until EOF and the point
   * here is that it does not.
   */
  def hangingUp(server: java.net.ServerSocket): Thread =
    Thread.ofVirtual().start { () =>
      try
        while !server.isClosed do
          val sock = server.accept()
          val in = java.io.DataInputStream(sock.getInputStream)
          val out = java.io.DataOutputStream(sock.getOutputStream)
          try
            val n = in.readInt()
            val bytes = new Array[Byte](n)
            in.readFully(bytes)
            val answer = okay.codec.Codecs.cbor(Req.given_Schema_Req).decode(bytes) match
              case Right(req) => Cluster.local(req)
              case Left(why) => Resp.Failed(why)
            val reply = okay.codec.Codecs.cbor(Resp.given_Schema_Resp).encode(answer)
            out.writeInt(reply.length)
            out.write(reply)
            out.flush()
          finally sock.close()      // one request, then the connection is gone
      catch case _: java.net.SocketException => ()
    }

  test("A CONNECTION THAT BREAKS EVERY TIME: `connect` dies, `reconnecting` heals") {
    // tolerance is enough for a worker that HICCUPS and cannot be
    // enough for a socket, because a broken one is broken for ever —
    // which is why `dataflow-reconnect` had two halves rather than
    // one. This is the second: the same server, the same job, and the
    // only difference is which `Serve` the coordinator was handed.
    val server = java.net.ServerSocket(0)
    val serving = hangingUp(server)
    try
      val port = server.getLocalPort
      val once = Vector.fill(2)(Served.connect("127.0.0.1", port))
      val e = intercept[IllegalStateException](Cluster.run(FanJob, feed, 8, once).runWith)
      assert(e.getMessage.contains("no workers left"), e.getMessage)

      val healing = Vector.fill(2)(Served.reconnecting("127.0.0.1", port))
      val got = Cluster.run(FanJob, feed, 8, healing).runWith
      assertEquals(got.value, healthy.value, "a healing connection changed the answer")
      assertEquals(got.merged, healthy.merged)
      assertEquals(got.retried, 0L, "a connection that healed was buried anyway")
    finally
      server.close()
      serving.join()
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
      // `lost`, not `retried`: since dataflow-reconnect a worker is
      // buried after several CONSECUTIVE failures, so one killed
      // process in a short run may never be buried at all — every
      // partition it held simply moves to a survivor. What this test
      // is about is that the death was NOTICED and paid for, and the
      // attempts counter is what says so.
      assert(got.failed > 0, "the killed worker was never noticed")
    finally procs.foreach(_.destroyForcibly(): Unit)
  }
}
