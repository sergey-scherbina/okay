package okay.wroclaw

import okay.cluster.{Cluster, Flows, Resp, Served}
import okay.given
import java.util.concurrent.atomic.AtomicLong

/**
 * WHAT THE DISTRIBUTED ROAD COSTS (specs/dataflow.md, stage 7).
 *
 * Every number the engine has produced so far was measured inside one
 * JVM: stage 3 priced the plan against §20's hand-written lane (1.14x
 * of the eight-thread one), and stages 4 to 6 proved the same answer
 * came back across processes without ever asking what that cost. This
 * asks — one job, four roads, on one machine:
 *
 *   1. eight fibres in this JVM (`Flows.fan`), the reference;
 *   2. eight partitions through the COORDINATOR, workers in this JVM
 *      (`Cluster.local`) — the protocol and the CBOR, no socket;
 *   3. the same over sockets, still one JVM — the transport, with the
 *      operating system in it but no second heap;
 *   4. eight partitions over FOUR REAL OS PROCESSES — everything.
 *
 * Reading them in that order says where the money goes, which one
 * number could not.
 *
 * WHAT IS NOT COMPARED HERE, and it belongs in the section rather
 * than a footnote: this is not okay-on-a-cluster against
 * Flink-on-a-cluster. §20's Flink is a MiniCluster and its Spark is
 * `local[4]` — both one JVM — and a cluster number for either would
 * need a cluster. What these rows compare is our own in-process road
 * against our own distributed road on the same box.
 */
class MeasureWroclawCluster extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present
  override def munitTimeout = scala.concurrent.duration.Duration(30, "min")

  /** the in-JVM workers answer out of the same registry a worker
   * PROCESS builds from its command line — so this has to run here
   * too, and the two roads differ by the transport and nothing else */
  WroclawJobs.install()

  /** four service days, as MeasureWroclawFlow uses: a lane is then
   * hundreds of milliseconds rather than tens, and a lane that swings
   * a quarter cannot price a tenth */
  val Days4: Days = Days(4)
  val Parts = 8
  val Procs = 4
  val Rounds = 5
  val Warmup = 2

  lazy val feed: Feed = Distributed.feed(Days4.days)._1
  lazy val expected: Job.Result = OkayLane.run(feed)

  /** the test classpath, written down by build.sbt — a test inside
   * sbt cannot read its own */
  lazy val classpath: String =
    val in = getClass.getResourceAsStream("/okay-cluster-cp.txt")
    assert(in != null, "the classpath resource is missing (see build.sbt, compare)")
    try String(in.readAllBytes(), "UTF-8") finally in.close()

  /** what crosses, counted rather than described: the partials, in
   * bytes, and how many requests carried them */
  final class Wire:
    val bytes = AtomicLong(0)
    val calls = AtomicLong(0)
    def weigh(s: Cluster.Serve): Cluster.Serve = req =>
      val r = s(req)
      calls.incrementAndGet(): Unit
      r match
        case Resp.Partial(b) => bytes.addAndGet(b.length.toLong): Unit
        case _ => ()
      r

  def inProcess(): Job.Result =
    Distributed.assemble(Flows.fan(WroclawJob.flow(Days4, Parts), WroclawJob.sink(Days4)).runWith.value)

  def through(workers: Vector[Cluster.Serve]): Job.Result =
    Distributed.assemble(Cluster.run(WroclawJob, Days4, Parts, workers).runWith.value)

  final case class Lane(name: String, run: () => Job.Result)

  /** every lane once per ROUND, so the machine's drift is shared, and
   * the minimum reported with the worst beside it */
  def interleaved(lanes: Vector[Lane]): Vector[(String, Long, Long)] =
    for _ <- 0 until Warmup do
      for l <- lanes do assertEquals(l.run(), expected, s"${l.name} computes something else")
    val lo = Array.fill(lanes.length)(Long.MaxValue)
    val hi = Array.fill(lanes.length)(0L)
    for _ <- 0 until Rounds do
      System.gc()
      for (l, i) <- lanes.zipWithIndex do
        val t0 = System.nanoTime()
        val got = l.run()
        val ms = (System.nanoTime() - t0) / 1000000L
        assertEquals(got, expected, s"${l.name} computes something else")
        if ms < lo(i) then lo(i) = ms
        if ms > hi(i) then hi(i) = ms
    lanes.indices.toVector.map(i => (lanes(i).name, lo(i), hi(i)))

  /**
   * Four worker PROCESSES, and the ports they announce.
   *
   * Each parses the GTFS feed on its first request — seconds of the
   * BENCHMARK'S data, not the engine's work — so the warm-up rounds
   * pay for it and no timed row does.
   */
  def processes(n: Int): (Vector[Process], Vector[Int]) =
    val procs = (0 until n).map { _ =>
      val pb = ProcessBuilder("java", "-Xmx2g", "-cp", classpath,
        "okay.cluster.WorkerMain", "0", "okay.wroclaw.WroclawJobs$")
      pb.redirectErrorStream(true)
      pb.start()
    }.toVector
    val ports = procs.map { pr =>
      val in = scala.io.Source.fromInputStream(pr.getInputStream)
      in.getLines().find(_.startsWith("worker listening"))
        .getOrElse(throw IllegalStateException("a worker never announced a port"))
        .split(' ')(2).toInt
    }
    (procs, ports)

  test("the same job on four roads: in this JVM, through the coordinator, over sockets, over processes") {
    val n = feed.events.length

    // ONE CONNECTION PER PARTITION. A `Served` socket serves one
    // request at a time (it is synchronized, deliberately), so eight
    // partitions over four sockets would run two at a time and the
    // row would measure that choice rather than the transport. Eight
    // connections spread over four processes is the fair shape: each
    // process serves two partitions on two virtual threads.
    val server = java.net.ServerSocket(0)
    val serving = Thread.ofVirtual().start(() => Served.serve(server, Cluster.local))
    val (procs, ports) = processes(Procs)
    try
      val sockets = Vector.tabulate(Parts)(_ => Served.connect("127.0.0.1", server.getLocalPort))
      val remote = Vector.tabulate(Parts)(i => Served.connect("127.0.0.1", ports(i % Procs)))

      val rows = interleaved(Vector(
        Lane(s"$Parts fibres, one JVM (Flows.fan)", () => inProcess()),
        Lane(s"$Parts partitions, coordinator, workers in this JVM", () =>
          through(Vector.fill(Parts)(Cluster.local))),
        Lane(s"$Parts partitions, over sockets, one JVM", () => through(sockets)),
        Lane(s"$Parts partitions, over $Procs OS PROCESSES", () => through(remote)),
      ))
      val floor = rows.map(_._2).min
      println(f"%n  ${n}%,d events, four service days, best of $Rounds interleaved rounds%n")
      println("  road                                              |    ms | (worst) |    ev/s | vs best")
      println("  --------------------------------------------------|-------|---------|---------|--------")
      for (name, ms, worst) <- rows do
        val evs = if ms == 0 then 0L else n.toLong * 1000L / ms
        println(f"  $name%-49s | $ms%,5d | $worst%,7d | $evs%,7d | ${ms.toDouble / floor}%.2fx")
      println()
    finally
      procs.foreach(_.destroyForcibly(): Unit)
      server.close()
      serving.join()
  }

  /** the partials themselves, kept so their encoding can be timed —
   * a weigher that remembers instead of only counting */
  final class Kept:
    private val got = scala.collection.mutable.ArrayBuffer.empty[Array[Byte]]
    def keep(s: Cluster.Serve): Cluster.Serve = req =>
      val r = s(req)
      r match
        case Resp.Partial(b) => synchronized { got += b; () }
        case _ => ()
      r
    def bytes: Vector[Array[Byte]] = synchronized(got.toVector)

  def weighed[R](job: okay.cluster.Job[Days, R]): (Long, Long, Long) =
    val w = Wire()
    val run = Cluster.run(job, Days4, Parts, Vector.tabulate(Parts)(_ => w.weigh(Cluster.local))).runWith
    (run.merged, w.bytes.get, w.calls.get)

  test("what actually crosses the wire, weighed — the whole job and each stage") {
    val (merged, bytes, calls) = weighed(WroclawJob)
    val panes = expected.routeWins + expected.stopWins
    val n = feed.events.length
    println(f"%n  ${n}%,d events, $Parts partitions, the whole job%n")
    println(f"  panes the job produces                        | ${panes}%,12d")
    println(f"  accumulators reaching the coordinator         | ${merged}%,12d")
    println(f"  requests the coordinator made                 | ${calls}%,12d")
    println(f"  BYTES of partials that crossed                | ${bytes}%,12d")
    println(f"  bytes per event                               | ${bytes.toDouble / n}%12.3f")
    println(f"  bytes per pane                                | ${bytes.toDouble / panes}%12.3f")

    // and WHAT it is made of: the same flow, one stage at a time
    val stages = Vector(
      ("stage 2 — tumbling per route (138 keys)", weighed(WroclawRouteJob)),
      ("stage 3 — sliding per stop (2 482 keys x 3)", weighed(WroclawStopJob)),
      ("stage 4 — keyed state, no window (Claim 2)", weighed(WroclawBunchJob)),
    )
    println(f"%n  stage                                       | accumulators |       bytes | per event")
    println(f"  --------------------------------------------|--------------|-------------|----------")
    for (name, (m, b, _)) <- stages do
      println(f"  $name%-43s | $m%,12d | $b%,11d | ${b.toDouble / n}%9.3f")
    println()
  }

  /**
   * THE FIXED COST, SEPARATED FROM THE MARGINAL ONE — the same
   * least-squares split §20 already applies to Flink, on the same
   * three feed sizes (two, four and eight service days), so the rows
   * can be read beside each other.
   *
   * A distributed run pays something before it has seen an event: two
   * round trips, a plan built on every worker, sockets already open.
   * A benchmark over a fixed dataset charges that to the events, and
   * the smaller the dataset the worse it looks — which is a statement
   * about the benchmark, not about the engine, exactly as it is for
   * Flink.
   */
  test("the fixed cost of the distributed road, and the marginal one") {
    val sizes = Vector(2, 4, 8)
    val (procs, ports) = processes(Procs)
    try
      val remote = Vector.tabulate(Parts)(i => Served.connect("127.0.0.1", ports(i % Procs)))
      // warm every worker on every size first: a worker derives the
      // feed from its parameters, and that parse is the BENCHMARK'S
      // data rather than the engine's work
      for d <- sizes; _ <- 0 until Warmup do
        // `val _ =`, not `: Unit`: the ascription silences the
        // non-unit STATEMENT lint, not value discard
        val _ = Cluster.run(WroclawJob, Days(d), Parts, remote).runWith
        val _ = Flows.fan(WroclawJob.flow(Days(d), Parts), WroclawJob.sink(Days(d))).runWith

      def points(name: String, run: Days => Unit): (String, Vector[(Long, Long)]) =
        (name, sizes.map { d =>
          val n = Distributed.feed(d)._1.events.length.toLong
          var lo = Long.MaxValue
          for _ <- 0 until Rounds do
            System.gc()
            val t0 = System.nanoTime()
            run(Days(d))
            lo = math.min(lo, (System.nanoTime() - t0) / 1000000L)
          (n, lo)
        })

      val lanes = Vector(
        points(s"okay, $Parts fibres, one JVM", d =>
          { val _ = Flows.fan(WroclawJob.flow(d, Parts), WroclawJob.sink(d)).runWith }),
        points(s"okay, $Parts partitions over $Procs processes", d =>
          { val _ = Cluster.run(WroclawJob, d, Parts, remote).runWith }),
      )

      println(f"%n  lane                                        | fixed cost |     marginal | the points")
      println(f"  --------------------------------------------|------------|--------------|-----------")
      for (name, ps) <- lanes do
        // least squares on ms = fixed + n / rate
        val k = ps.length.toDouble
        val sx = ps.map(_._1.toDouble).sum
        val sy = ps.map(_._2.toDouble).sum
        val sxx = ps.map(p => p._1.toDouble * p._1.toDouble).sum
        val sxy = ps.map(p => p._1.toDouble * p._2.toDouble).sum
        val slope = (k * sxy - sx * sy) / (k * sxx - sx * sx)
        val fixed = (sy - slope * sx) / k
        val rate = if slope <= 0 then 0.0 else 1000.0 / slope
        val shown = ps.map((n, ms) => f"${n / 1000}%,dk:${ms}%,dms").mkString(" ")
        println(f"  $name%-43s | $fixed%7.0f ms | $rate%,10.0f ev/s | $shown")
      println()
    finally procs.foreach(_.destroyForcibly(): Unit)
  }

  test("how much of the distributed road is the CODEC") {
    // the partials of one whole run, kept — then decoded and encoded
    // again on their own, so the road's overhead can be split into
    // "the bytes" and "everything else the coordinator does"
    val k = Kept()
    val run = Cluster.run(WroclawJob, Days4, Parts,
      Vector.tabulate(Parts)(_ => k.keep(Cluster.local))).runWith
    assertEquals(Distributed.assemble(run.value), expected)
    val raw = k.bytes
    val s = WroclawJob.sink(Days4)
    val codec = okay.codec.Codecs.cbor(s.wire)

    def best(f: () => Unit): Long =
      for _ <- 0 until Warmup do f()
      var lo = Long.MaxValue
      for _ <- 0 until Rounds do
        System.gc()
        val t0 = System.nanoTime()
        f()
        lo = math.min(lo, (System.nanoTime() - t0) / 1000000L)
      lo

    val values = raw.map(b => codec.decode(b).fold(w => fail(w), identity))
    val dec = best(() => raw.foreach(b => codec.decode(b): Unit))
    val enc = best(() => values.foreach(v => codec.encode(v): Unit))
    val total = raw.map(_.length.toLong).sum
    println(f"%n  ${raw.length} partials, ${total}%,d bytes%n")
    println(f"  CBOR decode, all of them                      | $dec%,6d ms")
    println(f"  CBOR encode, all of them                      | $enc%,6d ms")
    println(f"  encode + decode                               | ${dec + enc}%,6d ms")
    println()
  }
