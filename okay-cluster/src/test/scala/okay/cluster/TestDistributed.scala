package okay.cluster

import okay.given
import java.net.ServerSocket

/**
 * THE ENGINE ACROSS PROCESSES (specs/dataflow.md, stage 4b).
 *
 * Three levels, in this order on purpose: in-process workers, then
 * sockets inside this JVM, then real operating-system processes. A
 * worker is `Req => Resp` either way, so a failure at level three is
 * a failure of the transport and never of the arithmetic — the
 * arithmetic was already pinned at level one.
 *
 * The bar at every level is the same and is not "close": the answer,
 * the drop count and the merged count must EQUAL what `Flows.fan`
 * computes in one process.
 */
class TestDistributed extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()

  val feed: Feed = Feed(20000, Late - 1)
  val late: Feed = Feed(20000, Late * 8)

  def local[R](job: Job[Feed, R], f: Feed, parts: Int): Run[R] =
    Flows.fan(job.flow(f, parts), job.sink(f)).runWith

  test("in-process workers: the protocol computes what the fan computes") {
    for parts <- Vector(1, 2, 4, 8); workers <- Vector(1, 2, 3) do
      val here = local(WindowJob, feed, parts)
      val there = Cluster.run(WindowJob, feed, parts,
        Vector.fill(workers)(Cluster.local)).runWith
      assertEquals(there.value, here.value, s"$parts partitions over $workers workers")
      assertEquals(there.dropped, here.dropped, s"$parts partitions over $workers workers")
      assertEquals(there.merged, here.merged, s"$parts partitions over $workers workers")
  }

  test("in-process workers: a fan of three sinks, at a distance") {
    for parts <- Vector(1, 4, 8) do
      val here = local(FanJob, feed, parts)
      val there = Cluster.run(FanJob, feed, parts, Vector.fill(3)(Cluster.local)).runWith
      assertEquals(there.value, here.value, s"$parts partitions")
      assertEquals(there.merged, here.merged, s"$parts partitions")
  }

  test("the late feed too: what a partition drops survives the distance") {
    val here = local(WindowJob, late, 8)
    assert(here.dropped > 0, "the late feed drops nothing — it asserts nothing")
    val there = Cluster.run(WindowJob, late, 8, Vector.fill(4)(Cluster.local)).runWith
    assertEquals(there.dropped, here.dropped)
    assertEquals(there.value, here.value)
  }

  test("a job this build does not know is an answer, not a crash") {
    object Stranger extends Job[Feed, Sum] {
      type A = Ev
      def name: String = "test.not-registered"
      def params: okay.codec.Schema[Feed] = summon[okay.codec.Schema[Feed]]
      def flow(f: Feed, parts: Int): Flow[Ev] = Flow.slices(events(f), parts)
      def sink(f: Feed): Wire[Ev, Sum] = WindowJob.sink(f)
    }
    val e = intercept[IllegalStateException](
      Cluster.run(Stranger, feed, 2, Vector(Cluster.local)).runWith)
    assert(e.getMessage.contains("no job named"), e.getMessage)
    assert(e.getMessage.contains("test.window"), s"it should say what it DOES know: ${e.getMessage}")
  }

  test("FOUR REAL PROCESSES: the answer does not depend on where a partition ran") {
    // the point of the whole stage. Four JVMs that share nothing but
    // an artifact and a job NAME; the coordinator here holds only
    // sockets. Everything the workers need to build the same plan
    // travels as `Feed`, described by its Schema — no closure, no
    // serialized lambda, no class shipped.
    val cp = System.getProperty("okay.cluster.cp")
    assume(cp != null, "the test classpath was not handed over (see build.sbt)")

    val procs = (0 until 4).map { _ =>
      val pb = ProcessBuilder("java", "-cp", cp, "okay.cluster.WorkerMain", "0",
        "okay.cluster.TestJobs$")
      pb.redirectErrorStream(true)
      pb.start()
    }
    try
      // each worker prints its port once it is bound — waiting for
      // that line beats sleeping and guessing
      val ports = procs.map { pr =>
        val in = scala.io.Source.fromInputStream(pr.getInputStream)
        val line = in.getLines().find(_.startsWith("worker listening"))
          .getOrElse(throw IllegalStateException("a worker never announced a port"))
        assert(line.contains("test.window"), s"the worker did not register the jobs: $line")
        line.split(' ')(2).toInt
      }
      val wire = ports.toVector.map(p => Served.connect("127.0.0.1", p))

      for parts <- Vector(4, 8) do
        val here = local(FanJob, feed, parts)
        val there = Cluster.run(FanJob, feed, parts, wire).runWith
        assertEquals(there.value, here.value, s"$parts partitions over 4 processes")
        assertEquals(there.dropped, here.dropped, s"$parts partitions over 4 processes")
        assertEquals(there.merged, here.merged, s"$parts partitions over 4 processes")

      // and the late feed, whose drop count only agrees if every
      // worker seeded its watermark from the coordinator's bounds
      val hereLate = local(WindowJob, late, 8)
      val thereLate = Cluster.run(WindowJob, late, 8, wire).runWith
      assert(hereLate.dropped > 0)
      assertEquals(thereLate.dropped, hereLate.dropped, "the seeding did not cross")
      assertEquals(thereLate.value, hereLate.value)
    finally procs.foreach(_.destroyForcibly(): Unit)
  }

  test("over sockets, in this JVM: real framing, real bytes") {
    val server = ServerSocket(0)
    val serving = Thread.ofVirtual().start(() => Served.serve(server, Cluster.local))
    try
      val wire = Vector.fill(3)(Served.connect("127.0.0.1", server.getLocalPort))
      for parts <- Vector(1, 4, 8) do
        val here = local(FanJob, feed, parts)
        val there = Cluster.run(FanJob, feed, parts, wire).runWith
        assertEquals(there.value, here.value, s"$parts partitions over sockets")
        assertEquals(there.dropped, here.dropped, s"$parts partitions over sockets")
        assertEquals(there.merged, here.merged, s"$parts partitions over sockets")
    finally
      server.close()
      serving.join(2000): Unit
  }
}
