package okay.flink.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}
import java.io.File
import java.nio.file.{Files, Path}

/**
 * FLINK ACROSS PROCESSES (docs/benchmarks.md §20,
 * bench-across-processes).
 *
 * §20's Flink rows are a MiniCluster: one JVM, no serialization
 * between tasks that a real deployment would pay, no cluster to come
 * up. That was the honest thing to measure while okay had no
 * distributed lane, and it stopped being the honest thing when okay
 * got one. This starts a real standalone cluster — a JobManager and
 * two TaskManagers, each its own JVM — and submits the same job to
 * it.
 *
 * WHAT IT TAKES, because it is not obvious and cost this lane an
 * evening: the entry points are on the test classpath already
 * (`StandaloneSessionClusterEntrypoint`, `TaskManagerRunner`), so no
 * distribution is needed — but a TaskManager launched directly must
 * be TOLD its resources, since `taskmanager.sh` normally derives them
 * with BashJavaUtils, and it fails with "The required configuration
 * option taskmanager.cpu.cores is not set" if it is not.
 *
 * And the SOURCE cannot be the one §20's MiniCluster rows use: it
 * replays out of a static array in the client's JVM, and a
 * TaskManager's copy of that static is empty. `FlinkLane`'s remote
 * road derives the feed where it is read, from the one number that
 * travels — which is what okay's workers have always done, and what
 * makes the two lanes comparable at all rather than one shipping its
 * input.
 */
class FlinkClusterBench extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present || cp == null
  override def munitTimeout = scala.concurrent.duration.Duration(30, "min")

  /**
   * The classpath the cluster's processes get — the same one this test
   * runs on, so driver and cluster share a build and a stdlib. Read
   * from a resource the build writes, because a test inside sbt
   * cannot read its own classpath and this module does not fork.
   */
  lazy val cp: String =
    val in = getClass.getResourceAsStream("/okay-flink-cp.txt")
    if in == null then null
    else try String(in.readAllBytes, "UTF-8") finally in.close()

  val Slots = 4
  val Managers = 2
  val Parallelism = 8
  val Rounds = 3
  val engine = s"flink 1.20, standalone, ${1 + Managers} processes"

  def config(dir: Path): Path =
    val f = dir.resolve("config.yaml")
    Files.writeString(f, s"""
jobmanager:
  rpc: { address: localhost, port: 6123 }
  bind-host: localhost
  memory: { process: { size: 1600m } }
rest: { address: localhost, bind-address: localhost, port: 8081 }
blob: { server: { port: 6124 } }
parallelism: { default: 1 }
taskmanager:
  bind-host: localhost
  host: localhost
  numberOfTaskSlots: $Slots
  cpu: { cores: 4.0 }
  memory:
    task: { heap: { size: 1024m }, off-heap: { size: 64m } }
    managed: { size: 256m }
    network: { min: 64m, max: 64m }
    framework: { heap: { size: 128m }, off-heap: { size: 128m } }
    jvm-metaspace: { size: 256m }
    jvm-overhead: { min: 192m, max: 192m }
""")
    f

  def spawn(main: String, conf: Path): Process =
    val pb = ProcessBuilder("java", "-Xmx1600m",
      s"-Dokay.wroclaw.gtfs=${Gtfs.dir.getAbsolutePath}",
      "-cp", cp, main, "--configDir", conf.getParent.toString)
    pb.redirectErrorStream(true)
    pb.redirectOutput(File("/dev/null"))
    pb.start()

  def ready(seconds: Int): Boolean =
    var left = seconds * 2
    while left > 0 do
      try
        val c = java.net.URI.create("http://localhost:8081/overview").toURL.openConnection()
        c.setConnectTimeout(500); c.setReadTimeout(500)
        val body = String(c.getInputStream.readAllBytes, "UTF-8")
        if body.contains(s""""taskmanagers":$Managers""") then return true
      catch case _: Throwable => ()
      Thread.sleep(500)
      left -= 1
    false


  /** the same least-squares split §20 applies to every engine: ms =
   * fixed + n / rate, so a cluster's startup is not charged to the
   * events */
  def report(name: String, points: Vector[(Long, Long)]): Unit =
    val k = points.length.toDouble
    val sx = points.map(_._1.toDouble).sum
    val sy = points.map(_._2.toDouble).sum
    val sxx = points.map(p => p._1.toDouble * p._1.toDouble).sum
    val sxy = points.map(p => p._1.toDouble * p._2.toDouble).sum
    val slope = (k * sxy - sx * sy) / (k * sxx - sx * sx)
    val fixed = (sy - slope * sx) / k
    val rate = if slope <= 0 then 0.0 else 1000.0 / slope
    println(f"%n  $name%-44s | $fixed%7.0f ms fixed | $rate%,10.0f ev/s marginal%n")

  test("the same job on a real Flink cluster, and the same eleven checksums") {
    val dir = Files.createTempDirectory("okay-flink-cluster")
    val conf = config(dir)
    val procs = scala.collection.mutable.ArrayBuffer.empty[Process]
    try
      procs += spawn("org.apache.flink.runtime.entrypoint.StandaloneSessionClusterEntrypoint", conf)
      Thread.sleep(8000)
      for _ <- 0 until Managers do
        procs += spawn("org.apache.flink.runtime.taskexecutor.TaskManagerRunner", conf)
      assert(ready(60), s"the cluster never reached $Managers task managers")

      println(f"%n  Flink 1.20 standalone: 1 JobManager + $Managers TaskManagers, " +
        f"$Slots slots each, parallelism $Parallelism%n")
      val points = scala.collection.mutable.ArrayBuffer.empty[(Long, Long)]
      println("  days |     events |    ms | (worst)")
      println("  -----|------------|-------|--------")
      for days <- Vector(2, 4, 8) do
        val feed = Gtfs.events(days)
        val expected = OkayLane.run(feed)
        // warm: every process parses the feed once before anything is timed
        val warm = FlinkLane.run(feed, Parallelism, remote = Some(("localhost", 8081)), days = days)
        assertEquals(warm, expected, s"$days days: the cluster computed something else")
        var lo = Long.MaxValue
        var hi = 0L
        for _ <- 0 until Rounds do
          val t0 = System.nanoTime()
          val got = FlinkLane.run(feed, Parallelism, remote = Some(("localhost", 8081)), days = days)
          val ms = (System.nanoTime() - t0) / 1000000L
          assertEquals(got, expected, s"$days days")
          lo = math.min(lo, ms); hi = math.max(hi, ms)
        println(f"  $days%4d | ${feed.events.length}%,10d | $lo%,5d | $hi%,7d")
        points += ((feed.events.length.toLong, lo))
      report(engine, points.toVector)
    finally procs.foreach(_.destroyForcibly(): Unit)
  }
