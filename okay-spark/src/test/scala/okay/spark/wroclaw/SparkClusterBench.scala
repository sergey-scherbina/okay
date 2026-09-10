package okay.spark.wroclaw

import okay.wroclaw.{Gtfs, OkayLane}
import java.io.File
import java.nio.file.{Files, Path}

/**
 * SPARK ACROSS PROCESSES (docs/benchmarks.md §20,
 * bench-across-processes).
 *
 * §20's Spark rows are `local[4]`: one JVM, no executors, no cluster.
 * This starts a real standalone cluster — a Master and two Workers,
 * each its own JVM, each Worker launching executor JVMs of its own —
 * and submits the same job to it.
 *
 * THREE THINGS IT TAKES, none of them obvious, all of them found by
 * the failure they cause:
 *
 *   - the DRIVER must come out of the same build as the cluster. Run
 *     from anywhere else it serialises a `scala.collection.immutable.
 *     ArraySeq` from a different scala-library and the Master answers
 *     `InvalidClassException: local class incompatible`. That is the
 *     two-stdlib arrangement okay-spark already documents, met from
 *     the other side.
 *   - a Worker builds its executors' launch command through
 *     SPARK_HOME, and without one it fails with "Cannot find any
 *     build directories". There is no distribution here, so the
 *     harness FABRICATES the layout: a directory of symlinks to the
 *     jars already on the classpath, which is what a distribution is.
 *   - an executor starts in a directory under the Worker's work dir
 *     and cannot resolve a relative path into this repository, so it
 *     is TOLD where the feed is. `Gtfs.dir` reads
 *     `okay.wroclaw.gtfs` / `OKAY_WROCLAW_GTFS` for exactly this, and
 *     a real deployment answers the question the same way.
 *
 * And the feed is DERIVED on the executor rather than shipped from
 * the driver — `SparkLane`'s remote road — because `parallelize` of a
 * driver array would put 2.4 million events through the wire that
 * neither of the other two lanes sends, and the row would be
 * measuring that.
 */
class SparkClusterBench extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present || cp == null
  override def munitTimeout = scala.concurrent.duration.Duration(30, "min")

  def cp: String = System.getProperty("okay.spark.cp")

  val Workers = 2
  val Cores = 4
  val Rounds = 3
  val engine = s"spark 4.0, standalone, ${1 + Workers} processes + executors"
  val Master = "spark://127.0.0.1:7077"

  /** what a distribution is, made out of the classpath we already have */
  def sparkHome(dir: Path): Path =
    val jars = dir.resolve("jars")
    Files.createDirectories(jars)
    for e <- cp.split(File.pathSeparator) if e.endsWith(".jar") do
      val target = jars.resolve(File(e).getName)
      if !Files.exists(target) then Files.createSymbolicLink(target, File(e).toPath): Unit
    dir

  def spawn(main: String, args: Seq[String], home: Path, log: File): Process =
    val pb = ProcessBuilder((Seq("java", "-Xmx1g", "-cp", cp, main) ++ args)*)
    pb.environment.put("SPARK_HOME", home.toString)
    pb.environment.put("SPARK_SCALA_VERSION", "2.13")
    pb.redirectErrorStream(true)
    pb.redirectOutput(log)
    pb.start()

  def listening(port: Int, seconds: Int): Boolean =
    var left = seconds * 2
    while left > 0 do
      try { val s = java.net.Socket("127.0.0.1", port); s.close(); return true }
      catch case _: Throwable => Thread.sleep(500)
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

  test("the same job on a real Spark cluster, and the same eleven checksums") {
    val dir = Files.createTempDirectory("okay-spark-cluster")
    val home = sparkHome(dir)
    val work = dir.resolve("work")
    Files.createDirectories(work)
    val procs = scala.collection.mutable.ArrayBuffer.empty[Process]
    try
      procs += spawn("org.apache.spark.deploy.master.Master",
        Seq("--host", "127.0.0.1", "--port", "7077", "--webui-port", "8085"),
        home, File("/dev/null"))
      assert(listening(7077, 60), "the master never bound its port")
      for i <- 0 until Workers do
        procs += spawn("org.apache.spark.deploy.worker.Worker",
          Seq(Master, "--cores", Cores.toString, "--memory", "2g", "--host", "127.0.0.1",
            "--webui-port", (8086 + i).toString, "--work-dir", work.resolve(s"w$i").toString),
          home, File("/dev/null"))
      Thread.sleep(8000)

      println(f"%n  Spark 4.0 standalone: 1 Master + $Workers Workers, $Cores cores each%n")
      val points = scala.collection.mutable.ArrayBuffer.empty[(Long, Long)]
      println("  days |     events |    ms | (worst)")
      println("  -----|------------|-------|--------")
      for days <- Vector(2, 4, 8) do
        val feed = Gtfs.events(days)
        val expected = OkayLane.run(feed)
        val warm = SparkLane.run(feed, Cores, master = Some(Master), days = days, executorCp = cp)
        assertEquals(warm, expected, s"$days days: the cluster computed something else")
        var lo = Long.MaxValue
        var hi = 0L
        for _ <- 0 until Rounds do
          val t0 = System.nanoTime()
          val got = SparkLane.run(feed, Cores, master = Some(Master), days = days, executorCp = cp)
          val ms = (System.nanoTime() - t0) / 1000000L
          assertEquals(got, expected, s"$days days")
          lo = math.min(lo, ms); hi = math.max(hi, ms)
        println(f"  $days%4d | ${feed.events.length}%,10d | $lo%,5d | $hi%,7d")
        points += ((feed.events.length.toLong, lo))
      report(engine, points.toVector)
    finally procs.foreach(_.destroyForcibly(): Unit)
  }
