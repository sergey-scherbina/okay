package okay.spark

import java.nio.file.Files
import scala.util.Random
import okay.{Bulk, Chunks}
import okay.bayes.{Bayes, Smooth, Summary, Support, DarkWorlds}
import okay.spark.SparkBulk.Rows
import org.apache.spark.sql.SparkSession

/**
 * specs/okay-bayes.md stage 6, on Spark (okay-bayes-spark): the Dark Worlds
 * model of okay-bayes, written over any Bulk[D], observed over an RDD — its
 * likelihood and AD gradient aggregated on the executors, the aggregator
 * shipped to them by serialisation — against the same model over Chunks
 * and against the exact grid.
 */
class TestSparkBayes extends munit.FunSuite:
  import DarkWorlds.*
  override def munitTimeout = scala.concurrent.duration.Duration(10, "min")

  lazy val spark = SparkSession.builder().master("local[4]").appName("bayes").config("spark.ui.enabled", "false").getOrCreate()
  override def afterAll(): Unit = spark.stop()

  def report(line: String): Unit = println(s"  okay-bayes | $line")

  /** Spark reads files, not the classpath: the sky copied out of okay-bayes's test resources */
  lazy val dir: String =
    val d = Files.createTempDirectory("okay-bayes-darkworlds")
    val in = getClass.getResourceAsStream("/bmh/darkworlds/Training_Sky3.csv")
    try Files.copy(in, d.resolve("Training_Sky3.csv")) finally in.close()
    d.toString

  lazy val sparkBulk: Bulk[Rows] = SparkBulk(spark)
  lazy val local: Bulk[Chunks] = Bulk.local(path => scala.io.Source.fromFile(path).getLines())
  lazy val onSpark: Rows[Galaxy] = galaxies[Rows](3, dir)(using sparkBulk)
  lazy val onChunks: Chunks[Galaxy] = galaxies[Chunks](3, dir)(using local)

  val points = Seq((2324.0, 1123.0, 145.0), (100.0, 4000.0, 50.0), (3000.0, 2000.0, 170.0))
  def unconstrained(x: Double, y: Double, m: Double): Array[Double] =
    Array(Support.Interval(40, 180).unconstrain(m), Support.Interval(0, 4200).unconstrain(x), Support.Interval(0, 4200).unconstrain(y))

  test("the same model over Spark and over Chunks: the same log density and AD gradient, both forms") {
    assertEquals(count(onSpark)(using sparkBulk), 578L)
    val (ts, tc) = (Smooth.target(haloAd(onSpark)(using sparkBulk)), Smooth.target(haloAd(onChunks)(using local)))
    for (x, y, m) <- points do
      val u = unconstrained(x, y, m)
      val ((ls, gs), (lc, gc)) = (ts.gradient(u), tc.gradient(u))
      assertEqualsDouble(ls, lc, 1e-9 * math.abs(lc))
      for i <- 0 until 3 do assertEqualsDouble(gs(i), gc(i), 1e-9 * math.max(1, math.abs(gc(i))), s"d/du$i at ($x, $y, $m)")
      val ms = Bayes.prior(Bayes.observeBulk(onSpark)(galaxyLogLik(_, x, y, m))(using sparkBulk), Random(0)).logLik
      val mc = Bayes.prior(Bayes.observeBulk(onChunks)(galaxyLogLik(_, x, y, m))(using local), Random(0)).logLik
      assertEqualsDouble(ms, mc, 1e-9 * math.abs(mc))
    report(f"Dark Worlds over Spark (local[4]): log density and gradient equal to Chunks' at ${points.length} points")
  }

  test("NUTS over a Spark RDD: every gradient one aggregate on the executors, the halo where the exact grid puts it") {
    given Bulk[Rows] = sparkBulk
    val g = grid(onChunks)(using local)
    val (sx, sy, sm) = start(onSpark)
    val t0 = System.nanoTime()
    val post = Smooth.nuts(haloAd(onSpark), samples = 200, burn = 150, init = Map("x" -> sx, "y" -> sy, "mass" -> sm))
    val secs = (System.nanoTime() - t0) / 1e9
    val (xs, ys) = (post.site("x"), post.site("y"))
    report(f"NUTS over Spark: x ${Summary.mean(xs)}%.1f, y ${Summary.mean(ys)}%.1f (grid ${g.x}%.1f ± ${g.sdX}%.1f, ${g.y}%.1f ± ${g.sdY}%.1f), ESS(x) ${Summary.ess(xs)}%.0f of 200, $secs%.1f s")
    assert(math.abs(Summary.mean(xs) - g.x) < 4 * g.sdX / math.sqrt(Summary.ess(xs)))
    assert(math.abs(Summary.mean(ys) - g.y) < 4 * g.sdY / math.sqrt(Summary.ess(ys)))
  }
