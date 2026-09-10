package okay.spark.wroclaw

import okay.wroclaw.{Depart, Feed, Job, Ride}

import okay.Aggregator
import okay.spark.SparkInterop
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}
import scala.reflect.ClassTag

/**
 * THE FIFTH ENGINE: Spark, in local mode, over RDDs.
 *
 * The same aggregator again — `SparkInterop.aggregateByKey` hands an
 * okay `Aggregator` to Spark as the (zero, seqOp, combOp) triple, which
 * is the same value Flink takes through `toFlink` and the JDK through
 * `Collect.collector`. Five engines, one definition of the arithmetic.
 *
 * SPARK'S ANSWER TO THIS JOB IS A BATCH ONE, and the lane says so
 * rather than pretending otherwise: an RDD has no event time and no
 * watermark, so a window is a KEY — exactly the `groupingBy` shape of
 * the JDK lane, distributed. Structured Streaming is Spark's
 * event-time engine and would be the like-for-like answer to Flink;
 * that is a separate lane (BACKLOG `spark-structured-streaming-lane`),
 * not a variation on this one.
 *
 * AN RDD HAS NO ENCOUNTER ORDER, which the bunching stage needs. The
 * arrival INDEX is therefore carried through the shuffle
 * (`zipWithIndex` before the partitioning) and each key's group is
 * sorted by it — the cost of asking a shuffle-based engine for a
 * question about order.
 *
 * WHY spark-core AND NOT spark-sql: okay-spark's own tests need a
 * two-stdlib classpath because `SparkSession`'s companion lookup goes
 * through Scala 2 runtime reflection, which cannot bootstrap against
 * the Scala 3.9 stdlib (see build.sbt, okay-spark). The RDD API needs
 * none of that — measured here: `new SparkContext` starts and runs on
 * this module's ordinary classpath — so the lane sits beside the
 * others instead of dragging a mixed stdlib onto them.
 */
object SparkLane {

  /** one context per run, as a job would have one per submission */
  private def context(cores: Int): SparkContext =
    new SparkContext(new SparkConf()
      .setMaster(s"local[$cores]").setAppName("wroclaw")
      .set("spark.ui.enabled", "false")
      .set("spark.driver.host", "127.0.0.1"))
      // NOT Kryo, and the reason is the same one that keeps
      // SparkSession out of this module: Spark's KryoSerializer
      // registers a serializer for `scala.Enumeration$Value` that
      // reflects for `scala$Enumeration$$outerEnum`, a Scala 2.13
      // accessor that the Scala 3.9 stdlib does not have. Measured
      // here — `NoSuchMethodException` on the first shuffle. Java
      // serialization has no such lookup and works, and okay-spark's
      // own doc already prices Kryo against it (18 s vs 4.3 s on the
      // Wrocław demo's persist), so this lane is paying that.

  /** what the checksum folds of stages 2 and 3 accumulate */
  private final case class Rows(wins: Long, events: Long, delay: Long, hash: Long)

  private def rowsOf(slide: Long): Aggregator[(Long, Job.Stats), Rows, Rows] =
    Aggregator[(Long, Job.Stats), Rows, Rows](Rows(0, 0, 0, 0))(
      (r, kv) =>
        val (id, s) = kv
        Rows(r.wins + 1, r.events + s.n, r.delay + s.sum,
          r.hash ^ Job.hash((id >>> 20) * slide, (id & 0xfffffL).toLong, s.n, s.sum, s.max.toLong)))(
      (a, b) => Rows(a.wins + b.wins, a.events + b.events, a.delay + b.delay, a.hash ^ b.hash))(identity)

  def run(feed: Feed, cores: Int = 4): Job.Result = {
    val sc = context(cores)
    try {
      val tram = feed.routes.iterator.map(_.tram).toArray
      val events: RDD[Depart] = sc.parallelize(feed.events.toIndexedSeq, cores * 2)

      // stage 1: the map-side join, the table shipped in the closure
      val rides: RDD[Ride] = events
        .filter(d => d.route >= 0 && d.route < tram.length)
        .map(d => new Ride(d.ts, d.route, d.stop, d.vehicle, d.delay, tram(d.route)))
        .cache()

      // stage 2: tumbling per route — a window is a KEY here.
      // `SparkInterop.aggregateByKey` collects to the driver, which is
      // right for this stage (201k windows) and wrong for the next one
      val byRoute: Map[Long, Job.Stats] = SparkInterop.aggregateByKey(
        rides.map(r => (((r.ts - Math.floorMod(r.ts, Job.WindowMs)) / Job.WindowMs << 20) | r.route.toLong, r)))(
        Job.stats)
      val routeRows = byRoute.foldLeft(Rows(0, 0, 0, 0))((r, kv) =>
        rowsOf(Job.WindowMs).add(r, kv))

      // stage 5 needs the tram windows themselves
      val trams = byRoute.filter(kv => tram((kv._1 & 0xfffffL).toInt))

      // stage 3: sliding per stop — three keys per element, expanded.
      // Millions of panes, so the shuffle's result stays DISTRIBUTED
      // and the checksum folds over it with the same aggregator
      // vocabulary rather than travelling to the driver
      val stopPairs = rides.flatMap { r =>
        val first = r.ts - Math.floorMod(r.ts, Job.SlideMs) - Job.SlideWindowMs + Job.SlideMs
        (0 until Job.Panes).map(i =>
          ((((first + i * Job.SlideMs) / Job.SlideMs) << 20) | r.stop.toLong, r))
      }
      val byStop: RDD[(Long, Job.Stats)] = RDD.rddToPairRDDFunctions(stopPairs)
        .aggregateByKey(Job.stats.init)(Job.stats.add, Job.stats.merge)
        .map((k, acc) => (k, Job.stats.present(acc)))
      val stopRows = SparkInterop.aggregate(byStop)(rowsOf(Job.SlideMs))

      // stage 4: the bunching detector. An RDD has no encounter order,
      // so the arrival index travels with the element and the group is
      // sorted by it before the scan
      val bunched = rides.zipWithIndex()
        .map((r, i) => ((r.route.toLong << 20) | r.stop.toLong, (i, r.ts)))
        .groupByKey()
        .map { (_, seen) =>
          var bunches = 0L; var gap = 0L; var prev = Long.MinValue
          for (_, ts) <- seen.toArray.sortBy(_._1) do
            if prev != Long.MinValue then
              val d = Math.abs(ts - prev)
              if d < Job.BunchMs then { bunches += 1; gap += d }
            prev = ts
          (bunches, gap)
        }
        .fold((0L, 0L))((a, b) => (a._1 + b._1, a._2 + b._2))

      // stage 5: the ranking, on the driver over the tram windows
      val ranked = scala.collection.mutable.LongMap.empty[List[(Int, Job.Stats)]]
      for (id, s) <- trams do
        val start = (id >>> 20) * Job.WindowMs
        ranked.update(start, Job.top.add(ranked.getOrElse(start, Job.top.init), ((id & 0xfffffL).toInt, s)))
      var topWins = 0L; var topHash = 0L
      ranked.foreachKey { start =>
        topWins += 1
        topHash ^= Job.topHash(start, Job.top.present(ranked(start)))
      }

      Job.Result(routeRows.wins, routeRows.events, routeRows.delay, routeRows.hash,
        stopRows.wins, stopRows.events, stopRows.hash,
        bunched._1, bunched._2, topWins, topHash)
    } finally sc.stop()
  }
}
