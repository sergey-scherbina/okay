package okay.spark.wroclaw

import okay.wroclaw.{Feed, Job}
import org.apache.spark.sql.expressions.Window as SqlWindow
import org.apache.spark.sql.functions.*
import org.apache.spark.sql.streaming.Trigger
import org.apache.spark.sql.types.*
import org.apache.spark.sql.{DataFrame, Row as SqlRow, SparkSession}
import java.nio.file.Files

/**
 * SPARK'S OWN EVENT-TIME ENGINE: Structured Streaming, with its own
 * window and its own watermark — `withWatermark("event", "30 seconds")`
 * and `window($"event", "5 minutes")`. This is the lane that is
 * like-for-like with Flink; the RDD lane beside it is Spark's BATCH
 * answer, where a window is only a key.
 *
 * THE SOURCE IS FILES, deliberately and said out loud. A streaming
 * query needs a streaming source, and the honest local ones are a file
 * source and Kafka. The feed is written to Parquet ONCE per JVM,
 * before anything is timed — it is the source's contents, the way a
 * topic's contents are not part of a job's cost — and the query
 * replays it with `Trigger.AvailableNow`, Spark's own "read everything
 * that is there, in micro-batches, then stop".
 *
 * TWO THINGS THE SHAPE REQUIRED, both of them worth knowing:
 *
 *   - **A sentinel row.** In append mode a window is emitted when the
 *     watermark passes its end, so at the end of a BOUNDED replay the
 *     last windows never come out — there is no later event to advance
 *     the watermark. The feed therefore carries one row far in the
 *     future with a route the table does not know; the `known` filter
 *     sits AFTER `withWatermark`, so the sentinel advances the
 *     watermark and never reaches an aggregation. That is the standard
 *     way to flush a bounded stream, and it keeps the lane in APPEND
 *     mode — which is the one that evicts. Complete mode would have
 *     answered too, by keeping the whole result table in state, which
 *     is exactly the property this benchmark is measuring elsewhere.
 *   - **The bunching stage is not streaming.** Its Structured
 *     Streaming shape is `flatMapGroupsWithState`, which needs typed
 *     Datasets and encoders for a POJO with no bean accessors; the
 *     lane uses Spark's idiomatic BATCH answer instead — `lag` over a
 *     window partitioned by (route, stop), ordered by the arrival
 *     index. The eleven checksums are the same either way, and this
 *     comment is the whole disclosure.
 */
object SparkStreamLane {

  private val schema = StructType(Seq(
    StructField("ts", LongType, nullable = false),
    StructField("route", IntegerType, nullable = false),
    StructField("stop", IntegerType, nullable = false),
    StructField("delay", IntegerType, nullable = false),
    StructField("idx", LongType, nullable = false)))

  def session(cores: Int): SparkSession =
    SparkSession.builder()
      .master(s"local[$cores]").appName("wroclaw-streaming")
      .config("spark.ui.enabled", "false")
      .config("spark.driver.host", "127.0.0.1")
      .config("spark.sql.shuffle.partitions", (cores * 2).toString)
      .config("spark.sql.session.timeZone", "UTC")
      .getOrCreate()

  /** the feed as Parquet, plus the sentinel that flushes the last
   * windows; written once and NOT timed */
  def stage(spark: SparkSession, feed: Feed): String =
    val dir = Files.createTempDirectory("wroclaw-parquet")
    dir.toFile.deleteOnExit()
    val rows = new java.util.ArrayList[SqlRow](feed.events.length + 1)
    var i = 0
    while i < feed.events.length do
      val d = feed.events(i)
      rows.add(SqlRow(d.ts, d.route, d.stop, d.delay, i.toLong))
      i += 1
    // BY RANGE, not by hash: each file must be a contiguous slice of
    // the ARRIVAL order. A shuffled write was the first cut and it
    // silently lost five events and two windows — every file then
    // spans the whole time range, so the first micro-batch pushes the
    // watermark to the end and everything after it is late. The lane's
    // equality assertion is what caught it.
    // ONE SLICE PER WRITE, IN ORDER, AND A SECOND BETWEEN THEM. The
    // file source hands files to micro-batches oldest-FIRST by
    // modification time, and a directory written in one job has eight
    // files with the same timestamp — so the batches came in an
    // arbitrary order, the first one pushed the watermark into the
    // middle of the day, and everything before it was late. Measured:
    // 17 199 windows of 22 543. Writing the slices one at a time, a
    // second apart, is what makes the replay a replay.
    val df = spark.createDataFrame(rows, schema)
    val slices = 8
    val step = (rows.size + slices - 1) / slices
    for k <- 0 until slices do
      df.filter(col("idx") >= lit(k.toLong * step) && col("idx") < lit((k + 1).toLong * step))
        .repartition(1)
        .write.mode(if k == 0 then "overwrite" else "append").parquet(dir.toString)
      Thread.sleep(1100)

    dir.toString

  /**
   * SPARK'S LATE RULE IS NOT FLINK'S, and the lane compensates rather
   * than pretending. Flink and `okay.Windows` drop an element only if
   * every WINDOW it belongs to has closed; Structured Streaming drops
   * the ROW as soon as its event time is below the watermark, even
   * when the window it would land in is still open. With the job's own
   * 30 s that costs a handful of rows at the micro-batch boundaries
   * (measured: five of 234 950, and two whole windows with them). A
   * five-minute delay is above anything the feed's 25 s jitter can
   * produce, so under Spark's rule it keeps exactly the rows the other
   * engines keep — and Spark pays for it in state held longer, which
   * is its own trade to make.
   */
  private val lateness: Long = 5 * 60 * 1000L

  def run(spark: SparkSession, path: String, feed: Feed): Job.Result = {
    val tram = feed.routes.iterator.map(_.tram).toArray
    val routes = tram.length

    // the watermark is taken BEFORE the filter, so the sentinel can do
    // its work; `event` is the event time as a timestamp (the feed's
    // times are whole seconds, so nothing is lost)
    def source: DataFrame =
      spark.readStream.schema(schema).option("maxFilesPerTrigger", 1).parquet(path)
        .withColumn("event", (col("ts") / 1000).cast(TimestampType))
        .withWatermark("event", s"${lateness / 1000} seconds")
        .filter(col("route") >= 0 && col("route") < lit(routes))

    // UPDATE mode, and the driver keeps the LAST value of each window.
    // Append mode would have been tidier — one row per window, at the
    // moment the watermark closes it — but at the end of a BOUNDED
    // replay its last batch's windows are never flushed: the watermark
    // update takes effect at the start of the next batch, and with
    // `Trigger.AvailableNow` there is no next batch. Two sentinel rows
    // in files of their own were tried and the file source's ordering
    // made them unreliable; update mode needs neither, at the price of
    // a map on the driver — which the section reports rather than
    // hides.
    val routeRows = scala.collection.mutable.HashMap.empty[(Long, Int), Job.Stats]
    val stopRows = scala.collection.mutable.HashMap.empty[(Long, Int), Job.Stats]

    // ---- stage 2: Spark's own TUMBLING window, per route
    source
      .groupBy(window(col("event"), s"${Job.WindowMs / 60000} minutes"), col("route"))
      .agg(count(lit(1)).as("n"), sum("delay").as("sum"), max("delay").as("mx"))
      .select(col("window.start").as("start"), col("route"), col("n"), col("sum"), col("mx"))
      .writeStream.outputMode("update").trigger(Trigger.AvailableNow())
      .foreachBatch { (batch: DataFrame, _: Long) =>
        batch.collect().foreach { r =>
          routeRows.update((r.getTimestamp(0).getTime, r.getInt(1)),
            Job.Stats(r.getLong(2), r.getLong(3), r.getInt(4)))
        }
      }
      .start().awaitTermination()

    // ---- stage 3: Spark's own SLIDING window, per stop
    source
      .groupBy(window(col("event"), s"${Job.SlideWindowMs / 60000} minutes",
        s"${Job.SlideMs / 60000} minutes"), col("stop"))
      .agg(count(lit(1)).as("n"), sum("delay").as("sum"), max("delay").as("mx"))
      .select(col("window.start").as("start"), col("stop"), col("n"), col("sum"), col("mx"))
      .writeStream.outputMode("update").trigger(Trigger.AvailableNow())
      .foreachBatch { (batch: DataFrame, _: Long) =>
        batch.collect().foreach { r =>
          stopRows.update((r.getTimestamp(0).getTime, r.getInt(1)),
            Job.Stats(r.getLong(2), r.getLong(3), r.getInt(4)))
        }
      }
      .start().awaitTermination()

    // ---- stage 4: the keyed state, as Spark's batch idiom for "the
    // previous row of this key": lag over (route, stop) by arrival
    val batch = spark.read.schema(schema).parquet(path)
      .filter(col("route") >= 0 && col("route") < lit(routes))
    val byPair = SqlWindow.partitionBy(col("route"), col("stop")).orderBy(col("idx"))
    val gaps = batch
      .withColumn("prev", lag(col("ts"), 1).over(byPair))
      .filter(col("prev").isNotNull)
      .withColumn("gap", abs(col("ts") - col("prev")))
      .filter(col("gap") < lit(Job.BunchMs))
      .agg(count(lit(1)).as("bunches"), sum("gap").as("gap"))
      .collect()
      .head
    val bunches = gaps.getLong(0)
    val bunchGap = if gaps.isNullAt(1) then 0L else gaps.getLong(1)

    // ---- the checksums, from the final value of every window
    var routeWins = 0L; var routeEvents = 0L; var routeDelay = 0L; var routeHash = 0L
    val ranked = scala.collection.mutable.LongMap.empty[List[(Int, Job.Stats)]]
    for ((start, route), s) <- routeRows do
      routeWins += 1; routeEvents += s.n; routeDelay += s.sum
      routeHash ^= Job.hash(start, route.toLong, s.n, s.sum, s.max.toLong)
      if tram(route) then
        ranked.update(start, Job.top.add(ranked.getOrElse(start, Job.top.init), (route, s)))
    var stopWins = 0L; var stopEvents = 0L; var stopHash = 0L
    for ((start, stop), s) <- stopRows do
      stopWins += 1; stopEvents += s.n
      stopHash ^= Job.hash(start, stop.toLong, s.n, s.sum, s.max.toLong)

    // ---- stage 5: the ranking, on the driver over the tram windows
    var topWins = 0L; var topHash = 0L
    ranked.foreachKey { start =>
      topWins += 1
      topHash ^= Job.topHash(start, Job.top.present(ranked(start)))
    }

    Job.Result(routeWins, routeEvents, routeDelay, routeHash,
      stopWins, stopEvents, stopHash, bunches, bunchGap, topWins, topHash)
  }
}
