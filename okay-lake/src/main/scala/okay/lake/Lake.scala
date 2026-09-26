package okay.lake

import okay.{!, Async, Chunks, given}
import okay.arrow.{Column, Rows, Table}
import okay.blob.{Blob, Bytes}
import okay.cluster.{Bounds, Flow, Wire}
import okay.codec.{Codecs, Schema}
import okay.parquet.{ParquetCodec, ReadAt}
import java.util.concurrent.atomic.AtomicLong

/**
 * OBJECT STORAGE AND PARQUET AS THE CLUSTER ENGINE'S SOURCE AND SINK
 * (specs/dataflow.md, stage 17).
 *
 * A LAKE is an okay-blob `Blob` — S3, MinIO, a directory — known to a
 * process by NAME (`Lakes.register`), because a job's parameters travel
 * to every worker and a credential must not travel with them: each
 * process is started knowing its own lakes, the job names one.
 */
object Lakes:
  private val known = scala.collection.mutable.LinkedHashMap.empty[String, Blob]
  def register(name: String, blob: Blob): Unit = synchronized(known.update(name, blob))
  def apply(name: String): Blob = synchronized(known.get(name)).getOrElse(
    throw IllegalStateException(s"no lake named '$name' in this process; it knows ${synchronized(known.keys.toVector)}"))

/** one partition of a plan: row group `group` of the object `key`, and
 * the values of a table's partition columns that the file does not hold
 * (a Delta table's `partitionValues`; `None` is a null) */
final case class Part(key: String, group: Int, rows: Long,
                      partition: Vector[(String, Option[String])] = Vector.empty)

/** the partitions of a source, planned once at submission; `columns`
 * types the partition values (`string`, `long`, `integer`, `short`,
 * `byte`, `double`, `float`, `boolean`, `date`, `timestamp`) */
final case class LakePlan(lake: String, parts: Vector[Part], columns: Vector[(String, String)] = Vector.empty)

/** one object a sink wrote: its key, rows and bytes */
final case class Written(key: String, rows: Long, bytes: Long)

/** what a run wrote, and what its commit made visible */
final case class Manifest(files: Vector[Written]):
  def rows: Long = files.map(_.rows).sum

  /**
   * THE RUN'S VISIBLE OUTPUT AS A DUCKDB TABLE (duckdb-lake-reads):
   * `read_parquet([...])` of exactly the manifest's objects under `root`
   * (`s3://bucket` with DuckDB's httpfs, or the directory a `Fs` lake
   * lives in). Never a glob: a glob over `_data/` reads what a racing or
   * lost writer left there, and the manifest is what the commit made
   * visible.
   */
  def duckdb(root: String): String =
    if files.isEmpty then throw IllegalStateException("an empty manifest names nothing for DuckDB to read")
    files.map(f => "'" + s"${root.stripSuffix("/")}/${f.key}".replace("'", "''") + "'").mkString("read_parquet([", ", ", "])")

object LakePlan:
  given Schema[Part] = Schema.derived
  given Schema[LakePlan] = Schema.derived

object Written:
  given Schema[Written] = Schema.derived

object Manifest:
  given Schema[Manifest] = Schema.derived
  /** where a run's manifest lives under its prefix */
  def keyOf(prefix: String): String = s"$prefix/_manifest.json"

  /** the manifest a run committed under `prefix`, if it has one */
  def of(lake: String, prefix: String): Option[Manifest] =
    Run(Lakes(lake).getBytes(keyOf(prefix))).toOption.map(json =>
      Codecs.readJson[Manifest](String(json, "UTF-8"))
        .fold(why => throw IllegalStateException(s"the manifest of '$prefix': $why"), identity))

/** blocking reads of a lake: a flow's partition runs on its own thread */
private[lake] object Run:
  def apply[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

/**
 * A PARQUET OBJECT AS A `ReadAt`: its size from the listing (or a HEAD),
 * each read one range GET. What the reader asks for is the footer, then
 * one row group's column chunks — so memory is a group, never the object.
 */
final class BlobReadAt(blob: Blob, key: String, val size: Long) extends ReadAt:
  def read(offset: Long, len: Int): Array[Byte] =
    ParquetSource.largest.accumulateAndGet(len.toLong, math.max): Unit
    Run(blob.getBytes(key, Some((offset, offset + len)))) match
      case Right(b) if b.length == len => b
      case Right(b) => throw IllegalStateException(s"'$key': asked $len bytes at $offset, got ${b.length}")
      case Left(why) => throw IllegalStateException(why)

object ParquetSource:
  /** the largest single read this process made of an input — how a test
   * sees that memory is one row group */
  val largest: AtomicLong = AtomicLong(0)

  /**
   * THE PLAN: one partition per ROW GROUP of every Parquet object under
   * `prefix` — or of the objects its MANIFEST names, when it has one, so
   * a run's output is read as exactly what its commit made visible.
   */
  def plan(lake: String, prefix: String)(using codec: ParquetCodec): LakePlan =
    val blob = Lakes(lake)
    val objects: Vector[(String, Long)] = Run(blob.getBytes(Manifest.keyOf(prefix))) match
      case Right(json) =>
        Codecs.readJson[Manifest](String(json, "UTF-8"))
          .fold(why => throw IllegalStateException(s"the manifest of '$prefix': $why"), m => m.files.map(w => (w.key, w.bytes)))
      case Left(_) =>
        Run(okay.Source.concat(blob.list(s"$prefix/"))).filter(_.key.endsWith(".parquet")).map(m => (m.key, m.size))
    LakePlan(lake, objects.sortBy(_._1).flatMap { (key, size) =>
      val f = codec.footer(BlobReadAt(blob, key, size))
      f.groups.zipWithIndex.map((rows, g) => Part(key, g, rows))
    })

  /**
   * THE FLOW: partition `i` is `plan.parts(i)`, its rows decoded as `A`
   * (okay-arrow's `Rows`, a Schema's fields by name). Opened at its
   * position: a replacement reads its group and drops the rows already
   * folded — a group is the unit a Parquet reader can seek to.
   */
  def flow[A](plan: LakePlan)(using s: Schema[A], codec: ParquetCodec): Flow[A] =
    require(plan.parts.nonEmpty, s"the plan of lake '${plan.lake}' has no row groups")
    Flow.opened(plan.parts.length) { (i, start, _) =>
      val part = plan.parts(i)
      val blob = Lakes(plan.lake)
      val size = Run(blob.head(part.key)).getOrElse(throw IllegalStateException(s"'${part.key}' is gone")).size
      val in = BlobReadAt(blob, part.key, size)
      val read = codec.group(in, codec.footer(in), part.group)
      val table = if part.partition.isEmpty then read else withPartition(read, part, plan.columns)
      val rows = Rows.rows[A](table).fold(why => throw IllegalStateException(s"'${part.key}' group ${part.group}: $why"), identity)
      Chunks.fromIterator(rows.iterator.drop(start.toInt), 1024)
    }

/** a group's table with its partition values added as constant columns,
 * typed by the plan (stage 18) */
private[lake] def withPartition(t: Table, part: Part, types: Vector[(String, String)]): Table =
  val n = t.rows
  val typeOf = types.toMap
  val added = part.partition.map { (name, v) =>
    val ok = Array.fill(n)(v.isDefined)
    val s = v.getOrElse("")
    def parse[X](f: String => X): X =
      try f(s) catch case e: Exception => throw IllegalStateException(s"partition value '$s' of '$name': ${e.getMessage}")
    val col = typeOf.getOrElse(name, "string") match
      case "string" => Column.Utf8(Array.fill(n)(s), ok)
      case "long" => Column.Int64(Array.fill(n)(if v.isDefined then parse(_.toLong) else 0L), ok)
      case "integer" => Column.Ints(32, true, Array.fill(n)(if v.isDefined then parse(_.toLong) else 0L), ok)
      case "short" => Column.Ints(16, true, Array.fill(n)(if v.isDefined then parse(_.toLong) else 0L), ok)
      case "byte" => Column.Ints(8, true, Array.fill(n)(if v.isDefined then parse(_.toLong) else 0L), ok)
      case "double" => Column.Float64(Array.fill(n)(if v.isDefined then parse(_.toDouble) else 0.0), ok)
      case "float" => Column.Float32(Array.fill(n)(if v.isDefined then parse(_.toFloat) else 0f), ok)
      case "boolean" => Column.Bool(Array.fill(n)(v.isDefined && parse(_.toBoolean)), ok)
      case "date" =>
        Column.Date32(Array.fill(n)(if v.isDefined then parse(java.time.LocalDate.parse(_).toEpochDay.toInt) else 0), ok)
      case "timestamp" =>
        val micros = if v.isEmpty then 0L else parse { x =>
          val i = java.time.LocalDateTime.parse(x.replace(' ', 'T')).toInstant(java.time.ZoneOffset.UTC)
          i.getEpochSecond * 1000000L + i.getNano / 1000 }
        Column.Timestamp(okay.arrow.TimeUnit.Micro, Some("UTC"), Array.fill(n)(micros), ok)
      case other => throw IllegalStateException(s"partition column '$name' of type $other: not read")
    name -> col
  }
  Table(t.cols ++ added, t.metadata)

/**
 * A BATCH JOB'S OUTPUT AS PARQUET OBJECTS (stage 17).
 *
 * Each partition's rows become ONE object under `prefix/_data/`: built
 * on local disk a row group (`groupRows`) at a time, and put whole at
 * the partition's end — a worker killed before then uploaded nothing.
 * The coordinator's state is the list of objects the partials name, and
 * the COMMIT (the batch run's one epoch) writes `prefix/_manifest.json`
 * naming them, then deletes whatever else is under `_data/` (a lost
 * reply's duplicate). A stream is refused by name: a partition's object
 * is only whole at its end.
 */
object ParquetSink:
  /** the most rows a writer of this process has held — a test's memory check */
  val held: AtomicLong = AtomicLong(0)

  def to[A](lake: String, prefix: String, groupRows: Int = 64 * 1024)
           (using s: Schema[A], codec: ParquetCodec): Wire[A, Manifest] =
    require(groupRows > 0, "a row group holds at least one row")
    new Wire[A, Manifest]:
      final class Open(val file: java.nio.file.Path, val out: java.io.OutputStream,
                       val writer: okay.parquet.ParquetWriter):
        val buf = scala.collection.mutable.ArrayBuffer.empty[A]
        var rows = 0L
      type P = Open
      type W = Vector[Written]
      type S = Vector[Written]
      def wire: Schema[W] = summon[Schema[Vector[Written]]]
      def state: Schema[S] = summon[Schema[Vector[Written]]]
      def times: Vector[A => Long] = Vector.empty
      def slack: Long = 0L

      def start(bounds: Vector[Bounds]): P =
        val file = java.nio.file.Files.createTempFile("okay-lake", ".parquet")
        file.toFile.deleteOnExit()
        val out = java.io.BufferedOutputStream(java.nio.file.Files.newOutputStream(file))
        Open(file, out, codec.writer(b => out.write(b)))

      private def flush(p: P): Unit =
        if p.buf.nonEmpty then
          held.accumulateAndGet(p.buf.length.toLong, math.max): Unit
          p.writer.append(Rows.table(p.buf.toVector))
          p.buf.clear()

      def step(p: P, a: A): Unit =
        p.buf += a
        p.rows += 1
        if p.buf.length >= groupRows then flush(p)

      def finish(p: P): W =
        flush(p)
        p.writer.close()
        p.out.close()
        try
          if p.rows == 0 then Vector.empty
          else
            val key = s"$prefix/_data/${java.util.UUID.randomUUID()}.parquet"
            val bytes = java.nio.file.Files.size(p.file)
            val _ = Run(Lakes(lake).put(key, Bytes.file(p.file)))
            Vector(Written(key, p.rows, bytes))
        finally java.nio.file.Files.deleteIfExists(p.file): Unit

      def peek(p: P): W =
        throw IllegalStateException("a Parquet sink belongs to a batch run (Cluster.run): a partition's object is " +
          "whole only at its end — a stream writes its epochs through a staging sink (specs/dataflow.md, stage 17)")

      def empty: S = Vector.empty
      def absorb(st: S, ws: Vector[W], watermark: Long): S = st ++ ws.flatten
      def emit(st: S): Manifest =
        val m = Manifest(st.sortBy(_.key))
        emitted = Some(m)
        m
      def drops(ws: Vector[W]): Long = 0L
      def merged(ws: Vector[W]): Long = ws.length.toLong

      /** what `emit` answered, for the commit to make visible: the batch
       * run calls `result` (which emits) and then `committed`, in order */
      private var emitted: Option[Manifest] = None

      override def committed(epoch: Int): Unit = emitted.foreach { m =>
        val blob = Lakes(lake)
        Run(blob.putBytes(Manifest.keyOf(prefix),
          Codecs.writeJson(m).getBytes("UTF-8"))): Unit
        val named = m.files.map(_.key).toSet
        for stray <- Run(okay.Source.concat(blob.list(s"$prefix/_data/"))).map(_.key) if !named(stray) do
          Run(blob.delete(stray))
      }
