package okay.lake

import okay.arrow.{Column, Table}
import okay.codec.Json
import okay.parquet.ParquetCodec

/**
 * A DELTA TABLE AS THE ENGINE'S SOURCE (specs/dataflow.md, stage 18).
 *
 * The snapshot is a REPLAY of `_delta_log`: the last checkpoint (a
 * Parquet file, nested — okay-parquet reads it) holds every live file as
 * of its version, and the JSON commits after it add and remove files in
 * order. What is left is the table: its live data files, their partition
 * values, the schema that types those values. No Hadoop, no Delta Kernel
 * — see the spec's Decision for why a read is ours and a commit is not.
 *
 * Refused by name, because reading past them is reading WRONG: deletion
 * vectors (deleted rows would come back), column mapping (the files'
 * column names are not the table's), v2 checkpoints, and any reader
 * feature this reader does not know.
 */
object DeltaSource:

  /** the snapshot a replay arrives at */
  final case class Snapshot(version: Long, files: Vector[(String, Long, Vector[(String, Option[String])])],
                            columns: Vector[(String, String)], partitionColumns: Vector[String])

  /** reader features that change nothing a replay of adds and removes reads */
  private val Harmless = Set("timestampNtz", "vacuumProtocolCheck")

  def plan(lake: String, table: String)(using codec: ParquetCodec): LakePlan =
    val s = snapshot(lake, table)
    val blob = Lakes(lake)
    val parts = s.files.sortBy(_._1).flatMap { (path, size, partition) =>
      val key = s"$table/${decode(path)}"
      val f = codec.footer(BlobReadAt(blob, key, size))
      f.groups.zipWithIndex.map((rows, g) => Part(key, g, rows, partition))
    }
    LakePlan(lake, parts, s.columns.filter((n, _) => s.partitionColumns.contains(n)))

  def snapshot(lake: String, table: String)(using codec: ParquetCodec): Snapshot =
    val blob = Lakes(lake)
    val log = s"$table/_delta_log"
    val names = Run(okay.Source.concat(blob.list(s"$log/"))).map(_.key.stripPrefix(s"$log/"))
    val commits = names.flatMap(n => """^(\d{20})\.json$""".r.findFirstMatchIn(n).map(_.group(1).toLong)).sorted
    if commits.isEmpty && !names.exists(_.contains(".checkpoint.")) then
      throw IllegalStateException(s"'$table' is not a Delta table: no commits under $log")
    // a v2 checkpoint is UUID-named, with sidecars this replay does not read
    if names.exists(n => """^\d{20}\.checkpoint\.[0-9a-f]{8}-[0-9a-f-]{27}\.(json|parquet)$""".r.matches(n)) then
      throw IllegalStateException(s"'$table' has a v2 checkpoint (a UUID-named one): not read (specs/dataflow.md, stage 18)")

    val live = scala.collection.mutable.LinkedHashMap.empty[String, (Long, Vector[(String, Option[String])])]
    var schema = Vector.empty[(String, String)]
    var partitionColumns = Vector.empty[String]
    var version = -1L

    // the checkpoint, if there is one
    Run(blob.getBytes(s"$log/_last_checkpoint")).toOption.foreach { bytes =>
      val last = Json.parse(String(bytes, "UTF-8"))
      val v = num(field(last, "version")).getOrElse(throw IllegalStateException(s"'$table': _last_checkpoint without a version"))
      val parts = num(field(last, "parts"))
      val keys =
        parts match
          case None => Vector(f"$log/$v%020d.checkpoint.parquet")
          case Some(n) => (1 to n.toInt).toVector.map(i => f"$log/$v%020d.checkpoint.$i%010d.${n.toInt}%010d.parquet")
      for key <- keys do
        val size = Run(blob.head(key)).getOrElse(throw IllegalStateException(s"'$table': the checkpoint '$key' is gone")).size
        val t = codec.read(BlobReadAt(blob, key, size))
        checkpoint(t, table, live, meta => { schema = meta._1; partitionColumns = meta._2 })
      version = v
    }

    // the commits after it, in order and without a gap
    val after = commits.filter(_ > version)
    after.zipWithIndex.foreach { (c, i) =>
      if c != version + 1 + i then
        throw IllegalStateException(s"'$table': commit ${version + 1 + i} is missing from its log (found $c)")
    }
    for c <- after do
      val text = Run(blob.getBytes(f"$log/$c%020d.json")).fold(why => throw IllegalStateException(why), String(_, "UTF-8"))
      for line <- text.linesIterator if line.trim.nonEmpty do
        val a = Json.parse(line)
        field(a, "protocol").foreach(protocol(table, _))
        field(a, "metaData").foreach { m => val (s, p) = metaData(table, m); schema = s; partitionColumns = p }
        field(a, "add").foreach { add =>
          if field(add, "deletionVector").exists(_ != Json.JNull) then deletionVector(table)
          val path = str(field(add, "path")).getOrElse(throw IllegalStateException(s"'$table': an add without a path"))
          live.update(path, (num(field(add, "size")).getOrElse(0L), partitionValues(field(add, "partitionValues"))))
        }
        field(a, "remove").foreach { r => str(field(r, "path")).foreach(live.remove(_): Unit) }
      version = c
    Snapshot(version, live.toVector.map { case (p, (size, pv)) => (p, size, pv) }, schema, partitionColumns)

  /** a checkpoint's rows: every live file, the metadata and the protocol */
  private def checkpoint(t: Table, table: String,
                         live: scala.collection.mutable.LinkedHashMap[String, (Long, Vector[(String, Option[String])])],
                         meta: ((Vector[(String, String)], Vector[String])) => Unit): Unit =
    val cols = t.cols.toMap
    for i <- 0 until t.rows do
      cols.get("protocol").map(value(_, i)).foreach {
        case null => ()
        case p => protocolFields(table, p)
      }
      cols.get("metaData").map(value(_, i)).foreach {
        case null => ()
        case m => meta(metaFields(table, m))
      }
      cols.get("add").map(value(_, i)).foreach {
        case null => ()
        case add =>
          val f = fields(add)
          if f.get("deletionVector").exists(_ != null) then deletionVector(table)
          val path = f.get("path").collect { case s: String => s }.getOrElse(throw IllegalStateException(s"'$table': a checkpoint add without a path"))
          val size = f.get("size").collect { case n: Long => n }.getOrElse(0L)
          val pv = f.get("partitionValues").collect { case kvs: Vector[?] =>
            kvs.map(kv => fields(kv)).map(m => m.getOrElse("key", "").toString -> m.get("value").collect { case s: String => s })
          }.getOrElse(Vector.empty)
          live.update(path, (size, pv))
      }

  // ---------------------------------------------------------- the actions

  private def protocol(table: String, p: Json): Unit =
    val min = num(field(p, "minReaderVersion")).getOrElse(1L)
    val features = arr(field(p, "readerFeatures")).flatMap(j => str(Some(j)))
    readers(table, min, features)

  private def protocolFields(table: String, p: Any): Unit =
    val f = fields(p)
    val min = f.get("minReaderVersion").collect { case n: Long => n }.getOrElse(1L)
    val features = f.get("readerFeatures").collect { case xs: Vector[?] => xs.collect { case s: String => s } }.getOrElse(Vector.empty)
    readers(table, min, features)

  private def readers(table: String, min: Long, features: Vector[String]): Unit =
    if min == 2 then
      throw IllegalStateException(s"'$table' needs reader version 2 (column mapping): not read (specs/dataflow.md, stage 18)")
    if min > 3 then throw IllegalStateException(s"'$table' needs reader version $min: not read")
    val unknown = features.filterNot(Harmless)
    if unknown.nonEmpty then
      throw IllegalStateException(s"'$table' needs the reader features ${unknown.mkString(", ")}: not read — " +
        "a deletion vector or column mapping read past would return wrong rows (specs/dataflow.md, stage 18)")

  private def metaData(table: String, m: Json): (Vector[(String, String)], Vector[String]) =
    val conf = field(m, "configuration").collect { case Json.JObj(fs) => fs.collect { case (k, Json.JStr(v)) => k -> v } }.getOrElse(Vector.empty)
    mapping(table, conf.toMap)
    (schemaOf(str(field(m, "schemaString")).getOrElse("{}")), arr(field(m, "partitionColumns")).flatMap(j => str(Some(j))))

  private def metaFields(table: String, m: Any): (Vector[(String, String)], Vector[String]) =
    val f = fields(m)
    val conf = f.get("configuration").collect { case kvs: Vector[?] =>
      kvs.map(fields).map(x => x.getOrElse("key", "").toString -> x.getOrElse("value", "").toString).toMap
    }.getOrElse(Map.empty)
    mapping(table, conf)
    (schemaOf(f.get("schemaString").collect { case s: String => s }.getOrElse("{}")),
      f.get("partitionColumns").collect { case xs: Vector[?] => xs.collect { case s: String => s } }.getOrElse(Vector.empty))

  private def mapping(table: String, conf: Map[String, String]): Unit =
    conf.get("delta.columnMapping.mode").filter(_ != "none").foreach { mode =>
      throw IllegalStateException(s"'$table' maps its columns (delta.columnMapping.mode = $mode): not read (specs/dataflow.md, stage 18)")
    }

  private def deletionVector(table: String): Nothing =
    throw IllegalStateException(s"'$table' has a deletion vector: its deleted rows would be read — not read (specs/dataflow.md, stage 18)")

  /** a Delta schemaString's top-level fields: name to primitive type */
  private def schemaOf(text: String): Vector[(String, String)] =
    arr(field(Json.parse(text), "fields")).flatMap { f =>
      for n <- str(field(f, "name")); t <- str(field(f, "type")) yield n -> t
    }

  private def partitionValues(j: Option[Json]): Vector[(String, Option[String])] = j match
    case Some(Json.JObj(fs)) => fs.map { case (k, v) => k -> (v match { case Json.JStr(s) => Some(s); case _ => None }) }
    case _ => Vector.empty

  // -------------------------------------------------------------- helpers

  private def field(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.collectFirst { case (`name`, v) => v }
    case _ => None
  private def str(j: Option[Json]): Option[String] = j.collect { case Json.JStr(s) => s }
  private def num(j: Option[Json]): Option[Long] = j.collect { case Json.JNum(n) => n.toLong }
  private def arr(j: Option[Json]): Vector[Json] = j.collect { case Json.JArr(vs) => vs }.getOrElse(Vector.empty)

  /** a struct cell's fields */
  private def fields(v: Any): Map[String, Any] = v match
    case fs: Vector[?] => fs.collect { case (k: String, x) => k -> x }.toMap
    case _ => Map.empty

  /**
   * A CELL OF A CHECKPOINT as a plain value: null, a string, a number, a
   * struct as its (name, value) pairs, a list or map as a Vector. Bounded
   * by the column's type depth (`Column.MaxNesting`), which the reader
   * checked.
   */
  private def value(c: Column, i: Int): Any =
    if !c.validity(i) then null
    else c match
      case Column.Utf8(v, _) => v(i)
      case Column.Int64(v, _) => v(i)
      case Column.Ints(_, _, v, _) => v(i)
      case Column.Bool(v, _) => v(i)
      case Column.Float64(v, _) => v(i)
      case Column.Struct(fs, _) => fs.map((k, f) => k -> value(f, i))
      case Column.ListOf(o, child, _) => (o(i) until o(i + 1)).toVector.map(value(child, _))
      case other => other.getClass.getSimpleName

  /** a log path, percent-encoded as Delta writes them, as the object key */
  private def decode(path: String): String =
    if path.contains("://") then throw IllegalStateException(s"an absolute path in a Delta log ($path): not read")
    val b = java.io.ByteArrayOutputStream()
    var i = 0
    while i < path.length do
      val c = path.charAt(i)
      if c == '%' && i + 2 < path.length then
        b.write(Integer.parseInt(path.substring(i + 1, i + 3), 16)); i += 3
      else { b.write(String.valueOf(c).getBytes("UTF-8")); i += 1 }
    b.toString("UTF-8")
