package okay.lake

import okay.arrow.{Column, Table}
import okay.codec.Json
import okay.parquet.{ParquetCodec, ReadAt}
import scala.collection.mutable

/**
 * A HUDI COPY-ON-WRITE TABLE AS THE ENGINE'S SOURCE (specs/dataflow.md,
 * stage 18).
 *
 * A COW table's snapshot is, per FILE GROUP (a file id within a partition
 * directory), the latest BASE FILE written by a COMPLETED instant. Base
 * files are named `<fileId>_<writeToken>_<instantTime>.parquet`; the
 * timeline under `.hoodie/` (`.hoodie/timeline/` since layout 2, Hudi
 * 1.x) says which instants completed — `<requested>_<completed>.commit`
 * in layout 2, `<instant>.commit` in layout 1. A completed REPLACECOMMIT
 * (clustering, insert-overwrite) names file groups it replaced; those are
 * gone. A file written by an instant OLDER than the active timeline —
 * archived away — is committed: Hudi rolls back an uncommitted write
 * before it archives past it, which is Hudi's own rule for such files.
 *
 * A MERGE-ON-READ table (lake-hudi-mor) adds LOG FILES to a file slice:
 * `.<fileId>_<instant>.log.<version>_<writeToken>`, a sequence of
 * `#HUDI#` blocks, each stamped with the instant that wrote it. A slice
 * with logs is read whole and merged ([[merged]]); one without is read a
 * row group at a time, as COW. A log belongs to the latest committed
 * base of its file group when its instant is not older than the base's —
 * a compaction's base holds every log before it — and a block counts
 * only when its instant completed: a failed or rolled-back write's
 * blocks stay in the file and are skipped, which is Hudi's own rule.
 *
 * Refused by name: a base-file format that is not Parquet, a file group
 * with logs and no base file, a table without its meta fields
 * (`hoodie.populate.meta.fields=false` — the merge needs the record
 * key), a CUSTOM merge mode.
 */
object HudiSource:

  /** instant actions that write base files or log blocks */
  private val Writes = Set("commit", "deltacommit", "replacecommit")

  /** the base files, and a merge-on-read table's logs per base file and
   * its merge rule */
  final case class Snapshot(files: Vector[(String, Long)], layout: Int, completed: Int,
                            logs: Map[String, Vector[String]] = Map.empty, merge: Option[HudiMerge] = None)

  def snapshot(lake: String, table: String)(using avro: AvroReader): Snapshot =
    val blob = Lakes(lake)
    val props = Run(blob.getBytes(s"$table/.hoodie/hoodie.properties"))
      .fold(_ => throw IllegalStateException(s"'$table' is not a Hudi table: no .hoodie/hoodie.properties"), String(_, "UTF-8"))
      .linesIterator.filterNot(_.startsWith("#")).flatMap(l => l.split("=", 2) match
        case Array(k, v) => Some(k.trim -> v.trim)
        case _ => None).toMap
    val tpe = props.getOrElse("hoodie.table.type", "COPY_ON_WRITE")
    if tpe != "COPY_ON_WRITE" && tpe != "MERGE_ON_READ" then throw IllegalStateException(s"'$table' is a Hudi table of type $tpe")
    val mor = tpe == "MERGE_ON_READ"
    if mor && props.get("hoodie.populate.meta.fields").contains("false") then
      throw IllegalStateException(s"'$table' is MERGE_ON_READ without its meta fields: its logs cannot be merged without the record key")
    val format = props.getOrElse("hoodie.table.base.file.format", "PARQUET")
    if !format.equalsIgnoreCase("parquet") then throw IllegalStateException(s"'$table' keeps its base files as $format: only Parquet is read")
    val layout = props.get("hoodie.timeline.layout.version").flatMap(_.toIntOption).getOrElse(1)
    val timeline = if layout >= 2 then s"$table/.hoodie/${props.getOrElse("hoodie.timeline.path", "timeline")}" else s"$table/.hoodie"

    // the active timeline: every instant, and the completed writes
    val names = Run(okay.Source.concat(blob.list(s"$timeline/"))).map(_.key.stripPrefix(s"$timeline/")).filterNot(_.contains("/"))
    val Completed2 = """^(\d+)_(\d+)\.([a-z]+)$""".r
    val Completed1 = """^(\d+)\.([a-z]+)$""".r
    val Instant = """^(\d+)(?:_\d+)?\..*$""".r
    val all = names.collect { case Instant(t) => t }
    val done: Vector[(String, String, String)] = names.collect {   // (instant, action, file name)
      case n @ Completed2(t, _, action) if layout >= 2 => (t, action, n)
      case n @ Completed1(t, action) if layout < 2 && !n.endsWith(".inflight") => (t, action, n)
    }
    val committed = done.collect { case (t, a, _) if Writes(a) => t }.toSet
    val earliest = if all.isEmpty then "" else all.min

    // file groups a completed replacecommit replaced
    val replaced: Set[(String, String)] = done.collect { case (_, "replacecommit", n) => n }.flatMap { n =>
      val bytes = Run(blob.getBytes(s"$timeline/$n")).fold(why => throw IllegalStateException(why), identity)
      replacedIds(bytes)
    }.toSet

    // the base files, newest committed slice per file group
    val Base = """^(.*/)?([^/]+)_([^/_]+)_(\d+)\.parquet$""".r
    val Log = """^(.*/)?\.([^/_]+)_(\d+)\.log\.(\d+)_([^/.]+)$""".r   // not Hadoop's `..<name>.crc` beside it
    val objects = Run(okay.Source.concat(blob.list(s"$table/"))).filterNot(_.key.stripPrefix(s"$table/").startsWith(".hoodie"))
    val files = objects
      .flatMap { m =>
        m.key.stripPrefix(s"$table/") match
          case Base(dir, fileId, _, instant) =>
            val partition = Option(dir).getOrElse("").stripSuffix("/")
            if committed(instant) || (earliest.nonEmpty && instant < earliest) then Some(((partition, fileId), (instant, m.key, m.size)))
            else None
          case _ => None
      }
    val latest = files.groupBy(_._1).toVector.flatMap { (group, slices) =>
      if replaced(group) then None else Some(group -> slices.map(_._2).maxBy(_._1))
    }
    val bases = latest.map((_, b) => b._2 -> b._3).sortBy(_._1)
    if !mor then Snapshot(bases, layout, done.size)
    else
      // each file group's logs, in the order they merge: by instant, then version
      val logs = objects.flatMap { m =>
        m.key.stripPrefix(s"$table/") match
          case Log(dir, fileId, instant, version, token) =>
            Some(((Option(dir).getOrElse("").stripSuffix("/"), fileId), (instant, version.toInt, token, m.key)))
          case _ => None
      }.groupBy(_._1).map((g, ls) => g -> ls.map(_._2).sortBy(l => (l._1, l._2, l._3)))
      val baseOf = latest.toMap
      logs.foreach { (group, ls) =>
        if !baseOf.contains(group) && !replaced(group) then
          throw IllegalStateException(s"'$table': file group ${group._2} in '${group._1}' has log files and no committed base file " +
            s"(${ls.head._4}) — a log-only file group is not read")
      }
      val sliced = latest.flatMap { (group, base) =>
        val mine = logs.getOrElse(group, Vector.empty).filter(_._1 >= base._1).map(_._4)
        if mine.isEmpty then None else Some(base._2 -> mine)
      }.toMap
      Snapshot(bases, layout, done.size, sliced, Some(mergeRule(table, props, done.collect { case (t, a, _) if Writes(a) => t }, earliest)))

  /** the table's merge rule: Hudi 1.x names it (`hoodie.record.merge.mode`);
   * before it, the payload class decided — the default
   * OverwriteWithLatestAvroPayload is commit-time ordering,
   * DefaultHoodieRecordPayload event-time */
  private def mergeRule(table: String, props: Map[String, String], committed: Vector[String], earliest: String): HudiMerge =
    val payload = props.getOrElse("hoodie.compaction.payload.class", props.getOrElse("hoodie.payload.class", ""))
    val mode = props.get("hoodie.record.merge.mode").getOrElse(
      if payload.endsWith("DefaultHoodieRecordPayload") then "EVENT_TIME_ORDERING"
      else if payload.isEmpty || payload.endsWith("OverwriteWithLatestAvroPayload") then "COMMIT_TIME_ORDERING"
      else throw IllegalStateException(s"'$table' merges with its own payload class $payload: not read"))
    if mode != "EVENT_TIME_ORDERING" && mode != "COMMIT_TIME_ORDERING" then
      throw IllegalStateException(s"'$table' merges by $mode: only EVENT_TIME_ORDERING and COMMIT_TIME_ORDERING are read")
    val ordering = props.get("hoodie.table.ordering.fields").orElse(props.get("hoodie.table.precombine.field"))
      .toVector.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    if mode == "EVENT_TIME_ORDERING" && ordering.isEmpty then
      throw IllegalStateException(s"'$table' orders by event time and names no ordering field")
    HudiMerge(mode, ordering, committed.sorted, earliest)

  /** the plan: a row group per part, a merge-on-read slice with logs one
   * part (`group` -1) */
  def plan(lake: String, table: String)(using avro: AvroReader, codec: ParquetCodec): LakePlan =
    val blob = Lakes(lake)
    val s = snapshot(lake, table)
    LakePlan(lake, s.files.flatMap { (key, size) =>
      val groups = codec.footer(BlobReadAt(blob, key, size)).groups
      s.logs.get(key) match
        case Some(logs) => Vector(Part(key, -1, groups.sum, logs = logs))
        case None => groups.zipWithIndex.map((rows, g) => Part(key, g, rows))
    }, hudi = s.merge)

  // ---- merge-on-read ------------------------------------------------------

  /** the record key, Hudi's first meta field */
  val RecordKey = "_hoodie_record_key"
  /** the field a writer sets true to delete its record */
  val IsDeleted = "_hoodie_is_deleted"

  /** a log block: its type, header (by Hudi's key number) and content */
  private final case class Block(kind: Int, header: Map[Int, String], content: Array[Byte])
  private val Magic = "#HUDI#".getBytes("US-ASCII")
  private object Kind:
    val Command = 0; val Delete = 1; val Corrupt = 2; val AvroData = 3; val HFile = 4; val ParquetData = 5; val Cdc = 6
  private object Header:
    val InstantTime = 0; val Schema = 2

  /** a log file's blocks, in order. A block whose declared size runs past
   * the end is the torn tail of a write that failed — the end of the
   * file, as for Hudi; a block without its magic is refused. */
  private def blocks(bytes: Array[Byte], key: String): Vector[Block] =
    val b = java.nio.ByteBuffer.wrap(bytes)
    val out = Vector.newBuilder[Block]
    var at = 0
    var torn = false
    while !torn && at < bytes.length do
      if bytes.length - at < Magic.length + 8 then torn = true
      else
        if !java.util.Arrays.equals(bytes, at, at + Magic.length, Magic, 0, Magic.length) then
          throw IllegalStateException(s"'$key': no #HUDI# block at offset $at")
        val size = b.getLong(at + Magic.length)
        val start = at + Magic.length + 8
        if size < 0 || start + size > bytes.length then torn = true
        else
          b.position(start)
          val version = b.getInt()
          val kind = b.getInt()
          if version < 1 then throw IllegalStateException(s"'$key': a log block of format version $version is not read")
          val header = entries(b)
          val len = b.getLong()
          if len < 0 || b.position() + len > start + size then throw IllegalStateException(s"'$key': a log block's content runs past it")
          val content = java.util.Arrays.copyOfRange(bytes, b.position(), b.position() + len.toInt)
          out += Block(kind, header, content)
          at = (start + size).toInt
    out.result()

  private def entries(b: java.nio.ByteBuffer): Map[Int, String] =
    val n = b.getInt()
    (0 until n).map { _ =>
      val k = b.getInt()
      val v = new Array[Byte](b.getInt())
      b.get(v)
      k -> String(v, "UTF-8")
    }.toMap

  /** where a record stands in the merge: a base row, a log record, or deleted */
  private enum Entry:
    case FromBase(row: Int, ord: Vector[Any])
    case FromLog(rec: Map[String, Any], ord: Vector[Any])
    case Gone(ord: Vector[Any])
    def ordering: Vector[Any] = this match
      case FromBase(_, o) => o
      case FromLog(_, o) => o
      case Gone(o) => o

  /**
   * A FILE SLICE, MERGED: the base file whole, then every block of its logs
   * whose instant completed, in order, by record key. A data record
   * replaces the current one — under EVENT_TIME_ORDERING only when its
   * ordering value is not lower (a tie goes to the later write), under
   * COMMIT_TIME_ORDERING always. A delete removes it, under event time
   * only when its ordering value is not lower, and always when it carries
   * none or Hudi's default (0: "delete regardless"). The rows are the
   * surviving base rows in their order, then the log records' winners.
   */
  private[lake] def merged(blob: okay.blob.Blob, in: ReadAt, part: Part, m: HudiMerge)(using codec: ParquetCodec, avro: AvroReader): Table =
    val footer = codec.footer(in)
    if footer.groups.isEmpty then throw IllegalStateException(s"'${part.key}' has no row groups")
    val groups = footer.groups.indices.map(codec.group(in, footer, _)).toVector
    val base = Table(groups.head.cols.indices.toVector.map(c =>
      groups.head.cols(c)._1 -> Column.concat(groups.map(_.cols(c)._2)).decoded), groups.head.metadata)
    val cols = base.cols.toMap
    val keys = cols.get(RecordKey) match
      case Some(c: Column.Utf8) => c
      case other => throw IllegalStateException(s"'${part.key}' has no string $RecordKey column (${other.map(Column.describe)})")
    val eventTime = m.mode == "EVENT_TIME_ORDERING"
    val orderCols = m.ordering.map(f => cols.getOrElse(f, throw IllegalStateException(s"'${part.key}' has no ordering field '$f'")))
    val state = mutable.LinkedHashMap.empty[String, Entry]
    var i = 0
    while i < base.rows do
      if keys.valid(i) then state.update(keys.values(i), Entry.FromBase(i, orderCols.map(valueAt(_, i))))
      i += 1

    val committed = m.committed.toSet
    def counts(instant: String) = committed(instant) || (m.earliest.nonEmpty && instant < m.earliest)
    def wins(incoming: Vector[Any], current: Option[Entry]): Boolean =
      !eventTime || current.forall(c => compare(incoming, c.ordering) >= 0)
    val decoders = mutable.Map.empty[String, Array[Byte] => Any]
    val deletes = avro.decoder(DeleteSchema)

    part.logs.foreach { key =>
      val bytes = Run(blob.getBytes(key)).fold(why => throw IllegalStateException(why), identity)
      blocks(bytes, key).foreach { block =>
        val instant = block.header.getOrElse(Header.InstantTime, throw IllegalStateException(s"'$key': a log block without its instant"))
        if counts(instant) then block.kind match
          case Kind.AvroData =>
            val schema = block.header.getOrElse(Header.Schema, throw IllegalStateException(s"'$key': a data block without its schema"))
            val decode = decoders.getOrElseUpdate(schema, avro.decoder(schema))
            records(block.content, key).foreach { bytes =>
              val rec = fields(decode(bytes), key)
              val k = rec.get(RecordKey) match
                case Some(s: String) => s
                case _ => throw IllegalStateException(s"'$key': a log record without its $RecordKey")
              val ord = m.ordering.map(rec.getOrElse(_, null))
              // a soft delete: the record carries `_hoodie_is_deleted`
              val next = if rec.get(IsDeleted).contains(true) then Entry.Gone(ord) else Entry.FromLog(rec, ord)
              if wins(ord, state.get(k)) then state.update(k, next)
            }
          case Kind.Delete =>
            val b = java.nio.ByteBuffer.wrap(block.content)
            val version = b.getInt()
            if version < 3 then throw IllegalStateException(s"'$key': a delete block of version $version (Kryo) is not read")
            val payload = new Array[Byte](b.getInt())
            b.get(payload)
            fields(deletes(payload), key).get("deleteRecordList") match
              case Some(ds: Vector[?]) => ds.foreach { d =>
                val del = fields(d, key)
                del.get("recordKey") match
                  case Some(k: String) =>
                    val ord = orderingOf(del.getOrElse("orderingVal", null))
                    if ord.isEmpty || ord == Vector(0L) || wins(ord, state.get(k)) then state.update(k, Entry.Gone(ord))
                  case _ => throw IllegalStateException(s"'$key': a delete without its record key")
              }
              case _ => throw IllegalStateException(s"'$key': a delete block without its list")
          case Kind.Command => ()   // a rollback: its target never completed, so its blocks never count
          case other =>
            val name = other match
              case Kind.HFile => "HFILE_DATA"
              case Kind.ParquetData => "PARQUET_DATA"
              case Kind.Cdc => "CDC_DATA"
              case Kind.Corrupt => "CORRUPT"
              case n => s"type $n"
            throw IllegalStateException(s"'$key': a $name log block is not read — Avro data and delete blocks are")
      }
    }

    val kept = state.valuesIterator.collect { case Entry.FromBase(r, _) => r }.toArray.sorted
    val logged = state.valuesIterator.collect { case Entry.FromLog(rec, _) => rec }.toVector
    val t = Table(base.cols.map { (name, c) =>
      val survivors = c.take(kept, Array.fill(kept.length)(true))
      name -> (if logged.isEmpty then survivors
               else Column.concat(Vector(survivors, fromValues(c, logged.map(_.getOrElse(name, null)), name, part.key))))
    }, base.metadata)
    // a base-file row that is a soft delete is gone too
    t.cols.collectFirst { case (IsDeleted, Column.Bool(v, ok)) => (v, ok) } match
      case Some((v, ok)) if v.indices.exists(r => ok(r) && v(r)) =>
        val live = v.indices.filterNot(r => ok(r) && v(r)).toArray
        Table(t.cols.map((n, c) => n -> c.take(live, Array.fill(live.length)(true))), t.metadata)
      case _ => t

  /** the records of an Avro data block: version, count, then each record's
   * length and bytes */
  private def records(content: Array[Byte], key: String): Vector[Array[Byte]] =
    val b = java.nio.ByteBuffer.wrap(content)
    val version = b.getInt()
    if version < 2 then throw IllegalStateException(s"'$key': an Avro data block of version $version is not read")
    val n = b.getInt()
    Vector.fill(n) {
      val r = new Array[Byte](b.getInt())
      b.get(r)
      r
    }

  private def fields(v: Any, key: String): Map[String, Any] = v match
    case fs: Vector[?] => fs.collect { case (k: String, x) => k -> x }.toMap
    case other => throw IllegalStateException(s"'$key': a log record that is not a record: $other")

  /** a delete's ordering value: none, one wrapper, or an ArrayWrapper of them */
  private def orderingOf(v: Any): Vector[Any] = v match
    case null => Vector.empty
    case Vector(("value", x)) => Vector(plain(x))
    case Vector(("wrappedValues", xs: Vector[?])) => xs.map {
      case Vector(("value", x)) => plain(x)
      case null => null
      case other => throw IllegalStateException(s"a delete's ordering value $other")
    }
    case Vector(("wrappedValues", null)) => Vector.empty
    case other => throw IllegalStateException(s"a delete's ordering value $other")

  private def plain(x: Any): Any = x match
    case i: Int => i.toLong
    case f: Float => f.toDouble
    case other => other

  /** a base column's value at a row, for comparing ordering values */
  private def valueAt(c: Column, i: Int): Any =
    if !c.validity(i) then null
    else c match
      case Column.Int64(v, _) => v(i)
      case Column.Ints(_, _, v, _) => v(i)
      case Column.Float64(v, _) => v(i)
      case Column.Float32(v, _) => v(i).toDouble
      case Column.Utf8(v, _) => v(i)
      case Column.Bool(v, _) => v(i)
      case Column.Date32(v, _) => v(i).toLong
      case Column.Timestamp(_, _, v, _) => v(i)
      case other => throw IllegalStateException(s"an ordering field of type ${Column.describe(other)} is not compared")

  /** ordering values, compared field by field; null is lowest */
  private def compare(a: Vector[Any], b: Vector[Any]): Int =
    a.zipAll(b, null, null).iterator.map((x, y) => one(plain(x), plain(y))).find(_ != 0).getOrElse(0)

  private def one(x: Any, y: Any): Int = (x, y) match
    case (null, null) => 0
    case (null, _) => -1
    case (_, null) => 1
    case (a: Long, b: Long) => java.lang.Long.compare(a, b)
    case (a: Long, b: Double) => java.lang.Double.compare(a.toDouble, b)
    case (a: Double, b: Long) => java.lang.Double.compare(a, b.toDouble)
    case (a: Double, b: Double) => java.lang.Double.compare(a, b)
    case (a: String, b: String) => a.compareTo(b)
    case (a: Boolean, b: Boolean) => java.lang.Boolean.compare(a, b)
    case _ => throw IllegalStateException(s"ordering values $x and $y do not compare")

  /** log records' values as a column of the base column's type */
  private def fromValues(proto: Column, vs: Vector[Any], name: String, key: String): Column =
    val ok = vs.map(_ != null).toArray
    def bad(v: Any) = throw IllegalStateException(s"'$key': column '$name' (${Column.describe(proto)}) given $v in a log record")
    def long(v: Any): Long = v match { case null => 0L; case l: Long => l; case i: Int => i.toLong; case o => bad(o) }
    def double(v: Any): Double = v match { case null => 0.0; case d: Double => d; case f: Float => f.toDouble; case l: Long => l.toDouble; case o => bad(o) }
    def bytes(v: Any): Array[Byte] = v match { case null => Array.emptyByteArray; case b: Array[Byte] => b; case o => bad(o) }
    proto match
      case Column.Int64(_, _) => Column.Int64(vs.map(long).toArray, ok)
      case Column.Ints(bits, signed, _, _) => Column.Ints(bits, signed, vs.map(long).toArray, ok)
      case Column.Float64(_, _) => Column.Float64(vs.map(double).toArray, ok)
      case Column.Float32(_, _) => Column.Float32(vs.map(double(_).toFloat).toArray, ok)
      case Column.Utf8(_, _) => Column.Utf8(vs.map { case null => ""; case s: String => s; case o => bad(o) }.toArray, ok)
      case Column.Bool(_, _) => Column.Bool(vs.map { case null => false; case b: Boolean => b; case o => bad(o) }.toArray, ok)
      case Column.Binary(_, _) => Column.Binary(vs.map(bytes).toArray, ok)
      case Column.FixedBinary(w, _, _) => Column.FixedBinary(w, vs.map(bytes).toArray, ok)
      case Column.Decimal(p, s, _, _) => Column.Decimal(p, s, vs.map(v => if v == null then BigInt(0) else BigInt(bytes(v))).toArray, ok)
      case Column.Date32(_, _) => Column.Date32(vs.map(long(_).toInt).toArray, ok)
      // Hudi's Avro keeps Spark's microseconds, which is what its Parquet holds
      case Column.Timestamp(u, z, _, _) => Column.Timestamp(u, z, vs.map(long).toArray, ok)
      case Column.Nulls(_) => Column.Nulls(vs.length)
      case other => throw IllegalStateException(s"'$key': column '$name' of type ${Column.describe(other)} is not merged from a log")

  /** HoodieDeleteRecordList, Hudi 1.x's delete block payload (its own
   * Avro schema, the docs and namespace left out) */
  private val DeleteSchema: String =
    """{"type":"record","name":"HoodieDeleteRecordList","fields":[{"name":"deleteRecordList","type":{"type":"array","items":{"type":"record","name":"HoodieDeleteRecord","fields":[{"name":"recordKey","type":["null",{"type":"string"}]},{"name":"partitionPath","type":["null",{"type":"string"}]},{"name":"orderingVal","type":["null",{"type":"record","name":"BooleanWrapper","fields":[{"name":"value","type":"boolean"}]},{"type":"record","name":"IntWrapper","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"LongWrapper","fields":[{"name":"value","type":"long"}]},{"type":"record","name":"FloatWrapper","fields":[{"name":"value","type":"float"}]},{"type":"record","name":"DoubleWrapper","fields":[{"name":"value","type":"double"}]},{"type":"record","name":"BytesWrapper","fields":[{"name":"value","type":"bytes"}]},{"type":"record","name":"StringWrapper","fields":[{"name":"value","type":{"type":"string"}}]},{"type":"record","name":"DateWrapper","fields":[{"name":"value","type":"int"}]},{"type":"record","name":"DecimalWrapper","fields":[{"name":"value","type":{"type":"bytes","logicalType":"decimal","precision":30,"scale":15}}]},{"type":"record","name":"TimeMicrosWrapper","fields":[{"name":"value","type":{"type":"long","logicalType":"time-micros"}}]},{"type":"record","name":"TimestampMicrosWrapper","fields":[{"name":"value","type":"long"}]},{"type":"record","name":"ArrayWrapper","fields":[{"name":"wrappedValues","type":["null",{"type":"array","items":["null","BooleanWrapper","IntWrapper","LongWrapper","FloatWrapper","DoubleWrapper","BytesWrapper","StringWrapper","DateWrapper","DecimalWrapper","TimeMicrosWrapper","TimestampMicrosWrapper",{"type":"record","name":"LocalDateWrapper","fields":[{"name":"value","type":"int"}]}]}]}]}]}]}}}]}"""

  /** (partition, file id) pairs a replacecommit's metadata replaced —
   * Avro in Hudi 1.x, JSON before it */
  private def replacedIds(bytes: Array[Byte])(using avro: AvroReader): Vector[(String, String)] =
    if bytes.length >= 4 && bytes(0) == 'O' && bytes(1) == 'b' && bytes(2) == 'j' then
      avro.records(bytes).flatMap {
        case fs: Vector[?] =>
          fs.collect { case ("partitionToReplaceFileIds", m: Map[?, ?]) => m }.flatMap(_.toVector.flatMap {
            case (p: String, ids: Vector[?]) => ids.collect { case id: String => p -> id }
            case _ => Vector.empty
          })
        case _ => Vector.empty
      }
    else
      Json.parse(String(bytes, "UTF-8")) match
        case Json.JObj(fs) =>
          fs.collectFirst { case ("partitionToReplaceFileIds", Json.JObj(ps)) => ps }.getOrElse(Vector.empty).flatMap {
            case (p, Json.JArr(ids)) => ids.collect { case Json.JStr(id) => p -> id }
            case _ => Vector.empty
          }
        case _ => Vector.empty
