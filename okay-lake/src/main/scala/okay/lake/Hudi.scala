package okay.lake

import okay.codec.Json
import okay.parquet.ParquetCodec

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
 * Refused by name: MERGE_ON_READ (its log files hold rows its base files
 * do not), a base-file format that is not Parquet.
 */
object HudiSource:

  /** instant actions that write base files */
  private val Writes = Set("commit", "replacecommit")

  final case class Snapshot(files: Vector[(String, Long)], layout: Int, completed: Int)

  def snapshot(lake: String, table: String)(using avro: AvroReader): Snapshot =
    val blob = Lakes(lake)
    val props = Run(blob.getBytes(s"$table/.hoodie/hoodie.properties"))
      .fold(_ => throw IllegalStateException(s"'$table' is not a Hudi table: no .hoodie/hoodie.properties"), String(_, "UTF-8"))
      .linesIterator.filterNot(_.startsWith("#")).flatMap(l => l.split("=", 2) match
        case Array(k, v) => Some(k.trim -> v.trim)
        case _ => None).toMap
    val tpe = props.getOrElse("hoodie.table.type", "COPY_ON_WRITE")
    if tpe != "COPY_ON_WRITE" then
      throw IllegalStateException(s"'$table' is $tpe: its log files hold rows its base files do not — only COPY_ON_WRITE is read " +
        "(specs/dataflow.md, stage 18)")
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
    val files = Run(okay.Source.concat(blob.list(s"$table/")))
      .filterNot(_.key.stripPrefix(s"$table/").startsWith(".hoodie"))
      .flatMap { m =>
        m.key.stripPrefix(s"$table/") match
          case Base(dir, fileId, _, instant) =>
            val partition = Option(dir).getOrElse("").stripSuffix("/")
            if committed(instant) || (earliest.nonEmpty && instant < earliest) then Some(((partition, fileId), (instant, m.key, m.size)))
            else None
          case _ => None
      }
    val latest = files.groupBy(_._1).toVector.flatMap { (group, slices) =>
      if replaced(group) then None else Some(slices.map(_._2).maxBy(_._1))
    }
    Snapshot(latest.map((_, key, size) => key -> size).sortBy(_._1), layout, done.size)

  def plan(lake: String, table: String)(using avro: AvroReader, codec: ParquetCodec): LakePlan =
    val blob = Lakes(lake)
    LakePlan(lake, snapshot(lake, table).files.flatMap { (key, size) =>
      codec.footer(BlobReadAt(blob, key, size)).groups.zipWithIndex.map((rows, g) => Part(key, g, rows))
    })

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
