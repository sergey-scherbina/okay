package okay.lake

import okay.codec.Json
import okay.parquet.ParquetCodec

/**
 * AN ICEBERG TABLE AS THE ENGINE'S SOURCE (specs/dataflow.md, stage 18).
 *
 * The current snapshot of a table is named by its metadata JSON
 * (`current-snapshot-id`); the snapshot's MANIFEST LIST names its
 * manifests, and each manifest's entries name data files — ADDED and
 * EXISTING ones are live, DELETED ones are not. The live Parquet files
 * become okay-lake's row-group plan. Manifests are Avro, read by the
 * `AvroReader` in scope (ours by default).
 *
 * Paths in Iceberg's metadata are absolute URIs (`s3://bucket/…`,
 * `file:///…`); `root` is the URI the lake's keys are relative to.
 *
 * Refused by name: delete files (position or equality deletes — rows the
 * data files still hold but the table does not), a data file that is not
 * Parquet, a snapshot the metadata does not have. Columns are read by
 * NAME: a table whose columns were renamed after its files were written
 * is read by the new names and misses those columns — Iceberg reads by
 * field id, and that is a stage of its own.
 */
object IcebergSource:

  /** the live data files of the metadata's current snapshot: (uri, size) */
  def files(lake: String, metadataKey: String, root: String)(using avro: AvroReader): Vector[(String, Long)] =
    val blob = Lakes(lake)
    val meta = Json.parse(Run(blob.getBytes(metadataKey)).fold(why => throw IllegalStateException(why), String(_, "UTF-8")))
    val current = num(field(meta, "current-snapshot-id"))
    current match
      case None | Some(-1L) => Vector.empty          // a table with no snapshot is empty
      case Some(id) =>
        val snaps = arr(field(meta, "snapshots"))
        val snap = snaps.find(s => num(field(s, "snapshot-id")).contains(id))
          .getOrElse(throw IllegalStateException(s"'$metadataKey' names snapshot $id and does not hold it"))
        val manifests: Vector[(String, Long)] = str(field(snap, "manifest-list")) match
          case Some(list) =>
            avro.records(read(blob, key(list, root))).map { r =>
              val f = fields(r)
              (f.get("manifest_path").collect { case s: String => s }.getOrElse(throw IllegalStateException(s"'$list': a manifest without a path")),
                f.get("content").collect { case n: Long => n }.getOrElse(0L))
            }
          case None =>
            // format v1 may list its manifests in the snapshot itself
            arr(field(snap, "manifests")).flatMap(j => str(Some(j))).map(_ -> 0L)
        manifests.flatMap { (path, content) =>
          val entries = avro.records(read(blob, key(path, root))).map(fields)
          val live = entries.filter(e => !e.get("status").contains(2L))
          if content != 0L && live.nonEmpty then
            throw IllegalStateException(s"'$metadataKey' has DELETE FILES (manifest '$path'): rows its data files hold are " +
              "not the table's — not read (specs/dataflow.md, stage 18)")
          live.map { e =>
            val d = e.get("data_file").map(fields).getOrElse(throw IllegalStateException(s"'$path': an entry without a data file"))
            val kind = d.get("content").collect { case n: Long => n }.getOrElse(0L)
            if kind != 0L then
              throw IllegalStateException(s"'$metadataKey' has a DELETE FILE: not read (specs/dataflow.md, stage 18)")
            val file = d.get("file_path").collect { case s: String => s }.getOrElse(throw IllegalStateException(s"'$path': a data file without a path"))
            val format = d.get("file_format").collect { case s: String => s }.getOrElse("PARQUET")
            if !format.equalsIgnoreCase("parquet") then
              throw IllegalStateException(s"'$file' is $format: only Parquet data files are read")
            file -> d.get("file_size_in_bytes").collect { case n: Long => n }.getOrElse(0L)
          }
        }

  def plan(lake: String, metadataKey: String, root: String)(using avro: AvroReader, codec: ParquetCodec): LakePlan =
    val blob = Lakes(lake)
    LakePlan(lake, files(lake, metadataKey, root).sortBy(_._1).flatMap { (uri, size) =>
      val k = key(uri, root)
      codec.footer(BlobReadAt(blob, k, size)).groups.zipWithIndex.map((rows, g) => Part(k, g, rows))
    })

  /** an absolute URI in the metadata as the lake's key */
  private def key(uri: String, root: String): String =
    val r = root.stripSuffix("/") + "/"
    if !uri.startsWith(r) then throw IllegalStateException(s"'$uri' is outside the lake's root '$root'")
    uri.stripPrefix(r)

  private def read(blob: okay.blob.Blob, k: String): Array[Byte] =
    Run(blob.getBytes(k)).fold(why => throw IllegalStateException(why), identity)

  private def field(j: Json, name: String): Option[Json] = j match
    case Json.JObj(fs) => fs.collectFirst { case (`name`, v) => v }
    case _ => None
  private def str(j: Option[Json]): Option[String] = j.collect { case Json.JStr(s) => s }
  private def num(j: Option[Json]): Option[Long] = j.collect { case Json.JNum(n) => n.toLong }
  private def arr(j: Option[Json]): Vector[Json] = j.collect { case Json.JArr(vs) => vs }.getOrElse(Vector.empty)

  /** an Avro record's fields */
  private def fields(v: Any): Map[String, Any] = v match
    case fs: Vector[?] => fs.collect { case (k: String, x) => k -> x }.toMap
    case _ => Map.empty
