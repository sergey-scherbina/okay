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

  /**
   * THE PLAN, columns matched by FIELD ID (lake-iceberg-field-ids): each
   * file's columns are renamed to the current schema's names by the ids
   * its Parquet schema carries, and a column the file does not have (added
   * after it was written) arrives as nulls typed by the schema. A file
   * whose columns carry no ids is read by name, as before.
   */
  def plan(lake: String, metadataKey: String, root: String)(using avro: AvroReader, codec: ParquetCodec): LakePlan =
    val blob = Lakes(lake)
    val fields = schema(lake, metadataKey)
    LakePlan(lake, files(lake, metadataKey, root).sortBy(_._1).flatMap { (uri, size) =>
      val k = key(uri, root)
      val f = codec.footer(BlobReadAt(blob, k, size))
      val byId = f.fieldIds.zip(f.columns.map(_._1)).collect { case (Some(id), n) => id -> n }.toMap
      val (rename, missing) =
        if byId.isEmpty then (Vector.empty, Vector.empty)
        else
          (fields.flatMap((id, n, _) => byId.get(id).filter(_ != n).map(_ -> n)),
            fields.collect { case (id, n, _) if !byId.contains(id) => n -> Option.empty[String] })
      f.groups.zipWithIndex.map((rows, g) => Part(k, g, rows, missing, rename))
    }, fields.flatMap((_, n, t) => typeName(t).map(n -> _)))

  /** the current schema's top-level fields: (id, name, Iceberg type) */
  def schema(lake: String, metadataKey: String): Vector[(Int, String, String)] =
    val meta = Json.parse(Run(Lakes(lake).getBytes(metadataKey)).fold(why => throw IllegalStateException(why), String(_, "UTF-8")))
    val current = num(field(meta, "current-schema-id"))
    val s = current.flatMap(id => arr(field(meta, "schemas")).find(x => num(field(x, "schema-id")).contains(id)))
      .orElse(field(meta, "schema"))
      .getOrElse(throw IllegalStateException(s"'$metadataKey' has no current schema"))
    arr(field(s, "fields")).flatMap { f =>
      for id <- num(field(f, "id")); n <- str(field(f, "name")) yield
        (id.toInt, n, field(f, "type").collect { case Json.JStr(t) => t }.getOrElse("struct"))
    }

  /** an Iceberg primitive type as okay-lake's partition-value type */
  private def typeName(t: String): Option[String] = t match
    case "long" => Some("long")
    case "int" => Some("integer")
    case "string" => Some("string")
    case "double" => Some("double")
    case "float" => Some("float")
    case "boolean" => Some("boolean")
    case "date" => Some("date")
    case "timestamp" | "timestamptz" => Some("timestamp")
    case _ => None

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
