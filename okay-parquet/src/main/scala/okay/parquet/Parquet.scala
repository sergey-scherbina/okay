package okay.parquet

import okay.arrow.Table

/**
 * WHERE A PARQUET FILE IS READ FROM (specs/parquet.md): its size and any
 * byte range of it. A file on disk, an object in S3 read by range GET,
 * an array — each is these two methods, and the reader asks for the
 * footer, then one row group's column chunks at a time.
 */
trait ReadAt:
  def size: Long
  def read(offset: Long, len: Int): Array[Byte]

object ReadAt:
  def of(bytes: Array[Byte]): ReadAt = new ReadAt:
    def size: Long = bytes.length.toLong
    def read(offset: Long, len: Int): Array[Byte] =
      if offset < 0 || offset + len > bytes.length then
        throw IllegalArgumentException(s"a read of $len bytes at $offset past a file of ${bytes.length}")
      java.util.Arrays.copyOfRange(bytes, offset.toInt, offset.toInt + len)

/** a file's shape, from its footer: the columns and each row group's
 * rows. `parsed` is the implementation's own reading of the footer,
 * kept so reading a row group does not fetch the footer again (on an
 * object store that is a request per group) — outside equality */
final case class Footer(columns: Vector[(String, String)], groups: Vector[Long],
                        metadata: Vector[(String, String)], createdBy: Option[String])
                       (private[parquet] val parsed: Any = null):
  def rows: Long = groups.sum

/** how a written file's pages are compressed */
enum Compress:
  case None, Snappy, Zstd

/** a file being written: row groups appended, the footer at `close` */
trait ParquetWriter:
  def append(group: Table): Unit
  def close(): Unit

/**
 * PARQUET, AS A CHOICE (specs/own-or-standard.md): ours, `OkayParquet`,
 * is the default on every platform; parquet-java's is `ParquetJava.given`
 * on the JVM. The files are the same either way — each reads the other's.
 */
trait ParquetCodec:
  def name: String
  /** the footer: columns (name, type as `Column.describe` names it) and
   * row groups */
  def footer(in: ReadAt): Footer
  /** row group `g`, every column or those named */
  def group(in: ReadAt, footer: Footer, g: Int, columns: Option[Set[String]] = None)
           (using okay.compress.Compression): Table
  /** a writer handing bytes to `out` as each row group is appended */
  def writer(out: Array[Byte] => Unit, compress: Compress = Compress.Snappy,
             metadata: Vector[(String, String)] = Vector.empty)
            (using okay.compress.Compression): ParquetWriter

  /** every row group, one after another, as one table — a convenience
   * for a small file; a large one is read by `group` */
  final def read(in: ReadAt)(using okay.compress.Compression): Table =
    val f = footer(in)
    val parts = f.groups.indices.map(g => group(in, f, g)).toVector
    if parts.isEmpty then Table(Vector.empty, f.metadata)
    else Table(parts.head.cols.indices.toVector.map(c =>
      parts.head.cols(c)._1 -> okay.arrow.Column.concat(parts.map(_.cols(c)._2))), f.metadata)

  /** a whole table as one file of row groups of at most `groupRows` */
  final def write(t: Table, groupRows: Int = 1 << 20, compress: Compress = Compress.Snappy)
                 (using okay.compress.Compression): Array[Byte] =
    val out = java.io.ByteArrayOutputStream()
    val w = writer(b => out.write(b), compress, t.metadata)
    var at = 0
    while at < t.rows || (at == 0 && t.rows == 0) do
      val n = math.min(groupRows, t.rows - at)
      w.append(Table(t.cols.map((name, c) => name -> Slice(c, at, n)), t.metadata))
      at += math.max(n, 1)
    w.close()
    out.toByteArray

object ParquetCodec:
  /** THE DEFAULT: ours, on every platform */
  given own: ParquetCodec = OkayParquet

/** rows `[at, at + n)` of a column */
private[parquet] object Slice:
  def apply(c: okay.arrow.Column, at: Int, n: Int): okay.arrow.Column =
    if at == 0 && n == c.length then c else c.take(Array.range(at, at + n), Array.fill(n)(true))

/** refused input, and why */
final class Refused(why: String) extends IllegalArgumentException(why)
