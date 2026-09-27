package okay.parquet

import okay.Bulk
import okay.arrow.Rows
import okay.codec.Schema

/**
 * PARQUET AS A `Bulk` SOURCE (bulk-parquet, specs/bulk.md): a file's row
 * groups are its splits, and a group reads as rows of `A` by A's Schema
 * (okay-arrow `Rows`), PRUNED at the reader to A's fields — a column the
 * type does not name is never decoded. `open` turns a path into the bytes
 * of the file wherever the split is read (`ParquetFile.rows` on a JVM
 * opens a local file); the codec is ours unless another is given.
 *
 * {{{
 * val trips = localBulk.read(path, ParquetFile.rows[Ride])
 * }}}
 */
final class ParquetFormat[A](open: String => ReadAt)(using s: Schema[A], codec: ParquetCodec) extends Bulk.Format[A]:
  def name: String = s"parquet (${codec.name})"

  /** the fields a row of `A` reads: pruning, when A is a record */
  private val fields: Option[Set[String]] = s match
    case p: Schema.SProduct[?] => Some(p.fields.map(_._1).toSet)
    case _ => None

  def splits(path: String): Vector[Int] = codec.footer(open(path)).groups.indices.toVector

  def read(path: String, split: Int): Iterator[A] =
    val in = open(path)
    val footer = codec.footer(in)
    val table = codec.group(in, footer, split, fields)
    Rows.rows[A](table).fold(why => throw IllegalStateException(s"'$path' row group $split: $why"), _.iterator)
