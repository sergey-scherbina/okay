package okay.parquet

import okay.codec.Schema
import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Path, StandardOpenOption}

/** A PARQUET FILE ON THIS MACHINE'S DISK, as the reader's `ReadAt` and
 * as a `Bulk` format (bulk-parquet) */
object ParquetFile:
  /** each read one positional read of the file — the reader asks for
   * the footer, then one row group's column chunks */
  def readAt(path: String): ReadAt = new ReadAt:
    val size: Long = java.nio.file.Files.size(Path.of(path))
    def read(offset: Long, len: Int): Array[Byte] =
      val ch = FileChannel.open(Path.of(path), StandardOpenOption.READ)
      try
        val buf = ByteBuffer.allocate(len)
        var at = 0
        while at < len do
          val n = ch.read(buf, offset + at)
          if n < 0 then throw IllegalArgumentException(s"'$path': a read of $len bytes at $offset past its end")
          at += n
        buf.array()
      finally ch.close()

  /** the file's rows as `A`, one row group per split */
  def rows[A](using Schema[A], ParquetCodec): ParquetFormat[A] = ParquetFormat[A](readAt)
