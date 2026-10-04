package okay2.persist

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.zip.CRC32C
import scala.jdk.CollectionConverters._

/**
 * "Is this backup restorable?" — answered BEFORE anyone needs it to be
 * (okay-persist's Doctor.scala; specs/persist.md, Backup and restore).
 * The recovery scan run offline against a copy, as an INDEPENDENT reader
 * of the documented segment format: a second implementation
 * double-checks the writer instead of inheriting its bugs.
 *
 * The verdict follows recovery's own rule: a torn tail on the LAST
 * segment of a partition is the normal crash artifact — restorable,
 * named; damage anywhere in a CLOSED segment is not.
 */
object Doctor {

  final case class Segment(file: String, topic: String, partition: Int,
                           format: Int, base: Long, frames: Long,
                           lastOffset: Option[Long],
                           damage: Option[String])

  final case class Report(segments: Vector[Segment], problems: Vector[String]) {
    def restorable: Boolean = problems.isEmpty
  }

  def scan(root: Path): Report = {
    val files =
      if (!Files.isDirectory(root)) Vector.empty[Path]
      else {
        val walk = Files.walk(root)
        try walk.iterator.asScala
          .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
          .toVector.sortBy(_.toString)
        finally walk.close()
      }
    val segments = files.map(read(root, _))
    val problems = Vector.newBuilder[String]
    // per partition: only the LAST segment may carry damage (torn tail);
    // offsets must climb across the chain
    for ((_, parts) <- segments.groupBy(s => (s.topic, s.partition))) {
      val ordered = parts.sortBy(_.base)
      for ((s, i) <- ordered.zipWithIndex) {
        val last = i == ordered.length - 1
        s.damage match {
          case Some(d) if !last =>
            problems += s"${s.file}: a CLOSED segment is damaged ($d) — closed segments never change; the copy or the disk lied"
          case Some(d) if s.frames == 0 && s.base >= 0 && d.startsWith("refused") =>
            problems += s"${s.file}: $d"
          case _ => ()
        }
        if (i > 0) {
          val prev = ordered(i - 1)
          prev.lastOffset.foreach { po =>
            if (s.base <= po) problems += s"${s.file}: base ${s.base} does not follow ${prev.file}'s last offset $po"
          }
        }
      }
    }
    // refusals (bad magic, future format) are problems even on a last
    // segment — that is not a torn tail
    segments.foreach { s =>
      s.damage.filter(_.startsWith("refused")).foreach { d =>
        if (!problems.result().exists(_.startsWith(s.file))) problems += s"${s.file}: $d"
      }
    }
    Report(segments, problems.result().distinct)
  }

  /** one segment, read against the documented format */
  private def read(root: Path, path: Path): Segment = {
    val name = root.relativize(path).toString.replace('\\', '/')
    val buf = ByteBuffer.wrap(Files.readAllBytes(path))
    def refused(why: String) = Segment(name, "", -1, -1, -1, 0, None, Some(s"refused: $why"))
    if (buf.remaining < 12) refused("no header")
    else if (buf.getInt != FileStore.Magic) refused("bad magic")
    else {
      val format = buf.getInt
      if (format > FileStore.Format) refused(s"format v$format is from the future")
      else {
        val nameLen = buf.getInt
        if (nameLen < 0 || nameLen > buf.remaining) refused("bad header")
        else {
          val topicBytes = new Array[Byte](nameLen)
          buf.get(topicBytes)
          val topic = new String(topicBytes, "UTF-8")
          if (buf.remaining < 12) refused("bad header")
          else frames(name, topic, buf.getInt, format, buf.getLong, buf)
        }
      }
    }
  }

  /** the frame walk — this reader's own, not the engine's */
  private def frames(name: String, topic: String, partition: Int, format: Int, base: Long, buf: ByteBuffer): Segment = {
    val bodyFixed = if (format >= 2) 20 else 12
    var frames = 0L
    var lastOffset: Option[Long] = None
    var derived = base
    var damage: Option[String] = None
    var go = true
    while (go && buf.remaining >= 8) {
      val len = buf.getInt
      val crc = buf.getInt
      if (len < bodyFixed || len > buf.remaining) {
        damage = Some(s"frame ${frames + 1}: length $len does not fit — torn tail")
        go = false
      } else {
        val body = buf.slice(buf.position, len)
        val c = new CRC32C
        c.update(body.duplicate)
        if (c.getValue.toInt != crc) {
          damage = Some(s"frame ${frames + 1}: CRC mismatch")
          go = false
        } else {
          val offset = if (format >= 2) body.getLong else derived
          if (lastOffset.exists(_ >= offset)) {
            damage = Some(s"frame ${frames + 1}: offset $offset does not climb")
            go = false
          } else {
            frames += 1
            lastOffset = Some(offset)
            derived += 1
            buf.position(buf.position + len)
          }
        }
      }
    }
    if (go && buf.remaining > 0) damage = Some(s"${buf.remaining} trailing bytes after the last frame — torn tail")
    Segment(name, topic, partition, format, base, frames, lastOffset, damage)
  }
}
