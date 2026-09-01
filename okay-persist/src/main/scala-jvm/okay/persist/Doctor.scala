package okay.persist

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}
import java.util.zip.CRC32C
import scala.jdk.CollectionConverters.*

/**
 * "Is this backup restorable?" — answered BEFORE anyone needs it to
 * be (specs/persist.md, Backup and restore). The doctor runs the
 * recovery scan offline, against a copy, and deliberately as an
 * INDEPENDENT reader of the documented segment format (magic,
 * version, topic, partition, base; length-prefixed CRC32C frames):
 * a second implementation double-checks the writer instead of
 * inheriting its bugs.
 *
 * The verdict follows recovery's own rule: a torn tail on the LAST
 * segment of a partition is the normal crash artifact — restorable,
 * named; damage anywhere in a CLOSED segment is not — closed
 * segments never change, so a bad frame there means the copy (or
 * the disk) lied.
 */
object Doctor {

  final case class Segment(file: String, topic: String, partition: Int,
                           format: Int, base: Long, frames: Long,
                           lastOffset: Option[Long],
                           damage: Option[String])

  final case class Report(segments: Vector[Segment], problems: Vector[String]):
    def restorable: Boolean = problems.isEmpty

  def scan(root: Path): Report =
    val files = if !Files.isDirectory(root) then Vector.empty
      else Files.walk(root).iterator.asScala
        .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
        .toVector.sortBy(_.toString)
    val segments = files.map(read(root, _))
    val problems = Vector.newBuilder[String]
    // per partition: only the LAST segment may carry damage (torn
    // tail); offsets must climb across the chain
    for ((_, _), parts) <- segments.groupBy(s => (s.topic, s.partition)) do
      val ordered = parts.sortBy(_.base)
      for (s, i) <- ordered.zipWithIndex do
        val last = i == ordered.length - 1
        s.damage match
          case Some(d) if !last =>
            problems += s"${s.file}: a CLOSED segment is damaged ($d) — closed segments never change; the copy or the disk lied"
          case Some(d) if s.frames == 0 && s.base >= 0 && d.startsWith("refused") =>
            problems += s"${s.file}: $d"
          case _ => ()
        if i > 0 then
          val prev = ordered(i - 1)
          prev.lastOffset.foreach { po =>
            if s.base <= po then
              problems += s"${s.file}: base ${s.base} does not follow ${prev.file}'s last offset $po"
          }
    segments.filter(s => s.damage.exists(_.startsWith("refused"))).foreach { s =>
      // refusals (bad magic, future format) are problems even on a
      // last segment — that is not a torn tail
      if !problems.result().exists(_.startsWith(s.file)) then
        problems += s"${s.file}: ${s.damage.get}"
    }
    Report(segments, problems.result().distinct)

  /** one segment, read against the documented format */
  private def read(root: Path, path: Path): Segment =
    val name = root.relativize(path).toString.replace('\\', '/')
    val bytes = Files.readAllBytes(path)
    val buf = ByteBuffer.wrap(bytes)
    def refused(why: String) =
      Segment(name, "", -1, -1, -1, 0, None, Some(s"refused: $why"))
    if buf.remaining < 12 then return refused("no header")
    if buf.getInt != FileStore.Magic then return refused("bad magic")
    val format = buf.getInt
    if format > FileStore.Format then return refused(s"format v$format is from the future")
    val nameLen = buf.getInt
    if nameLen < 0 || nameLen > buf.remaining then return refused("bad header")
    val topicBytes = new Array[Byte](nameLen); buf.get(topicBytes)
    val topic = String(topicBytes, "UTF-8")
    if buf.remaining < 12 then return refused("bad header")
    val partition = buf.getInt
    val base = buf.getLong
    // the frame walk — this reader's own, not the engine's
    val bodyFixed = if format >= 2 then 20 else 12
    var frames = 0L
    var lastOffset: Option[Long] = None
    var derived = base
    var damage: Option[String] = None
    var go = true
    while go && buf.remaining >= 8 do
      val len = buf.getInt
      val crc = buf.getInt
      if len < bodyFixed || len > buf.remaining then
        damage = Some(s"frame ${frames + 1}: length $len does not fit — torn tail")
        go = false
      else
        val body = buf.slice(buf.position, len)
        val c = new CRC32C; c.update(body.duplicate)
        if c.getValue.toInt != crc then
          damage = Some(s"frame ${frames + 1}: CRC mismatch")
          go = false
        else
          val offset = if format >= 2 then body.getLong else derived
          if lastOffset.exists(_ >= offset) then
            damage = Some(s"frame ${frames + 1}: offset $offset does not climb")
            go = false
          else
            frames += 1
            lastOffset = Some(offset)
            derived += 1
            buf.position(buf.position + len)
    if go && buf.remaining > 0 then
      damage = Some(s"${buf.remaining} trailing bytes after the last frame — torn tail")
    Segment(name, topic, partition, format, base, frames, lastOffset, damage)
}
