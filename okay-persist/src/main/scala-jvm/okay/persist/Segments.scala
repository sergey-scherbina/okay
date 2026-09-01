package okay.persist

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.util.zip.CRC32C

/**
 * A public reader of the documented segment format (specs/
 * persist.md: the disk format is a DOCUMENTED SURFACE, not an
 * internal) — what Doctor reads independently for certification,
 * offered here as a library so a segment's BYTES parse into
 * records wherever they live: a local file, a blob store's copy,
 * a backup under inspection. Total, like every reader in this
 * stack: a torn tail ends the records, a refused header names
 * itself, nothing throws for damage.
 */
object Segments {

  final case class Header(format: Int, topic: String, partition: Int, base: Long)

  /** header + the records that check out; `sound` is false when the
   * bytes END in damage (a torn tail — normal for a crash artifact,
   * notable for a stored copy) */
  final case class Parsed(header: Header, records: Vector[Record], sound: Boolean)

  /** a header this reader must not guess about */
  final case class Refused(why: String) extends RuntimeException(why)

  def parse(bytes: Array[Byte]): Parsed =
    val buf = ByteBuffer.wrap(bytes)
    if buf.remaining < 12 then throw Refused("no header: shorter than magic and version")
    if buf.getInt != 0x4F4B5053 then throw Refused("bad magic: not a segment")
    val format = buf.getInt
    if format > 2 then throw Refused(
      s"segment format v$format; this reader reads up to v2 — refuse rather than guess")
    val nameLen = buf.getInt
    if nameLen < 0 || nameLen > buf.remaining then throw Refused("bad header: name length")
    val name = new Array[Byte](nameLen)
    buf.get(name)
    if buf.remaining < 12 then throw Refused("bad header: truncated")
    val partition = buf.getInt
    val base = buf.getLong
    val header = Header(format, new String(name, UTF_8), partition, base)

    val bodyFixed = if format >= 2 then 20 else 12
    val out = Vector.newBuilder[Record]
    var derived = base
    var sound = true
    var going = true
    while going && buf.remaining >= 8 do
      val mark = buf.position
      val len = buf.getInt
      val crc = buf.getInt
      if len < bodyFixed || len > buf.remaining then { sound = false; going = false }
      else
        val body = buf.slice(buf.position, len)
        val c = new CRC32C
        c.update(body.duplicate)
        if c.getValue.toInt != crc then { sound = false; going = false }
        else
          val offset = if format >= 2 then body.getLong else derived
          val ts = body.getLong
          val keyLen = body.getInt
          if keyLen < 0 || keyLen > len - bodyFixed then { sound = false; going = false }
          else
            val key = new Array[Byte](keyLen)
            body.get(key)
            val value = new Array[Byte](len - bodyFixed - keyLen)
            body.get(value)
            out += Record(offset, ts, key, value)
            buf.position(mark + 8 + len)
            derived += 1
    if buf.remaining > 0 && buf.remaining < 8 then sound = false
    Parsed(header, out.result(), sound)
}
