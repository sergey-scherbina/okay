package okay.persist

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.zip.CRC32C
import scala.jdk.CollectionConverters.*

/**
 * The file engine (specs/persist.md, Storage engine): per partition,
 * append-only SEGMENT files rolled at a size bound, each starting
 * with a self-describing header (magic, format version, topic,
 * partition, base offset), then length-prefixed frames carrying a
 * CRC32C.
 *
 * Recovery scans the LAST segment's frames; a frame whose length or
 * CRC does not check out ENDS the log there and the file is
 * truncated to the last good frame — a torn tail is the normal
 * crash artifact, not an exception (damage truncates rather than
 * throws, the rule everywhere in this stack). Reads validate the
 * same way and stop at damage: total, never a throw. A segment
 * whose header claims a NEWER format is refused loudly — that is
 * not damage, it is a version of this engine that does not exist
 * yet writing where an old one reads.
 *
 * fsync is the `Ack` decision made physical: `Received` returns
 * after the write, `Durable` (and, until replication exists,
 * `Replicated`) after `force`. Retention deletes whole segments
 * from the front and moves `begin`; the active segment is never
 * deleted.
 *
 * Layout: `<root>/<topic>/<partition>/<base offset, 20 digits>.log`.
 * Frame v2: `[len:int][crc:int][offset:long][timestamp:long]
 * [keyLen:int][key][value]` where `len` is the body length (offset
 * onward) and the CRC covers exactly the body. v1 frames had no
 * offset field — the offset was base plus position, which works
 * only while offsets are dense; compaction punches holes, so from
 * v2 the frame says which record it is. The engine writes v2 and
 * reads both; a v1 ACTIVE segment found on recovery is closed and
 * a fresh v2 segment rolled, so no segment ever mixes formats.
 */
object FileStore:
  val Magic = 0x4F4B5053 // "OKPS"
  val Format = 2

  def open(root: Path): FileStore = new FileStore(root)

  private val FrameHeader = 8            // len + crc
  private val BodyFixedV1 = 12           // timestamp + keyLen
  private val BodyFixedV2 = 20           // offset + timestamp + keyLen

  private def crcOf(body: ByteBuffer): Int =
    val c = new CRC32C
    c.update(body)
    c.getValue.toInt

final class FileStore(root: Path) extends Store:
  import FileStore.*

  private final class Segment(val path: Path, val base: Long):
    var size = 0L
    var count = 0L          // maintained for the ACTIVE segment only
    var format = Format     // per segment: v1 segments stay readable

  private final class Part(topicName: String, val partition: Int, policy: Policy):
    val dir: Path = root.resolve(topicName).resolve(partition.toString)
    Files.createDirectories(dir)

    var segments: Vector[Segment] = Vector.empty
    var channel: FileChannel = null       // append channel of the active segment

    private def headerBytes(base: Long): Array[Byte] =
      val topicUtf = topicName.getBytes(UTF_8)
      val b = ByteBuffer.allocate(4 + 4 + 4 + topicUtf.length + 4 + 8)
      b.putInt(Magic).putInt(Format)
      b.putInt(topicUtf.length).put(topicUtf)
      b.putInt(partition).putLong(base)
      b.array()

    /** validates the header; returns (format version, header length).
     * A newer format is refused loudly with the path — the one
     * failure here that is not damage and must not truncate. */
    private def readHeader(buf: ByteBuffer, path: Path): (Int, Int) =
      def refuse(what: String) =
        throw IllegalStateException(s"$path: $what — not a segment of $topicName/$partition")
      if buf.remaining < 12 then refuse("no header")
      if buf.getInt != Magic then refuse("bad magic")
      val v = buf.getInt
      if v > Format then throw IllegalStateException(
        s"$path is segment format v$v; this engine reads up to v$Format — refuse rather than guess")
      val nameLen = buf.getInt
      if nameLen < 0 || nameLen > buf.remaining then refuse("bad header")
      buf.position(buf.position + nameLen)
      if buf.remaining < 12 then refuse("bad header")
      buf.getInt // partition, informational
      buf.getLong // base, authoritative copy is the filename
      (v, buf.position)

    /** walks frames from the buffer's position; calls `f` per valid
     * record (returning false stops early); returns the position
     * after the last VALID frame. v1 frames derive their offset as
     * base plus position (dense by construction); v2 frames carry it. */
    private def scan(buf: ByteBuffer, base: Long, format: Int)
                    (f: (Long, Long, Array[Byte], Array[Byte]) => Boolean): Int =
      val bodyFixed = if format >= 2 then BodyFixedV2 else BodyFixedV1
      var validEnd = buf.position
      var derived = base
      var go = true
      while go && buf.remaining >= FrameHeader do
        val mark = buf.position
        val len = buf.getInt
        val crc = buf.getInt
        if len < bodyFixed || len > buf.remaining then go = false
        else
          val body = buf.slice(buf.position, len)
          if crcOf(body.duplicate) != crc then go = false
          else
            val offset = if format >= 2 then body.getLong else derived
            val ts = body.getLong
            val keyLen = body.getInt
            if keyLen < 0 || keyLen > len - bodyFixed then go = false
            else
              val key = new Array[Byte](keyLen)
              body.get(key)
              val value = new Array[Byte](len - bodyFixed - keyLen)
              body.get(value)
              buf.position(mark + FrameHeader + len)
              validEnd = buf.position
              go = f(offset, ts, key, value)
              derived += 1
      validEnd

    /** one v2 frame, ready to write */
    private def frameOf(offset: Long, ts: Long,
                        key: Array[Byte], value: Array[Byte]): ByteBuffer =
      val body = ByteBuffer.allocate(BodyFixedV2 + key.length + value.length)
      body.putLong(offset).putLong(ts).putInt(key.length).put(key).put(value)
      body.flip()
      val crc = crcOf(body.duplicate)
      val frame = ByteBuffer.allocate(FrameHeader + body.remaining)
      frame.putInt(body.remaining).putInt(crc).put(body).flip()
      frame

    private def mapOf(seg: Segment): ByteBuffer =
      val ch = FileChannel.open(seg.path, StandardOpenOption.READ)
      try ch.map(FileChannel.MapMode.READ_ONLY, 0, seg.size)
      finally ch.close()

    // ── recovery ─────────────────────────────────────────────────
    //
    // LOSING THE CREATE IS NORMAL, and for a while it was fatal.
    // Several processes opening one shared log is the arrangement this
    // module's own two-node story describes, and they all arrive here
    // at once: each lists the directory, each finds no segments, and
    // each calls `newSegment(0)` with CREATE_NEW. One wins; the rest
    // died with FileAlreadyExistsException.
    //
    // Staggered opens never hit it — a few seconds apart and every
    // opener succeeds — which is why it survived: tests and hand-runs
    // open one store at a time. It took two real processes starting
    // together to show it.
    //
    // The answer is not a lock. A segment appearing between the
    // listing and the create is the WINNER'S segment, and the loser's
    // job is to recover from it exactly as it would have recovered
    // from a segment left by a previous run. So: look again. Bounded,
    // because a directory that empties itself between two glances is
    // a different problem and should be reported rather than spun on.
    private def openExisting(attempt: Int = 0): Unit =
      val listing = Files.list(dir)
      val found =
        try listing.iterator.asScala.toVector
          .filter(_.getFileName.toString.endsWith(".log")).sortBy(_.getFileName.toString)
        finally listing.close()
      if found.isEmpty then
        try newSegment(0L)
        catch case _: java.nio.file.FileAlreadyExistsException if attempt < 3 =>
          // the winner has created the file but may not have written
          // its header yet — a window of microseconds that a loaded
          // machine makes real. A reader arriving inside it sees a
          // zero-length file and reports "no header", which is true
          // and is not the answer: the segment is being born. So wait
          // for it, briefly and boundedly, and then recover from it.
          awaitHeader(dir.resolve(f"${0L}%020d.log"))
          openExisting(attempt + 1)
      else
        // and the opener who found the file ALREADY in its listing
        // never attempted a create, so it never waited — it walks
        // straight into the same half-born segment. The wait belongs
        // on the path that READS, not only on the one that lost a
        // create; for a segment that is already whole it is a size
        // check and nothing more.
        awaitHeader(found.last)
        segments = found.map { p =>
          val s = new Segment(p, p.getFileName.toString.stripSuffix(".log").toLong)
          s.size = Files.size(p)
          s
        }
        // headers of the closed segments: validated, versions kept
        segments.init.foreach { s =>
          val ch = FileChannel.open(s.path, StandardOpenOption.READ)
          try s.format =
            readHeader(ch.map(FileChannel.MapMode.READ_ONLY, 0, math.min(s.size, 4096)), s.path)._1
          finally ch.close()
        }
        // the last segment is where a crash lives: count the valid
        // frames, truncate the torn tail, continue appending after it
        val last = segments.last
        val buf = mapOf(last)
        val (v, start) = readHeader(buf, last.path)
        last.format = v
        buf.position(start)
        var n = 0L
        val validEnd = scan(buf, last.base, v) { (_, _, _, _) => n += 1; true }
        channel = FileChannel.open(last.path, StandardOpenOption.WRITE)
        if validEnd < last.size then
          channel.truncate(validEnd)
          channel.force(false)
          last.size = validEnd
        channel.position(last.size)
        last.count = n
        // an active segment in an older format is closed as it stands
        // and a fresh one rolled: no segment ever mixes frame formats
        if v < Format then newSegment(endUnsafe)

    // the recovery runs at construction, exactly where it used to
    openExisting()

    /** a segment that exists but is still empty is one somebody else
     * is writing this instant; anything longer than a moment is a
     * different problem and is left to the reader to report */
    private def awaitHeader(path: Path): Unit =
      val least = headerBytes(0L).length
      val deadline = System.nanoTime() + 2_000_000_000L
      while System.nanoTime() < deadline &&
        (!Files.exists(path) || Files.size(path) < least) do Thread.onSpinWait()

    private def newSegment(base: Long): Unit =
      if channel != null then { channel.force(false); channel.close() }
      val path = dir.resolve(f"$base%020d.log")
      channel = FileChannel.open(path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
      val header = headerBytes(base)
      val buf = ByteBuffer.wrap(header)
      while buf.hasRemaining do channel.write(buf): Unit
      val s = new Segment(path, base)
      s.size = header.length
      segments :+= s

    /** whole segments from the front, never the active one; a
     * compacted topic never retains away — compaction is exclusive
     * with retention (Policy) */
    private def retain(): Unit =
      while !policy.compact &&
        segments.map(_.size).sum > policy.retainBytes && segments.length > 1 do
        Files.delete(segments.head.path)
        segments = segments.tail

    def begin: Long = synchronized(segments.head.base)
    def end: Long = synchronized(endUnsafe)
    // the active segment is dense from its base (appends assign
    // base + count and compaction never touches it), so this holds
    // even after compaction leaves holes in the closed segments
    private def endUnsafe: Long = segments.last.base + segments.last.count

    def append(key: Array[Byte], value: Array[Byte], ack: Ack): Long = synchronized:
      val frameSize = FrameHeader + BodyFixedV2 + key.length + value.length
      val active = segments.last
      if active.size + frameSize > policy.segmentBytes && active.count > 0 then
        newSegment(endUnsafe)
        retain()
      val seg = segments.last
      val off = seg.base + seg.count
      val frame = frameOf(off, System.currentTimeMillis(), key, value)
      while frame.hasRemaining do channel.write(frame): Unit
      if ack != Ack.Received then channel.force(false)
      seg.size += frameSize
      seg.count += 1
      off

    def read(from: Long, max: Int): Topic.Read = synchronized:
      if from < segments.head.base then Topic.Read.TooEarly(segments.head.base)
      else
        val out = Vector.newBuilder[Record]
        var need = max
        var want = from
        for seg <- segments do
          // a closed segment's record count is not tracked; the scan
          // itself is the authority on where it ends
          val pastWant = (seg eq segments.last) && want >= seg.base + seg.count
          if need > 0 && !pastWant then
            val buf = mapOf(seg)
            buf.position(readHeader(buf, seg.path)._2)
            scan(buf, seg.base, seg.format) { (off, ts, k, v) =>
              if off >= want then
                out += Record(off, ts, k, v)
                need -= 1
                want = off + 1
              need > 0
            }: Unit
        Topic.Read.Records(out.result())

    /** keep the latest record per key across the CLOSED segments,
     * atomic-rename shape: survivors to a temporary file, fsync,
     * rename over the head segment, only then delete the superseded
     * ones. A crash in the window leaves segments whose records
     * reads already skip — every survivor carries a later or equal
     * offset than what the leftovers hold, and a read serves
     * offsets monotonically. */
    def compact(): Unit = synchronized:
      if segments.length > 1 then
        val closed = segments.init
        val latest = scala.collection.mutable.LinkedHashMap
          .empty[scala.collection.immutable.ArraySeq[Byte], Record]
        for seg <- closed do
          val buf = mapOf(seg)
          buf.position(readHeader(buf, seg.path)._2)
          scan(buf, seg.base, seg.format) { (off, ts, k, v) =>
            latest(scala.collection.immutable.ArraySeq.unsafeWrapArray(k)) =
              Record(off, ts, k, v)
            true
          }
        val survivors = latest.values.toVector.sortBy(_.offset)
        val head = closed.head
        val tmp = dir.resolve("compact.tmp")   // not *.log: recovery ignores it
        val ch = FileChannel.open(tmp, StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)
        try
          val hdr = ByteBuffer.wrap(headerBytes(head.base))
          while hdr.hasRemaining do ch.write(hdr): Unit
          for r <- survivors do
            val f = frameOf(r.offset, r.timestamp, r.key, r.value)
            while f.hasRemaining do ch.write(f): Unit
          ch.force(false)
        finally ch.close()
        Files.move(tmp, head.path,
          java.nio.file.StandardCopyOption.ATOMIC_MOVE,
          java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        closed.tail.foreach(s => Files.delete(s.path))
        val ns = new Segment(head.path, head.base)
        ns.size = Files.size(head.path)
        segments = ns +: Vector(segments.last)

    def statsOf: Store.PartitionStats = synchronized:
      Store.PartitionStats(partition, segments.head.base, endUnsafe,
        segments.map(_.size).sum, segments.length)

    def close(): Unit = synchronized:
      if channel != null then { channel.force(false); channel.close(); channel = null }

  private final class FileTopic(val name: String, val partitions: Int,
                                policy: Policy) extends Topic:
    val parts: Array[Part] = Array.tabulate(partitions)(new Part(name, _, policy))
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      parts(partition).append(key, value, ack)
    def read(partition: Int, from: Long, max: Int): Topic.Read =
      parts(partition).read(from, max)
    def begin(partition: Int): Long = parts(partition).begin
    def end(partition: Int): Long = parts(partition).end
    def compact(partition: Int): Unit = parts(partition).compact()

  private var byName = Vector.empty[FileTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized:
    byName.find(_.name == name) match
      case Some(t) =>
        if t.partitions != partitions then
          throw IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        // a topic already on disk keeps the partition count it was
        // created with; silently rerouting keys is the one thing an
        // engine must never do to its consumers
        val dir = root.resolve(name)
        val existing =
          if Files.isDirectory(dir) then
            val listing = Files.list(dir)
            try listing.iterator.asScala.count(p => p.getFileName.toString.forall(_.isDigit))
            finally listing.close()
          else 0
        if existing > 0 && existing != partitions then
          throw IllegalArgumentException(
            s"topic $name exists with $existing partitions; asked for $partitions")
        val t = new FileTopic(name, partitions, policy)
        byName :+= t
        t

  def topics: Vector[String] = synchronized:
    val open = byName.map(_.name)
    val onDisk =
      if Files.isDirectory(root) then
        val listing = Files.list(root)
        try listing.iterator.asScala.filter(Files.isDirectory(_))
          .map(_.getFileName.toString).toVector
        finally listing.close()
      else Vector.empty
    (open ++ onDisk.filterNot(open.contains)).sorted

  def stats: Store.Stats = synchronized:
    Store.Stats(byName.map(t => Store.TopicStats(t.name, t.parts.toVector.map(_.statsOf))))

  /** releases the append channels; the store can be reopened on the
   * same root, which is exactly what recovery is */
  def close(): Unit = synchronized:
    byName.foreach(_.parts.foreach(_.close()))
