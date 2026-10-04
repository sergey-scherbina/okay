package okay2.persist

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.zip.CRC32C
import scala.jdk.CollectionConverters._
import scala.annotation.tailrec

/**
 * The file engine (okay-persist's FileStore.scala; specs/persist.md,
 * Storage engine): per partition, append-only SEGMENT files rolled at a
 * size bound, each starting with a self-describing header (magic, format
 * version, topic, partition, base offset), then length-prefixed frames
 * carrying a CRC32C.
 *
 * Recovery scans the LAST segment's frames; a frame whose length or CRC
 * does not check out ENDS the log there and the file is truncated to the
 * last good frame — a torn tail is the normal crash artifact. Reads
 * validate the same way and stop at damage: total, never a throw. A
 * segment whose header claims a NEWER format is refused loudly.
 *
 * fsync is the `Ack` decision made physical: `Received` returns after
 * the write, `Durable` and `Replicated` after `force`. Retention deletes
 * whole segments from the front and moves `begin`; the active segment is
 * never deleted.
 *
 * Layout: `<root>/<topic>/<partition>/<base offset, 20 digits>.log`.
 * Frame v2: `[len:int][crc:int][offset:long][timestamp:long]
 * [keyLen:int][key][value]`, the CRC over exactly the body. v1 frames had
 * no offset field (base plus position); the engine writes v2 and reads
 * both, and a v1 ACTIVE segment found on recovery is closed and a fresh
 * v2 segment rolled, so no segment ever mixes formats. The byte format is
 * the Scala 3 engine's: either reads the other's files.
 */
object FileStore {
  val Magic = 0x4F4B5053 // "OKPS"
  val Format = 2

  def open(root: Path): FileStore = new FileStore(root)

  private val FrameHeader = 8            // len + crc
  private val BodyFixedV1 = 12           // timestamp + keyLen
  private val BodyFixedV2 = 20           // offset + timestamp + keyLen

  private def crcOf(body: ByteBuffer): Int = {
    val c = new CRC32C
    c.update(body)
    c.getValue.toInt
  }

  private def logsIn(dir: Path): Vector[Path] = {
    val listing = Files.list(dir)
    try listing.iterator.asScala.toVector
      .filter(_.getFileName.toString.endsWith(".log")).sortBy(_.getFileName.toString)
    finally listing.close()
  }
}

final class FileStore(root: Path) extends Store {
  import FileStore._

  private final class Segment(val path: Path, val base: Long) {
    var size = 0L
    var count = 0L          // maintained for the ACTIVE segment only
    var format = Format     // per segment: v1 segments stay readable
  }

  private final class Part(topicName: String, val partition: Int, policy: Policy) {
    val dir: Path = root.resolve(topicName).resolve(partition.toString)
    Files.createDirectories(dir)

    var segments: Vector[Segment] = Vector.empty
    var channel: FileChannel = null       // append channel of the active segment

    private def headerBytes(base: Long): Array[Byte] = {
      val topicUtf = topicName.getBytes(UTF_8)
      val b = ByteBuffer.allocate(4 + 4 + 4 + topicUtf.length + 4 + 8)
      b.putInt(Magic).putInt(Format)
      b.putInt(topicUtf.length).put(topicUtf)
      b.putInt(partition).putLong(base)
      b.array()
    }

    /** validates the header; returns (format version, header length). A
     * newer format is refused loudly with the path — the one failure here
     * that is not damage and must not truncate */
    private def readHeader(buf: ByteBuffer, path: Path): (Int, Int) = {
      def refuse(what: String) =
        throw new IllegalStateException(s"$path: $what — not a segment of $topicName/$partition")
      if (buf.remaining < 12) refuse("no header")
      if (buf.getInt != Magic) refuse("bad magic")
      val v = buf.getInt
      if (v > Format) throw new IllegalStateException(
        s"$path is segment format v$v; this engine reads up to v$Format — refuse rather than guess")
      val nameLen = buf.getInt
      if (nameLen < 0 || nameLen > buf.remaining) refuse("bad header")
      buf.position(buf.position + nameLen)
      if (buf.remaining < 12) refuse("bad header")
      buf.getInt // partition, informational
      buf.getLong // base, authoritative copy is the filename
      (v, buf.position)
    }

    /** walks frames from the buffer's position; calls `f` per valid
     * record (returning false stops early); returns the position after
     * the last VALID frame. v1 frames derive their offset as base plus
     * position (dense by construction); v2 frames carry it */
    private def scan(buf: ByteBuffer, base: Long, format: Int)
                    (f: (Long, Long, Array[Byte], Array[Byte]) => Boolean): Int = {
      val bodyFixed = if (format >= 2) BodyFixedV2 else BodyFixedV1
      var validEnd = buf.position
      var derived = base
      var go = true
      while (go && buf.remaining >= FrameHeader) {
        val mark = buf.position
        val len = buf.getInt
        val crc = buf.getInt
        if (len < bodyFixed || len > buf.remaining) go = false
        else {
          val body = buf.slice(buf.position, len)
          if (crcOf(body.duplicate) != crc) go = false
          else {
            val offset = if (format >= 2) body.getLong else derived
            val ts = body.getLong
            val keyLen = body.getInt
            if (keyLen < 0 || keyLen > len - bodyFixed) go = false
            else {
              val key = new Array[Byte](keyLen)
              body.get(key)
              val value = new Array[Byte](len - bodyFixed - keyLen)
              body.get(value)
              buf.position(mark + FrameHeader + len)
              validEnd = buf.position
              go = f(offset, ts, key, value)
              derived += 1
            }
          }
        }
      }
      validEnd
    }

    /** one v2 frame, ready to write */
    private def frameOf(offset: Long, ts: Long, key: Array[Byte], value: Array[Byte]): ByteBuffer = {
      val body = ByteBuffer.allocate(BodyFixedV2 + key.length + value.length)
      body.putLong(offset).putLong(ts).putInt(key.length).put(key).put(value)
      body.flip()
      val crc = crcOf(body.duplicate)
      val frame = ByteBuffer.allocate(FrameHeader + body.remaining)
      frame.putInt(body.remaining).putInt(crc).put(body).flip()
      frame
    }

    private def mapOf(seg: Segment): ByteBuffer = {
      val ch = FileChannel.open(seg.path, StandardOpenOption.READ)
      try ch.map(FileChannel.MapMode.READ_ONLY, 0, seg.size)
      finally ch.close()
    }

    private def writeAll(ch: FileChannel, buf: ByteBuffer): Unit =
      while (buf.hasRemaining) { val _ = ch.write(buf) }

    // ── recovery ──────────────────────────────────────────────────
    //
    // LOSING THE CREATE IS NORMAL. Several processes opening one shared
    // log all list the directory, find no segments, and create
    // `newSegment(0)` with CREATE_NEW; one wins. A segment appearing
    // between the listing and the create is the WINNER'S, and the
    // loser recovers from it as from a previous run's: look again,
    // bounded (okay-persist's FileStore keeps the whole story).
    @tailrec private def openExisting(attempt: Int = 0): Unit = {
      val found = logsIn(dir)
      if (found.isEmpty) {
        val lost =
          try { newSegment(0L); false }
          catch {
            case _: java.nio.file.FileAlreadyExistsException if attempt < 3 =>
              // the winner may not have written its header yet
              awaitHeader(dir.resolve(f"${0L}%020d.log"))
              true
          }
        if (lost) openExisting(attempt + 1)
      } else {
        // an opener who found the file in its listing never attempted a
        // create — it waits for a half-born segment the same way
        awaitHeader(found.last)
        segments = found.map { p =>
          val s = new Segment(p, p.getFileName.toString.stripSuffix(".log").toLong)
          s.size = Files.size(p)
          s
        }
        // headers of the closed segments: validated, versions kept
        segments.init.foreach { s =>
          val ch = FileChannel.open(s.path, StandardOpenOption.READ)
          try s.format = readHeader(ch.map(FileChannel.MapMode.READ_ONLY, 0, math.min(s.size, 4096)), s.path)._1
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
        if (validEnd < last.size) {
          channel.truncate(validEnd.toLong)
          channel.force(false)
          last.size = validEnd.toLong
        }
        channel.position(last.size)
        last.count = n
        // an active segment in an older format is closed as it stands
        // and a fresh one rolled: no segment ever mixes frame formats
        if (v < Format) newSegment(endUnsafe)
      }
    }

    openExisting()

    /** a segment that exists but is still empty is one somebody else is
     * writing this instant; anything longer than a moment is left to the
     * reader to report */
    private def awaitHeader(path: Path): Unit = {
      val least = headerBytes(0L).length
      val deadline = System.nanoTime() + 2000000000L
      while (System.nanoTime() < deadline && (!Files.exists(path) || Files.size(path) < least)) Thread.onSpinWait()
    }

    private def newSegment(base: Long): Unit = {
      if (channel != null) { channel.force(false); channel.close() }
      val path = dir.resolve(f"$base%020d.log")
      channel = FileChannel.open(path, StandardOpenOption.CREATE_NEW, StandardOpenOption.WRITE)
      val header = headerBytes(base)
      writeAll(channel, ByteBuffer.wrap(header))
      val s = new Segment(path, base)
      s.size = header.length.toLong
      segments :+= s
    }

    /** whole segments from the front, never the active one; a compacted
     * topic never retains away (Policy) */
    private def retain(): Unit =
      while (!policy.compact && segments.map(_.size).sum > policy.retainBytes && segments.length > 1) {
        Files.delete(segments.head.path)
        segments = segments.tail
      }

    /**
     * WHAT THE FILES SAY NOW, for a handle that did not write them: the
     * end of the active segment is a property of the FILE, so ask the
     * file how long it is and scan on from the last valid end — the
     * scan's length and CRC check is recovery's own authority, so a
     * half-written record at the tail is simply not there yet. `deep`
     * also looks at the DIRECTORY, for a segment the writer rolled since
     * and for segments another handle's retention deleted.
     */
    private def refresh(deep: Boolean): Unit = {
      val last = segments.last
      val len = try Files.size(last.path) catch { case _: Throwable => last.size }
      if (len > last.size) {
        val ch = FileChannel.open(last.path, StandardOpenOption.READ)
        try {
          val buf = ch.map(FileChannel.MapMode.READ_ONLY, 0, len)
          buf.position(last.size.toInt)
          var found = 0L
          val validEnd = scan(buf, last.base + last.count, last.format) { (_, _, _, _) => found += 1; true }
          if (found > 0) {
            last.size = validEnd.toLong
            last.count += found
          }
        } finally ch.close()
      }
      if (deep) {
        val found = logsIn(dir)
        val known = segments.map(_.path.getFileName.toString).toSet
        val rolled = found.filterNot(p => known(p.getFileName.toString))
        if (rolled.nonEmpty) {
          rolled.foreach { p =>
            awaitHeader(p)
            val seg = new Segment(p, p.getFileName.toString.stripSuffix(".log").toLong)
            seg.size = Files.size(p)
            val ch = FileChannel.open(p, StandardOpenOption.READ)
            try {
              val buf = ch.map(FileChannel.MapMode.READ_ONLY, 0, seg.size)
              val (v, start) = readHeader(buf, p)
              seg.format = v
              buf.position(start)
              var n = 0L
              seg.size = scan(buf, seg.base, v) { (_, _, _, _) => n += 1; true }.toLong
              seg.count = n
            } finally ch.close()
            segments :+= seg
          }
          segments = segments.sortBy(_.base)
          // a handle that also WRITES has been overtaken: follow the roll
          if (channel != null) {
            try channel.close() catch { case _: Throwable => () }
            channel = FileChannel.open(segments.last.path, StandardOpenOption.WRITE)
            val _ = channel.position(segments.last.size)
          }
        }
        // and another handle's RETENTION deletes segments from the
        // FRONT: the directory decides, the list is a cache of it
        val alive = found.map(_.getFileName.toString).toSet
        val kept = segments.filter(s => alive(s.path.getFileName.toString))
        if (kept.nonEmpty && kept.length != segments.length) segments = kept
      }
    }

    def begin: Long = synchronized(segments.head.base)
    def end: Long = synchronized { refresh(deep = true); endUnsafe }
    // the active segment is dense from its base, so this holds even
    // after compaction leaves holes in the closed segments
    private def endUnsafe: Long = segments.last.base + segments.last.count

    def append(key: Array[Byte], value: Array[Byte], ack: Ack): Long = synchronized {
      val frameSize = FrameHeader + BodyFixedV2 + key.length + value.length
      val active = segments.last
      if (active.size + frameSize > policy.segmentBytes && active.count > 0) {
        newSegment(endUnsafe)
        retain()
      }
      val seg = segments.last
      val off = seg.base + seg.count
      writeAll(channel, frameOf(off, System.currentTimeMillis(), key, value))
      if (ack != Ack.Received) channel.force(false)
      seg.size += frameSize
      seg.count += 1
      off
    }

    def read(from: Long, max: Int): Topic.Read = synchronized {
      refresh(deep = false)
      if (from >= endUnsafe) refresh(deep = true)
      if (from < segments.head.base) Topic.Read.TooEarly(segments.head.base)
      else {
        val out = Vector.newBuilder[Record]
        var need = max
        var want = from
        // a segment file can also vanish BETWEEN the refresh and the map
        var vanished = false
        for (seg <- segments) {
          val pastWant = (seg eq segments.last) && want >= seg.base + seg.count
          if (need > 0 && !pastWant && !vanished) {
            try {
              val buf = mapOf(seg)
              buf.position(readHeader(buf, seg.path)._2)
              val _ = scan(buf, seg.base, seg.format) { (off, ts, k, v) =>
                if (off >= want) {
                  out += Record(off, ts, k, v)
                  need -= 1
                  want = off + 1
                }
                need > 0
              }
            } catch {
              case _: java.nio.file.NoSuchFileException => vanished = true
            }
          }
        }
        val got = out.result()
        if (vanished) {
          refresh(deep = true)
          if (got.isEmpty) Topic.Read.TooEarly(segments.head.base) else Topic.Read.Records(got)
        } else Topic.Read.Records(got)
      }
    }

    /** keep the latest record per key across the CLOSED segments,
     * atomic-rename shape: survivors to a temporary file, fsync, rename
     * over the head segment, only then delete the superseded ones */
    def compact(): Unit = synchronized {
      if (segments.length > 1) {
        val closed = segments.init
        val latest = scala.collection.mutable.LinkedHashMap.empty[scala.collection.immutable.ArraySeq[Byte], Record]
        for (seg <- closed) {
          val buf = mapOf(seg)
          buf.position(readHeader(buf, seg.path)._2)
          val _ = scan(buf, seg.base, seg.format) { (off, ts, k, v) =>
            latest(scala.collection.immutable.ArraySeq.unsafeWrapArray(k)) = Record(off, ts, k, v)
            true
          }
        }
        val survivors = latest.values.toVector.sortBy(_.offset)
        val head = closed.head
        val tmp = dir.resolve("compact.tmp")   // not *.log: recovery ignores it
        val ch = FileChannel.open(tmp, StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE)
        try {
          writeAll(ch, ByteBuffer.wrap(headerBytes(head.base)))
          for (r <- survivors) writeAll(ch, frameOf(r.offset, r.timestamp, r.key, r.value))
          ch.force(false)
        } finally ch.close()
        Files.move(tmp, head.path,
          java.nio.file.StandardCopyOption.ATOMIC_MOVE,
          java.nio.file.StandardCopyOption.REPLACE_EXISTING)
        closed.tail.foreach(s => Files.delete(s.path))
        val ns = new Segment(head.path, head.base)
        ns.size = Files.size(head.path)
        segments = ns +: Vector(segments.last)
      }
    }

    def statsOf: Store.PartitionStats = synchronized {
      Store.PartitionStats(partition, segments.head.base, endUnsafe, segments.map(_.size).sum, segments.length)
    }

    def close(): Unit = synchronized {
      if (channel != null) { channel.force(false); channel.close(); channel = null }
    }
  }

  private final class FileTopic(val name: String, val partitions: Int, policy: Policy) extends Topic {
    val parts: Array[Part] = Array.tabulate(partitions)(new Part(name, _, policy))
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      parts(partition).append(key, value, ack)
    def read(partition: Int, from: Long, max: Int): Topic.Read = parts(partition).read(from, max)
    def begin(partition: Int): Long = parts(partition).begin
    def end(partition: Int): Long = parts(partition).end
    def compact(partition: Int): Unit = parts(partition).compact()
  }

  private var byName = Vector.empty[FileTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized {
    byName.find(_.name == name) match {
      case Some(t) =>
        if (t.partitions != partitions)
          throw new IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        // a topic already on disk keeps the partition count it was
        // created with
        val dir = root.resolve(name)
        val existing =
          if (Files.isDirectory(dir)) {
            val listing = Files.list(dir)
            try listing.iterator.asScala.count(p => p.getFileName.toString.forall(_.isDigit))
            finally listing.close()
          } else 0
        if (existing > 0 && existing != partitions)
          throw new IllegalArgumentException(s"topic $name exists with $existing partitions; asked for $partitions")
        val t = new FileTopic(name, partitions, policy)
        byName :+= t
        t
    }
  }

  def topics: Vector[String] = synchronized {
    val open = byName.map(_.name)
    val onDisk =
      if (Files.isDirectory(root)) {
        val listing = Files.list(root)
        try listing.iterator.asScala.filter(Files.isDirectory(_)).map(_.getFileName.toString).toVector
        finally listing.close()
      } else Vector.empty
    (open ++ onDisk.filterNot(open.contains)).sorted
  }

  def stats: Store.Stats = synchronized {
    Store.Stats(byName.map(t => Store.TopicStats(t.name, t.parts.toVector.map(_.statsOf))))
  }

  /** releases the append channels; the store can be reopened on the
   * same root, which is exactly what recovery is */
  def close(): Unit = synchronized {
    byName.foreach(_.parts.foreach(_.close()))
  }
}
