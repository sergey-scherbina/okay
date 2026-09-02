package okay.persist

import java.nio.file.{Files, Path, StandardOpenOption}
import java.nio.ByteBuffer
import scala.jdk.CollectionConverters.*

/**
 * The file engine under the shared contract, then the tests that are
 * the REASON the engine can be trusted (specs/persist.md: the crash
 * cases are not extras): recovery, the torn tail, dense offsets over
 * restart, format-version refusal, retention by whole segments.
 */
class TestFileStore extends StoreSuite:
  private var dirs = List.empty[Path]

  private def tmp(): Path =
    val d = Files.createTempDirectory("okay-persist-test")
    dirs ::= d
    d

  override def afterAll(): Unit =
    def wipe(p: Path): Unit =
      if Files.isDirectory(p) then
        val l = Files.list(p)
        try l.forEach(wipe) finally l.close()
      Files.deleteIfExists(p): Unit
    dirs.foreach(wipe)

  def mkStore(): Store = FileStore.open(tmp())
  // frames here are ~48 bytes; tiny segments so retention has whole
  // segments to drop
  def tinyRetention: Policy = Policy(segmentBytes = 160, retainBytes = 700)
  // and so compaction has CLOSED segments to rewrite
  override def tinyCompact: Policy =
    Policy(compact = true, retainBytes = 100, segmentBytes = 200)

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  private def records(r: Topic.Read): Vector[Record] = r match
    case Topic.Read.Records(rs) => rs
    case Topic.Read.TooEarly(b) => fail(s"unexpected TooEarly($b)")

  private def segmentsOf(root: Path, topic: String, partition: Int = 0): Vector[Path] =
    val dir = root.resolve(topic).resolve(partition.toString)
    val l = Files.list(dir)
    try l.iterator.asScala.toVector.sortBy(_.getFileName.toString)
    finally l.close()

  private def lastSegment(root: Path, topic: String): Path = segmentsOf(root, topic).last

  test("reopen: records intact, offsets continue densely") {
    val root = tmp()
    val s1 = FileStore.open(root)
    val t1 = s1.topic("journal")
    (0 until 5).foreach(i => t1.append(0, bytes(s"k$i"), bytes(s"v$i"), Ack.Durable))
    s1.close()

    val s2 = FileStore.open(root)
    val t2 = s2.topic("journal")
    assertEquals(t2.end(0), 5L)
    assertEquals(records(t2.read(0, 0L, 100)).map(r => str(r.value)),
      (0 until 5).map(i => s"v$i").toVector)
    assertEquals(t2.append(0, bytes("k5"), bytes("v5"), Ack.Durable), 5L)
    s2.close()
  }

  test("torn tail: a partial frame truncates on recovery, earlier records survive") {
    val root = tmp()
    val s1 = FileStore.open(root)
    val t1 = s1.topic("journal")
    (0 until 3).foreach(i => t1.append(0, Array.empty, bytes(s"v$i"), Ack.Durable))
    s1.close()

    // the crash artifact: half a frame at the tail
    val seg = lastSegment(root, "journal")
    Files.write(seg, Array[Byte](0, 0, 0, 42, 1, 2, 3), StandardOpenOption.APPEND)
    val torn = Files.size(seg)

    val s2 = FileStore.open(root)
    val t2 = s2.topic("journal")
    assertEquals(t2.end(0), 3L)
    assertEquals(records(t2.read(0, 0L, 100)).map(r => str(r.value)), Vector("v0", "v1", "v2"))
    assert(Files.size(seg) < torn, "the torn tail was not truncated")
    // the next append reuses the never-acknowledged position, densely
    assertEquals(t2.append(0, Array.empty, bytes("v3"), Ack.Durable), 3L)
    assertEquals(records(t2.read(0, 3L, 10)).map(r => str(r.value)), Vector("v3"))
    s2.close()
  }

  test("corrupt CRC in the last frame: the log ends at the damage") {
    val root = tmp()
    val s1 = FileStore.open(root)
    val t1 = s1.topic("journal")
    (0 until 3).foreach(i => t1.append(0, Array.empty, bytes(s"v$i"), Ack.Durable))
    s1.close()

    // flip one byte in the last frame's body
    val seg = lastSegment(root, "journal")
    val all = Files.readAllBytes(seg)
    all(all.length - 1) = (all(all.length - 1) ^ 0xff).toByte
    Files.write(seg, all)

    val s2 = FileStore.open(root)
    val t2 = s2.topic("journal")
    assertEquals(t2.end(0), 2L)
    assertEquals(records(t2.read(0, 0L, 100)).map(r => str(r.value)), Vector("v0", "v1"))
    s2.close()
  }

  test("a newer segment format is refused loudly, naming the file") {
    val root = tmp()
    val s1 = FileStore.open(root)
    s1.topic("journal").append(0, Array.empty, bytes("v0"), Ack.Durable): Unit
    s1.close()

    // forge the header's format version to one that does not exist
    val seg = lastSegment(root, "journal")
    val all = Files.readAllBytes(seg)
    ByteBuffer.wrap(all).putInt(4, FileStore.Format + 1)
    Files.write(seg, all)

    val e = intercept[IllegalStateException](FileStore.open(root).topic("journal"))
    assert(e.getMessage.contains(seg.getFileName.toString), e.getMessage)
    assert(e.getMessage.contains(s"v${FileStore.Format + 1}"), e.getMessage)
  }

  test("segments roll at the size bound and retention deletes whole files") {
    val root = tmp()
    val s = FileStore.open(root)
    val t = s.topic("bounded", partitions = 1, policy = tinyRetention)
    (0 until 50).foreach(i => t.append(0, Array.empty, bytes(s"payload-$i"), Ack.Durable))
    val segs = segmentsOf(root, "bounded")
    assert(segs.length > 1, "segments never rolled")
    val b = t.begin(0)
    assert(b > 0L)
    assertEquals(segs.head.getFileName.toString, f"$b%020d.log",
      "begin must be the base of the oldest surviving segment")
    s.close()
  }

  test("reads spanning several segments come back in one piece") {
    val root = tmp()
    val s = FileStore.open(root)
    val t = s.topic("long", partitions = 1, policy = Policy(segmentBytes = 160))
    (0 until 30).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Durable))
    assert(segmentsOf(root, "long").length > 1)
    assertEquals(records(t.read(0, 0L, 1000)).map(_.offset), (0L until 30L).toVector)
    assertEquals(records(t.read(0, 7L, 9)).map(_.offset), (7L until 16L).toVector)
    s.close()
  }

  test("a v1 segment written by the old engine reads under the v2 engine") {
    // forge what the v1 engine wrote: same header layout, frames
    // WITHOUT the offset field — offsets were base plus position
    val root = tmp()
    val dir = root.resolve("old").resolve("0")
    Files.createDirectories(dir)
    val topicUtf = "old".getBytes("UTF-8")
    val buf = ByteBuffer.allocate(4096)
    buf.putInt(FileStore.Magic).putInt(1)
    buf.putInt(topicUtf.length).put(topicUtf).putInt(0).putLong(0L)
    for i <- 0 until 3 do
      val key = bytes(s"k$i"); val value = bytes(s"v$i")
      val body = ByteBuffer.allocate(12 + key.length + value.length)
      body.putLong(1000L + i).putInt(key.length).put(key).put(value)
      body.flip()
      val c = new java.util.zip.CRC32C
      c.update(body.duplicate)
      buf.putInt(body.remaining).putInt(c.getValue.toInt).put(body)
    buf.flip()
    Files.write(dir.resolve(f"${0L}%020d.log"),
      java.util.Arrays.copyOf(buf.array, buf.limit))

    val s = FileStore.open(root)
    val t = s.topic("old")
    assertEquals(t.end(0), 3L)
    assertEquals(records(t.read(0, 0L, 10)).map(r => (r.offset, str(r.key), str(r.value))),
      Vector((0L, "k0", "v0"), (1L, "k1", "v1"), (2L, "k2", "v2")))
    // the v1 active segment is closed as it stands; appends continue
    // densely in a fresh v2 segment — no segment mixes formats
    assertEquals(t.append(0, bytes("k3"), bytes("v3"), Ack.Durable), 3L)
    assertEquals(records(t.read(0, 0L, 10)).map(_.offset), (0L until 4L).toVector)
    assertEquals(segmentsOf(root, "old").length, 2)
    s.close()
  }

  test("compaction on disk: fewer files, same survivors after reopen") {
    val root = tmp()
    val policy = Policy(compact = true, segmentBytes = 200)
    val s1 = FileStore.open(root)
    val t1 = s1.topic("kv", partitions = 1, policy = policy)
    val keys = Vector("a", "b", "c")
    for i <- 0 until 30 do t1.append(0, bytes(keys(i % 3)), bytes(s"$i"), Ack.Durable)
    val filesBefore = segmentsOf(root, "kv").length
    t1.compact(0)
    assert(segmentsOf(root, "kv").length < filesBefore, "no segment file was reclaimed")
    val after = records(t1.read(0, 0L, 100)).map(r => (r.offset, str(r.key), str(r.value)))
    s1.close()

    // recovery reads the compacted head like any other segment
    val s2 = FileStore.open(root)
    val t2 = s2.topic("kv", partitions = 1, policy = policy)
    assertEquals(records(t2.read(0, 0L, 100)).map(r => (r.offset, str(r.key), str(r.value))), after)
    assertEquals(t2.end(0), 30L)
    assertEquals(t2.append(0, bytes("d"), bytes("30"), Ack.Durable), 30L)
    s2.close()
  }

  test("reopen sees topics on disk without re-declaring them") {
    val root = tmp()
    val s1 = FileStore.open(root)
    s1.topic("a").append(0, Array.empty, bytes("x"), Ack.Durable): Unit
    s1.topic("b").append(0, Array.empty, bytes("y"), Ack.Durable): Unit
    s1.close()
    assertEquals(FileStore.open(root).topics, Vector("a", "b"))
  }
