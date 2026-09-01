package okay.persist

import java.nio.file.{Files, Path}

/**
 * The doctor's verdicts, argued from real files: a healthy store
 * scans clean, a torn tail on the LAST segment is normal and named,
 * damage in a CLOSED segment condemns the copy, and refusals (bad
 * magic, future formats) are never mistaken for tails.
 */
class TestDoctor extends munit.FunSuite {

  def store(segmentBytes: Long = 256): (Path, Topic) =
    val root = Files.createTempDirectory("okay-doctor")
    val topic = FileStore.open(root).topic("events", 1, Policy(segmentBytes = segmentBytes))
    (root, topic)

  def fill(t: Topic, n: Int, from: Int = 0): Unit =
    for i <- from until (from + n) do
      val _ = t.append(0, s"k$i".getBytes, s"value-$i-padding-to-make-frames-realistic".getBytes, Ack.Durable)

  def segFiles(root: Path): Vector[Path] =
    import scala.jdk.CollectionConverters.*
    Files.walk(root).iterator.asScala
      .filter(p => p.getFileName.toString.endsWith(".log")).toVector.sortBy(_.toString)

  test("a healthy multi-segment store is restorable; frames and offsets accounted") {
    val (root, t) = store()
    fill(t, 40)
    assert(segFiles(root).size > 1, "the tiny segment bound should have rolled")
    val r = Doctor.scan(root)
    assert(r.restorable, r.problems.toString)
    assertEquals(r.segments.map(_.frames).sum, 40L)
    assertEquals(r.segments.flatMap(_.lastOffset).max, 39L)
    assertEquals(r.segments.map(_.topic).distinct, Vector("events"))
  }

  test("a torn tail on the LAST segment is normal: named, still restorable") {
    val (root, t) = store()
    fill(t, 40)
    val last = segFiles(root).last
    val bytes = Files.readAllBytes(last)
    Files.write(last, bytes.dropRight(7))   // mid-frame
    val r = Doctor.scan(root)
    assert(r.restorable, r.problems.toString)
    val damaged = r.segments.filter(_.damage.nonEmpty)
    assertEquals(damaged.length, 1)
    assert(damaged.head.damage.get.contains("torn tail")
      || damaged.head.damage.get.contains("CRC"), damaged.head.damage.get)
  }

  test("damage in a CLOSED segment condemns the copy, naming the file") {
    val (root, t) = store()
    fill(t, 40)
    val closed = segFiles(root).head
    val bytes = Files.readAllBytes(closed)
    bytes(bytes.length / 2) = (bytes(bytes.length / 2) ^ 0x40).toByte   // flip one bit mid-file
    Files.write(closed, bytes)
    val r = Doctor.scan(root)
    assert(!r.restorable)
    assert(r.problems.exists(p => p.contains(closed.getFileName.toString.take(8))
      || p.contains("CLOSED")), r.problems.toString)
  }

  test("bad magic and a future format refuse — never mistaken for a tail") {
    val (root, t) = store()
    fill(t, 5)
    val f = segFiles(root).head
    // bad magic
    val garbage = root.resolve("events/0/99999999999999999999.log")
    Files.write(garbage, "not a segment at all".getBytes)
    val r = Doctor.scan(root)
    assert(!r.restorable)
    assert(r.problems.exists(_.contains("bad magic")), r.problems.toString)
    Files.delete(garbage)
    // future format
    val bytes = Files.readAllBytes(f)
    val bb = java.nio.ByteBuffer.wrap(bytes)
    bb.putInt(4, 99)   // format field
    Files.write(f, bytes)
    val r2 = Doctor.scan(root)
    assert(!r2.restorable)
    assert(r2.problems.exists(_.contains("future")), r2.problems.toString)
  }

  test("the doctor is an INDEPENDENT reader: it counts what the engine wrote") {
    val (root, t) = store(segmentBytes = 64 * 1024)
    fill(t, 100)
    val r = Doctor.scan(root)
    assert(r.restorable)
    assertEquals(r.segments.map(_.frames).sum, 100L)
    // and the engine agrees with the doctor on where the log ends
    assertEquals(t.end(0), r.segments.flatMap(_.lastOffset).max + 1)
  }
}
