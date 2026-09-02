package okay.blob

import okay.{!, +, Async, Produce}
import okay.given
import okay.persist.{Ack, FileStore, Policy, Segments}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The cold tail becomes the lake (specs/persist.md, stage 3):
 * verified-then-evict under a local budget, and the tiered read
 * serving blob history + local tail byte-exact — TooEarly stops
 * meaning "gone" and starts meaning "cold".
 */
class TestOffload extends munit.FunSuite:

  private var dirs = List.empty[Path]
  private def tmp(): Path =
    val d = Files.createTempDirectory("okay-offload")
    dirs ::= d
    d
  override def afterAll(): Unit =
    def wipe(p: Path): Unit =
      if Files.isDirectory(p) then
        val l = Files.list(p)
        try l.forEach(wipe) finally l.close()
      Files.deleteIfExists(p)
      ()
    dirs.foreach(wipe)

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")
  def run[A](prog: A ! Async): A = !.run(Async.run[A, Nothing](prog))

  def logsUnder(root: Path): Vector[Path] =
    Files.walk(root).iterator.asScala
      .filter(p => Files.isRegularFile(p) && p.getFileName.toString.endsWith(".log"))
      .toVector

  /** a store with rolled segments, closed and ready for ops */
  def seeded(root: Path, n: Int = 40): Unit =
    val s = FileStore.open(root)
    val t = s.topic("events", 1, Policy(segmentBytes = 200))
    (0 until n).foreach(i => t.append(0, bytes(s"k$i"), bytes(s"v$i"), Ack.Durable))
    s.close()

  test("evict deletes ONLY blob-verified closed segments, oldest first, down to the budget") {
    val root = tmp(); val bl = Fs(tmp())
    seeded(root)
    val before = logsUnder(root).length
    assert(before > 3, s"only $before segments — the fixture did not roll")

    // without a backup, nothing is eligible: the budget stays
    // exceeded rather than data leaving unsafely
    assertEquals(run(Offload.evict(root, bl, keepLocalBytes = 0L)), Vector.empty)
    assertEquals(logsUnder(root).length, before)

    run(Backup.copy(root, bl)): Unit
    val evicted = run(Offload.evict(root, bl, keepLocalBytes = 500L))
    assert(evicted.nonEmpty, "nothing evicted under a tiny budget")
    val after = logsUnder(root)
    assert(after.length < before)
    // the active segment never leaves
    assert(after.nonEmpty)

    // the store reopens; begin advanced exactly as under retention
    val s2 = FileStore.open(root)
    val t2 = s2.topic("events", 1, Policy(segmentBytes = 200))
    assert(t2.begin(0) > 0L)
    s2.close()
  }

  test("the tiered read serves blob history + local tail, byte-exact and bounded") {
    val root = tmp(); val bl = Fs(tmp())
    seeded(root)
    run(Backup.copy(root, bl)): Unit
    run(Offload.evict(root, bl, keepLocalBytes = 500L)): Unit

    val s = FileStore.open(root)
    val t = s.topic("events", 1, Policy(segmentBytes = 200))
    val b = t.begin(0)
    assert(b > 0L, "eviction did not advance begin")

    val tiered = Offload.Tiered(t, bl, "persist", "events")
    val all = run(tiered.read(0, 0L, 100))
    assertEquals(all.map(_.offset), (0L until 40L).toVector,
      "the tier lost or reordered history")
    assertEquals(all.map(r => str(r.value)), (0 until 40).map(i => s"v$i").toVector)
    // bounded like any read
    assertEquals(run(tiered.read(0, 0L, 7)).map(_.offset), (0L until 7L).toVector)
    // at or past local begin: the local road answers alone
    assertEquals(run(tiered.read(0, b, 100)).map(_.offset), (b until 40L).toVector)
    assertEquals(run(tiered.end(0)), 40L)
    s.close()
  }

  test("a stored copy parses through the documented format, and says it is sound") {
    val root = tmp(); val bl = Fs(tmp())
    seeded(root, n = 10)
    run(Backup.copy(root, bl)): Unit
    val key = run {
      val S = summon[okay.Stream[[X] =>> X ! (Produce + Async), Async]]
      def first(p: okay.Chunk[Meta] ! (Produce + Async)): Option[Meta] ! Async =
        S.uncons(p).map(_.flatMap(_._1.headOption))
      first(bl.list("persist/"))
    }.getOrElse(fail("no copies")).key
    val parsed = Segments.parse(run(Offload.fetchBytes(bl, key)))
    assert(parsed.sound, "a verified copy parsed unsound")
    assertEquals(parsed.header.topic, "events")
    assert(parsed.records.nonEmpty)
  }
