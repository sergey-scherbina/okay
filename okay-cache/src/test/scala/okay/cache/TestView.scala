package okay.cache

import okay.{!, Async}
import okay.given
import okay.persist.{Ack, MemoryStore, Policy, Record}

/**
 * Regime 1 (specs/cache.md): the fold of a compacted keyed topic
 * serves `latest`; `lag` equals end minus consumed and appends move
 * it; a cold rebuild agrees with the warm view — including after
 * compaction, which is the snapshot story.
 */
class TestView extends munit.FunSuite {

  def run[A](prog: A ! Async): A =
    Async.runAsync(prog).value match
      case Some(t) => t.get
      case None => fail("the test program did not complete synchronously")

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  /** last-write-wins per key; an empty value is the tombstone */
  def lww(@scala.annotation.unused prev: Option[String], r: Record): Option[String] =
    if r.value.isEmpty then None else Some(str(r.value))

  def mkView(topic: okay.persist.Topic): View[String, String] =
    View(topic)(bytes)(lww)

  test("latest serves the fold; lag counts appends; refresh is the invalidation") {
    val t = MemoryStore().topic("kv", partitions = 1, policy = Policy(compact = true))
    val v = mkView(t)
    assertEquals(v.lag, 0L)

    t.append(bytes("a"), bytes("1"), Ack.Received): Unit
    t.append(bytes("b"), bytes("2"), Ack.Received): Unit
    assertEquals(v.lag, 2L, "appends did not move the lag")
    assertEquals(run(v.latest("a")), None, "latest read past the consumed offset")

    run(v.refresh())
    assertEquals(v.lag, 0L)
    assertEquals(run(v.latest("a")), Some("1"))
    assertEquals(run(v.latest("b")), Some("2"))

    t.append(bytes("a"), bytes("10"), Ack.Received): Unit
    assertEquals(v.lag, 1L)
    assertEquals(run(v.latest("a")), Some("1"), "the view ran ahead of its consumption")
    run(v.refresh())
    assertEquals(run(v.latest("a")), Some("10"))
  }

  test("a tombstone removes the key from the view") {
    val t = MemoryStore().topic("kv", partitions = 1, policy = Policy(compact = true))
    val v = mkView(t)
    t.append(bytes("a"), bytes("1"), Ack.Received): Unit
    t.append(bytes("a"), Array.empty, Ack.Received): Unit
    run(v.refresh())
    assertEquals(run(v.latest("a")), None)
  }

  test("a cold rebuild agrees with the warm view — before and after compaction") {
    val t = MemoryStore().topic("kv", partitions = 2, policy = Policy(compact = true))
    val warm = mkView(t)
    val keys = Vector("a", "b", "c", "d")
    for i <- 0 until 40 do
      t.append(bytes(keys(i % keys.length)), bytes(s"$i"), Ack.Received)
    run(warm.refresh())

    def snapshot(v: View[String, String]): Map[String, Option[String]] =
      keys.map(k => k -> run(v.latest(k))).toMap

    val cold = mkView(t)
    run(cold.refresh())
    assertEquals(snapshot(cold), snapshot(warm))

    // compaction drops the superseded history; the refold of what
    // remains is the SAME view — which is the snapshot story
    t.compact(0); t.compact(1)
    val afterCompact = mkView(t)
    run(afterCompact.refresh())
    assertEquals(snapshot(afterCompact), snapshot(warm))
    assertEquals(afterCompact.lag, 0L)
  }
}
