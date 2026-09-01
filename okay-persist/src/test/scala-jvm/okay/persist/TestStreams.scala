package okay.persist

import okay.{!, +, Async, Chunk, Handler, Produce}
import okay.given

/**
 * The streaming conveniences (specs/persist.md, Interface): chunked
 * pulls, one Async operation per chunk; `tail` parks on the platform
 * timer and sees appends made after it caught up — the ui-durable
 * and resumable-SSE shape, exercised here end to end.
 */
class TestStreams extends munit.FunSuite {

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  /** step the freer tree until `n` chunks are collected (or the
   * stream ends), handling Async on the current thread */
  def takeChunks[A](s0: Chunk[A] ! (Produce + Async), n: Int): List[Chunk[A]] =
    import okay.!.*
    var acc = List.empty[Chunk[A]]
    var cur = s0
    var going = true
    while going && acc.length < n do
      (cur.resume: @unchecked) match
        case Pure(_) => going = false
        case Effect(e) => okay.<|>[Async, Produce](e) match
          case Left(a) => (summon[Handler[Async]].handle(a): Unit); going = false
          case Right(c) => acc ::= c.asInstanceOf[Chunk[A]]; going = false
        case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
          case Left(a) => cur = k(summon[Handler[Async]].handle(a))
          case Right(c) => acc ::= c.asInstanceOf[Chunk[A]]; cur = k(c)
    acc.reverse

  test("stream: chunked to the end, bounded chunks, then it ends") {
    val t = MemoryStore().topic("s")
    (0 until 250).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Received))
    val chunks = takeChunks(Streams.stream(t, 0, 0L, chunk = 64), 100)
    assertEquals(chunks.map(_.length), List(64, 64, 64, 58))
    assertEquals(chunks.flatten.map(r => str(r.value)), (0 until 250).map(i => s"v$i").toList)
  }

  test("stream from dropped history: Fail throws naming begin, Resume continues from it") {
    val t = MemoryStore().topic("r", partitions = 1, policy = Policy(retainBytes = 340))
    (0 until 50).foreach(i => t.append(0, Array.empty, bytes(s"payload-$i"), Ack.Received))
    val b = t.begin(0)
    assert(b > 0L)

    val e = intercept[Streams.DroppedHistory](takeChunks(Streams.stream(t, 0, 0L), 1))
    assertEquals(e.asked, 0L)
    assertEquals(e.begin, b)

    val resumed = takeChunks(
      Streams.stream(t, 0, 0L, onTooEarly = Streams.OnTooEarly.Resume), 100)
    assertEquals(resumed.flatten.head.offset, b)
    assertEquals(resumed.flatten.last.offset, 49L)
  }

  test("tail sees an append made after it caught up") {
    val t = MemoryStore().topic("tl")
    (0 until 3).foreach(i => t.append(0, Array.empty, bytes(s"v$i"), Ack.Received))
    val writer = new Thread(() => {
      Thread.sleep(80)
      t.append(0, Array.empty, bytes("late"), Ack.Received)
      ()
    })
    writer.start()
    try
      val got = takeChunks(Streams.tail(t, 0, 0L, chunk = 10, pollMillis = 10), 2)
      assertEquals(got.map(_.length), List(3, 1))
      assertEquals(got.flatten.map(r => str(r.value)), List("v0", "v1", "v2", "late"))
    finally writer.join()
  }
}
