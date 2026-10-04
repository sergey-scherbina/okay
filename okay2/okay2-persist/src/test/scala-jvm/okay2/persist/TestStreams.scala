package okay2.persist

import okay2.platform._
import okay2.stream.{Chunk, Source}

/**
 * The streaming conveniences (okay-persist's TestStreams): chunked
 * pulls, one Async operation per chunk; `tail` parks on the platform
 * timer and sees appends made after it caught up.
 */
class TestStreams extends munit.FunSuite {

  private def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")
  private def str(b: Array[Byte]): String = new String(b, "UTF-8")

  /** the first `n` told chunks (or all, if the source ends first), walked
   * lazily — `take` stops asking after `n`, so `tail`'s endless source is
   * fine here */
  def takeChunks[A](s: Source[Chunk[A]], n: Int): List[Chunk[A]] =
    Source.SourceOps(s).toLazyList.take(n).toList

  test("stream: chunked to the end, bounded chunks, then it ends") {
    val t = new MemoryStore().topic("s")
    (0 until 250).foreach(i => t.append(0, Array.empty[Byte], bytes(s"v$i"), Ack.Received))
    val chunks = takeChunks(Streams.stream(t, 0, 0L, chunk = 64), 100)
    assertEquals(chunks.map(_.length), List(64, 64, 64, 58))
    assertEquals(chunks.flatten.map(r => str(r.value)), (0 until 250).map(i => s"v$i").toList)
  }

  test("stream from dropped history: Fail throws naming begin, Resume continues from it") {
    val t = new MemoryStore().topic("r", partitions = 1, policy = Policy(retainBytes = 340))
    (0 until 50).foreach(i => t.append(0, Array.empty[Byte], bytes(s"payload-$i"), Ack.Received))
    val b = t.begin(0)
    assert(b > 0L)

    val e = intercept[Streams.DroppedHistory](takeChunks(Streams.stream(t, 0, 0L), 1))
    assertEquals(e.asked, 0L)
    assertEquals(e.begin, b)

    val resumed = takeChunks(Streams.stream(t, 0, 0L, onTooEarly = Streams.OnTooEarly.Resume), 100)
    assertEquals(resumed.flatten.head.offset, b)
    assertEquals(resumed.flatten.last.offset, 49L)
  }

  test("chunks: the partition as a blocking recipe; dropped history is DroppedHistory") {
    val t = new MemoryStore().topic("c", partitions = 1, policy = Policy(retainBytes = 340))
    (0 until 50).foreach(i => t.append(0, Array.empty[Byte], bytes(s"payload-$i"), Ack.Received))
    val b = t.begin(0)
    val got = okay2.Stream.FeedOps(Streams.chunks(t, 0, b, chunk = 4)).toLazyList.toList.flatten
    assertEquals(got.map(_.offset), (b until 50L).toList)
    intercept[Streams.DroppedHistory](okay2.Stream.FeedOps(Streams.chunks(t, 0, 0L)).toLazyList.toList)
  }

  test("tail sees an append made after it caught up") {
    val t = new MemoryStore().topic("tl")
    (0 until 3).foreach(i => t.append(0, Array.empty[Byte], bytes(s"v$i"), Ack.Received))
    val writer = new Thread(() => {
      Thread.sleep(80)
      val _ = t.append(0, Array.empty[Byte], bytes("late"), Ack.Received)
    })
    writer.start()
    try {
      val got = takeChunks(Streams.tail(t, 0, 0L, chunk = 10, pollMillis = 10), 2)
      assertEquals(got.map(_.length), List(3, 1))
      assertEquals(got.flatten.map(r => str(r.value)), List("v0", "v1", "v2", "late"))
    } finally writer.join()
  }
}
