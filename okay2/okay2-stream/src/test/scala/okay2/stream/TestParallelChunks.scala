package okay2.stream

import okay2._
import okay2.platform._
import okay2.stream.ParallelChunks.{parMap, retryChunks}

/** A fiber per chunk, and lineage recompute per chunk — the Scala 3
 * core's TestParallelChunks */
class TestParallelChunks extends munit.FunSuite {

  test("parMap: a fiber per chunk — the same result, and an observed speedup") {
    val src = Chunks.range(0, 8, 1)   // 8 chunks of one element
    def slow(x: Long): Long = { Thread.sleep(30); x * 2 }
    val t0 = System.nanoTime()
    val seq = Chunks.fold(Chunks.map(src)(slow))(Fold.count)
    val tSeq = (System.nanoTime() - t0) / 1e6
    val t1 = System.nanoTime()
    val out = Chunks.fold(parMap(src, 4)(slow))(Fold.count)
    val ms = (System.nanoTime() - t1) / 1e6
    assertEquals(out, 8L)
    assertEquals(seq, 8L)
    assert(ms < tSeq * 0.8, s"not parallel: ${ms}ms vs sequential ${tSeq}ms")
    assertEquals(Chunks.fold(parMap(Chunks.range(0, 100), 4)(_ * 2))(Fold.sum[Long]), (0L until 100L).map(_ * 2).sum)
  }

  test("retryChunks: a failed chunk is recomputed from its lineage") {
    var failed = false
    val src = Chunks.generate(0)(x => if (x == 5 && !failed) { failed = true; throw new RuntimeException("chunk down") } else x)(_ + 1)(4)
    assertEquals(Chunks.fold(Chunks.take(retryChunks(src))(12))(Fold.count), 12L)
    assert(failed)
  }

  test("a non-replayable source refuses chunk-retry at compile time") {
    assert(compileErrors("""
      val live: okay2.Free[okay2.Produce with okay2.async.Async, okay2.stream.Chunk[Int]] = ???
      okay2.stream.ParallelChunks.retryChunks(live)""").nonEmpty, "an effectful source must not typecheck as retryable")
  }
}
