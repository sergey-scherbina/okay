package okay


/** A fiber per chunk, and lineage recompute per chunk: the half of
 * `TestParallel` that moved with `parMap` and `retryChunks`
 * (core-modules stage 1). */
class TestParallelChunks extends munit.FunSuite {

  test("parMap: a fiber per chunk — same result, observed speedup") {
    val src = Chunks.range(0, 8, 1)   // 8 chunks of one element
    def slow(x: Long): Long = { Thread.sleep(30); x * 2 }
    val t0 = System.nanoTime()
    val seq = Chunks.fold(Chunks.map(src)(slow))(using Fold.count)
    val tSeq = (System.nanoTime() - t0) / 1e6
    val t1 = System.nanoTime()
    val out = Chunks.fold(parMap(src, 4)(slow))(using Fold.count)
    val ms = (System.nanoTime() - t1) / 1e6
    assertEquals(out, 8L)
    assertEquals(seq, 8L)
    assert(ms < tSeq * 0.8, s"not parallel: ${ms}ms vs sequential ${tSeq}ms")
    assertEquals(
      Chunks.fold(parMap(Chunks.range(0, 100), 4)(_ * 2))(using Fold.sum[Long]),
      (0L until 100L).map(_ * 2).sum)
  }

  test("retryChunks: a failed chunk is recomputed from its lineage") {
    var failed = false
    val src = Chunks.generate(0)(x =>
      if x == 5 && !failed then { failed = true; throw RuntimeException("chunk down") }
      else x)(_ + 1)(4)
    assertEquals(Chunks.fold(Chunks.take(retryChunks(src))(12))(using Fold.count), 12L)
    assert(failed)
  }

  test("a non-replayable source refuses chunk-retry at compile time") {
    // retryChunks is typed on pure Chunks — a program whose pulls are
    // recomputable from the value alone. An effectful row (a live
    // consumer, a socket) is not that type, and the compiler says so.
    val errors = compileErrors(
      "val live: Chunk[Int] ! Produce + Async = ???\nretryChunks(live)")
    assert(errors.nonEmpty, "an effectful source must not typecheck as retryable")
  }
}
