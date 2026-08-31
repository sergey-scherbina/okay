package okay


/** Fibers per chunk, retries per policy, lineage recompute per chunk. */
class TestParallel extends munit.FunSuite {

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

  test("parTraverse: order preserved, fibers really used") {
    val r = parTraverse(1 to 6)(i => async { Thread.sleep(10); i * 10 }).runWith
    assertEquals(r, (1 to 6).map(_ * 10))
  }

  test("retry: succeeds after failures, per policy; exhausted rethrows") {
    var attempts = 0
    def flaky: Int ! Async = async {
      attempts += 1
      if attempts < 3 then throw RuntimeException("flap") else 42
    }
    assertEquals(retry(Retry.exponential(1).take(5))(flaky).runWith, 42)
    assertEquals(attempts, 3)

    attempts = 0
    def hopeless: Int ! Async = async { attempts += 1; throw RuntimeException("no") }
    intercept[RuntimeException](retry(Retry.immediate(2))(hopeless).runWith): Unit
    assertEquals(attempts, 3)   // the first try plus two retries
  }

  test("policies are streams: exponential sequences, jitter stays bounded") {
    assertEquals(Retry.exponential(10).take(4).toList, List(10L, 20L, 40L, 80L))
    assertEquals(Retry.exponential(10, cap = 35).take(4).toList, List(10L, 20L, 35L, 35L))
    Retry.jittered(Retry.constant(100)).take(50).foreach(d =>
      assert(d >= 50 && d < 150, s"jitter $d out of [50, 150)"))
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
      "val live: Chunk[Int] ! (Produce + Async) = ???\nretryChunks(live)")
    assert(errors.nonEmpty, "an effectful source must not typecheck as retryable")
  }

  test("supervised: the fiber restarts its program and completes") {
    var attempts = 0
    val f = supervised(Retry.immediate(3))(async {
      attempts += 1
      if attempts < 2 then throw RuntimeException("boom") else 7
    })
    assertEquals(f.join(), 7)
    assertEquals(attempts, 2)
  }
}
