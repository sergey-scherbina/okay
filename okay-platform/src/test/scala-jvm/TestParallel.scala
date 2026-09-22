package okay


/** Fibers per program, retries per policy. The CHUNKED half — a
 * fiber per chunk and lineage recompute per chunk — went to
 * okay-stream with `parMap` and `retryChunks` themselves
 * (core-modules stage 1); it is `TestParallelChunks` there. */
class TestParallel extends munit.FunSuite {

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
