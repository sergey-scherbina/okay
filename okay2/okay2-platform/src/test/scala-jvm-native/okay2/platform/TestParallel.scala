package okay2.platform

import okay2._
import okay2.async._

/** fibers per program, retries per policy — the blocking half */
class TestParallel extends munit.FunSuite {

  test("parTraverse: order preserved, fibers really used") {
    val r = parTraverse(1 to 6)(i => Async { Thread.sleep(10); i * 10 }).runWith
    assertEquals(r, (1 to 6).map(_ * 10))
  }

  test("retry: succeeds after failures, per policy; exhausted rethrows") {
    var attempts = 0
    def flaky: Int ! Async = Async { attempts += 1; if (attempts < 3) throw new RuntimeException("flap") else 42 }
    assertEquals(retry(Retry.exponential(1).take(5))(flaky).runWith, 42)
    assertEquals(attempts, 3)
    attempts = 0
    def hopeless: Int ! Async = Async { attempts += 1; throw new RuntimeException("no") }
    val _ = intercept[RuntimeException](retry(Retry.immediate(2))(hopeless).runWith)
    assertEquals(attempts, 3)
  }

  test("retry: a policy of 200 000 immediate retries exhausts as its own exception, not a stack overflow") {
    // stack-safety-rest: `go(rest)` inside the catch was a frame per retry.
    // Native builds an exception's trace slowly enough that 200 000 of
    // them pass munit's 30 s; 20 000 there still makes the point
    val n = if (System.getProperty("java.vm.name", "").contains("Scala Native")) 20000 else 200000
    var attempts = 0
    def hopeless: Int ! Async = Async { attempts += 1; throw new RuntimeException("no") }
    val e = intercept[RuntimeException](retry(Retry.immediate(n))(hopeless).runWith)
    assertEquals(e.getMessage, "no")
    assertEquals(attempts, n + 1)
  }

  test("policies are streams: exponential sequences, jitter stays bounded") {
    assertEquals(Retry.exponential(10).take(4).toList, List(10L, 20L, 40L, 80L))
    assertEquals(Retry.exponential(10, cap = 35).take(4).toList, List(10L, 20L, 35L, 35L))
    Retry.jittered(Retry.constant(100)).take(50).foreach(d => assert(d >= 50 && d < 150, s"jitter $d out of [50, 150)"))
  }

  test("supervised: the fiber restarts its program and completes") {
    var attempts = 0
    val f = supervised(Retry.immediate(3))(Async { attempts += 1; if (attempts < 2) throw new RuntimeException("boom") else 7 })
    assertEquals(f.join(), 7)
    assertEquals(attempts, 2)
  }

  test("Par.sequence: eight leaves that must meet, do meet; results in order; a failing leaf fails the spine at once") {
    val latch = new java.util.concurrent.CountDownLatch(8)
    def leaf(millis: Long): Boolean ! Async = Async { latch.countDown(); latch.await(millis, java.util.concurrent.TimeUnit.MILLISECONDS) }
    assertEquals(Par.sequence(Seq.fill(8)(leaf(10000))).runWith, Seq.fill(8)(true))
    assertEquals(Par.traverse((1 to 50).toList)(i => Async(i * 2)).runWith, (1 to 50).map(_ * 2))
    assertEquals(Par.sequence(Seq.empty[Int ! Async]).runWith, Seq.empty[Int])
    @volatile var finished = false
    def slow = Async { Thread.sleep(3000); finished = true; 1 }
    def bad: Int ! Async = Async(throw new RuntimeException("boom"))
    for ((label, leaves) <- Seq("failure first" -> Seq(bad, slow), "failure second" -> Seq(slow, bad))) {
      finished = false
      val t0 = System.nanoTime()
      assertEquals(intercept[RuntimeException](Par.sequence(leaves).runWith).getMessage, "boom", label)
      assert((System.nanoTime() - t0) / 1e9 < 2, s"$label: the spine waited for the healthy sibling")
      assertEquals(finished, false, label)
    }
  }
}
