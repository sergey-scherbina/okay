package okay

import Direct.*
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

/**
 * Independent binds, run together (specs/applicative-static.md,
 * stage 3). The proof is the same rendezvous stage 1 used and for the
 * same reason: each leaf must meet the others before it may finish,
 * so a sequential block cannot pass and no clock is consulted.
 */
class TestDirectParallel extends munit.FunSuite {

  /** a leaf: arrive, then wait for everyone else to arrive */
  private def leaf(latch: CountDownLatch, millis: Long, answer: Int): Int ! Async =
    async { latch.countDown(); if latch.await(millis, TimeUnit.MILLISECONDS) then answer else -1 }

  test("WITHOUT the import the same block is sequential — which is what makes the next test a proof") {
    val latch = CountDownLatch(2)
    val prog: Int ! Async = direct:
      val a = leaf(latch, 200, 1).reflect
      val b = leaf(latch, 200, 2).reflect
      a + b
    // the first leaf waits for a leaf that has not started: -1
    assertEquals(prog.runWith, 1)   // -1 + 2
  }

  test("WITH the import the leaves meet") {
    import Direct.parallelBinds.given
    val latch = CountDownLatch(2)
    val prog: Int ! Async = direct:
      val a = leaf(latch, 10000, 1).reflect
      val b = leaf(latch, 10000, 2).reflect
      a + b
    assertEquals(prog.runWith, 3)
  }

  test("a DEPENDENT pair stays sequential under the same import") {
    import Direct.parallelBinds.given
    val latch = CountDownLatch(2)
    val prog: Seq[Int] ! Async = direct:
      val a = leaf(latch, 200, 1).reflect
      val b = leaf(latch, 200, a + 1).reflect
      Seq(a, b)
    // b's rhs mentions a, so the run ends at one and nothing is
    // spawned: the first leaf waits alone and times out (-1), which
    // is exactly what the parallel pair above does NOT do
    assertEquals(prog.runWith.head, -1)
  }

  test("answers and binding order are unchanged, both modes") {
    val seq: Seq[Int] ! Async = direct:
      val a = async(1).reflect
      val b = async(2).reflect
      val c = async(3).reflect
      Seq(a, b, c)
    val par: Seq[Int] ! Async = locally:
      import Direct.parallelBinds.given
      direct:
        val a = async(1).reflect
        val b = async(2).reflect
        val c = async(3).reflect
        Seq(a, b, c)
    assertEquals(seq.runWith, Seq(1, 2, 3))
    assertEquals(par.runWith, Seq(1, 2, 3))
  }

  test("a statement that is not a spawnable leaf ends the run") {
    val forks = AtomicInteger(0)
    val under: Scheduler = Schedulers.threads   // the count is the test; any member will carry the fibers
    given counting: Scheduler = new:
      def fork[A](prog: () => A ! Async): Fiber[A] =
        forks.incrementAndGet()
        under.fork(prog)
    // the counter is the ONLY Scheduler here, so a fork count of zero
    // below means the macro emitted no spawn — not that it spawned
    // through some other one
    assert(summon[Scheduler] eq counting)

    import Direct.parallelBinds.given
    val prog: Int ! Async = direct:
      val a = async(1).reflect
      val gap = 10                 // no mark: the run ends here
      val b = async(2).reflect
      a + gap + b
    assertEquals(prog.runWith, 13)
    assertEquals(forks.get(), 0)
  }

  test("a WIDER row parallelises its Async leaves too") {
    // direct-parallel-wider-rows: the compiled leaf has been lifted
    // into the row by then, so the type test failed and the import
    // did nothing here, quietly. The leaf is read BEFORE the
    // narrowing now — the program the author wrote is still X ! Async.
    val forks = AtomicInteger(0)
    val under: Scheduler = Schedulers.threads   // the count is the test; any member will carry the fibers
    given counting: Scheduler = new:
      def fork[A](prog: () => A ! Async): Fiber[A] =
        forks.incrementAndGet()
        under.fork(prog)
    assert(summon[Scheduler] eq counting)

    import Direct.parallelBinds.given
    val prog: Int ! Reader % Int + Async = direct:
      val a = async(1).reflect
      val b = async(2).reflect
      val e = Reader.ask[Int].reflect
      a + b + e
    assertEquals(Reader.run[Int, Int, Async](39)(prog).runWith, 42)
    // the two Async leaves are independent and spawn; the Reader leaf
    // is not an Async program, so it ends the run and stays sequential
    assertEquals(forks.get(), 2)
  }

  test("a run of three forks exactly three fibers, and a dependent block forks none") {
    val forks = AtomicInteger(0)
    val under: Scheduler = Schedulers.threads   // the count is the test; any member will carry the fibers
    given counting: Scheduler = new:
      def fork[A](prog: () => A ! Async): Fiber[A] =
        forks.incrementAndGet()
        under.fork(prog)

    import Direct.parallelBinds.given
    val three: Int ! Async = direct:
      val a = async(1).reflect
      val b = async(2).reflect
      val c = async(3).reflect
      a + b + c
    assertEquals(three.runWith, 6)
    assertEquals(forks.get(), 3)

    forks.set(0)
    val chained: Int ! Async = direct:
      val a = async(1).reflect
      val b = async(a + 1).reflect
      a + b
    assertEquals(chained.runWith, 3)
    assertEquals(forks.get(), 0)
  }
}
