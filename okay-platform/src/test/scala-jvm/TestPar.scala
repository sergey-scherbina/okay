package okay

import java.util.concurrent.{CountDownLatch, TimeUnit}

/**
 * The parallel applicative (specs/applicative-static.md, stage 1).
 *
 * The concurrency proof is a RENDEZVOUS, not a clock: each leaf
 * counts down one latch and then waits for the others. Under
 * `parSequence` they arrive and the program finishes; under
 * `sequence` the first leaf waits alone for leaves that have not
 * started, and its wait times out. A ratio of durations would flake
 * on a busy box (TestAsync says so in its own words, having flaked at
 * 0.78 against a 0.75 bound); a handshake cannot.
 */
class TestPar extends munit.FunSuite {

  /** one leaf: arrive, then wait for everyone else to arrive */
  private def leaf(latch: CountDownLatch, millis: Long): Boolean ! Async =
    async { latch.countDown(); latch.await(millis, TimeUnit.MILLISECONDS) }

  test("Par.sequence: eight leaves that must meet, do meet") {
    val latch = CountDownLatch(8)
    val progs = Seq.fill(8)(leaf(latch, 10000))
    assertEquals(Par.sequence(progs).runWith, Seq.fill(8)(true))
  }

  test("sequence: the same leaves cannot meet — which is what makes the above a proof") {
    val latch = CountDownLatch(2)
    // 200ms is the whole cost of this test: the first leaf waits for a
    // leaf the sequential spine will not start until it returns
    val progs = Seq.fill(2)(leaf(latch, 200))
    assertEquals(sequence(progs).runWith.head, false)
  }

  test("Par.traverse agrees with traverse on results and order") {
    val xs = (1 to 50).toList
    val f = (i: Int) => async(i * 2)
    assertEquals(Par.traverse(xs)(f).runWith, traverse(xs)(f).runWith)
    assertEquals(Par.traverse(xs)(f).runWith, xs.map(_ * 2))
  }

  test("applicative laws hold on results") {
    val A = summon[Applicative[Par]]
    import A.pure
    val v: Par[Int] = Par(async(21))
    val u: Par[Int => Int] = Par(async((_: Int) * 2))
    val w: Par[Int => Int] = Par(async((_: Int) + 1))
    // identity
    assertEquals(pure(identity[Int]).app(v).seq.runWith, v.seq.runWith)
    // homomorphism
    assertEquals(pure((x: Int) => x + 1).app(pure(41)).seq.runWith,
                 pure(42).seq.runWith)
    // interchange
    assertEquals(u.app(pure(21)).seq.runWith,
                 pure((f: Int => Int) => f(21)).app(u).seq.runWith)
    // composition
    val compose = (f: Int => Int) => (g: Int => Int) => f.compose(g)
    assertEquals(pure(compose).app(u).app(w).app(v).seq.runWith,
                 u.app(w.app(v)).seq.runWith)
  }

  test("fmap does not fork: a leaf mapped runs on the caller's own thread") {
    // the observable difference between `map` and `pure(f).app(_)`
    // through this instance — the latter would run f on a fiber
    val A = summon[Applicative[Par]]
    val here = Thread.currentThread().getName
    val prog = A.fmap(Par(!.pure(1)), (_: Int) => Thread.currentThread().getName)
    assertEquals(prog.seq.runWith, here)
  }

  test("the three spellings, all of them resolving to Par's own") {
    def user(id: Int): String ! Async = async(s"u$id")
    def orders(id: Int): Int ! Async = async(id * 2)

    // map2: a plain method, no extension resolution to lose
    assertEquals(Par.map2(Par(user(1)), Par(orders(1)))((u, o) => (u, o)).seq.runWith, ("u1", 2))

    // the idiom bracket: fmap through the instance, then app
    val A = summon[Applicative[Par]]
    val fn = A.fmap(Par(user(1)), (u: String) => (o: Int) => (u, o))
    assertEquals(fn.app(Par(orders(1))).seq.runWith, ("u1", 2))

    // and what a reader tries first. It used to type-check as the
    // IDENTITY comonad's map, a package-level given that put `map` on
    // every type; since comonad-id-map-capture that instance lives in
    // `Comonad`'s companion and `Par(p).map(f)` is Par's own.
    val mapped: Par[Int] = Par(user(1)).map(_.length)
    assertEquals(mapped.seq.runWith, 2)
  }

  test("a failing leaf fails the spine at once, in either order") {
    // FAIL-FAST IS INHERITED FROM Async.par, and it is symmetric since
    // par-fail-fast. It was NOT: a right-side failure waited out the
    // healthy sibling (3.017 s against 0.0007 s), and this test used
    // to pin both orders so the fix would announce itself. It did.
    val boom = RuntimeException("boom")
    @volatile var finished = false
    def slow = async { Thread.sleep(3000); finished = true; 1 }
    def bad: Int ! Async = async(throw boom)

    for (label, leaves) <- Seq("failure first" -> Seq(bad, slow),
                               "failure second" -> Seq(slow, bad))
    do
      finished = false
      val t0 = System.nanoTime()
      assertEquals(intercept[RuntimeException](Par.sequence(leaves).runWith).getMessage, "boom", label)
      val secs = (System.nanoTime() - t0) / 1e9
      assert(secs < 2, s"$label: the spine waited $secs s for the healthy sibling")
      assertEquals(finished, false, label)
  }

  test("an empty spine and a one-leaf spine are the program itself") {
    assertEquals(Par.sequence(Seq.empty[Int ! Async]).runWith, Seq.empty[Int])
    assertEquals(Par.sequence(Seq(async(7))).runWith, Seq(7))
  }
}
