package okay

/**
 * THE SPIKE (delim-forward-not-throw, specs/delim-safety.md stage 1):
 * a machine that meets a capture for a prompt it does not hold can
 * RE-EMIT it outward instead of throwing — the shape the
 * foreign-operation path already uses for any effect it does not own.
 *
 * The argument for why it should work reads well, which is exactly
 * why each of its three claims is a separate test:
 *
 *   1. the inner machine's frames end up INSIDE the outer capture
 *   2. multi-shot survives (Segs is immutable, loop closes over
 *      nothing mutable)
 *   3. the inner delimiter is re-installed on resume (its Mark is
 *      still in the forwarded stack)
 *
 * A prediction is not a result.
 */
class TestDelimForward extends munit.FunSuite {

  type P = okay.Pure

  // an inner machine over a row that still has a Delim in it: the
  // shape `Delim.run` refuses and `runNested` is for
  def inner[A](p: A ! (Delim + (Delim + P))): A ! Delim + P =
    Delim.runNested(p)

  test("1 · the inner machine's frames are INSIDE the outer capture") {
    val outer = Delim.prompt[Int]
    // the capture names the OUTER prompt from inside the inner
    // machine; k(5) must run the inner tail (+1) AND the outer one
    // (+100), because both are in the captured continuation
    val prog: Int ! P = Delim.run[Int, P](Delim.push[Int, P](outer)(
      inner[Int](
        Delim.shift[Int, Int, Delim + P](outer)(k => k(5)).map(_ + 1)
      ).map(_ + 100)))
    assertEquals(!.run(prog), 106)
  }

  test("1b · dropping the forwarded continuation skips BOTH tails") {
    val outer = Delim.prompt[Int]
    var ranInner = false
    var ranOuter = false
    val prog: Int ! P = Delim.run[Int, P](Delim.push[Int, P](outer)(
      inner[Int](
        Delim.shift[Int, Int, Delim + P](outer)(_ => okay.pure(5))
          .map { x => ranInner = true; x + 1 }
      ).map { x => ranOuter = true; x + 100 }))
    assertEquals(!.run(prog), 5)
    assert(!ranInner, "the inner tail ran after its continuation was dropped")
    assert(!ranOuter, "the outer tail ran after the continuation was dropped")
  }

  test("2 · MULTI-SHOT survives forwarding: the inner machine is re-entered") {
    val outer = Delim.prompt[Int]
    val prog: Int ! P = Delim.run[Int, P](Delim.push[Int, P](outer)(
      inner[Int](
        Delim.shift[Int, Int, Delim + P](outer)(k =>
          k(1).flatMap(a => k(2).map(b => a + b))
        ).map(_ * 10)
      )))
    // k(1) = 10, k(2) = 20 — each a full re-entry of the inner machine
    assertEquals(!.run(prog), 30)
  }

  test("3 · the inner delimiter is still installed when the continuation resumes") {
    val outer = Delim.prompt[Int]
    // an inner scope whose prompt must survive the round trip: after
    // the forwarded capture resumes, a SECOND capture names the inner
    // prompt and must find it
    val prog: Int ! P = Delim.run[Int, P](Delim.push[Int, P](outer):
      val innerP = Delim.prompt[Int]
      inner[Int](Delim.push[Int, Delim + P](innerP)(
        Delim.shift[Int, Int, Delim + P](outer)(k => k(5)).flatMap(x =>
          Delim.shift[Int, Int, Delim + P](innerP)(k2 => k2(x * 2))).map(_ + 1))))
    assertEquals(!.run(prog), 11)
  }

  test("a capture the OUTER machine cannot place either still throws, and says so") {
    val stray = Delim.prompt[Int]
    val outer = Delim.prompt[Int]
    // a `def`: the outer machine runs while the program is being
    // BUILT, so the throw lands there rather than inside `!.run`
    def prog: Int ! P = Delim.run[Int, P](Delim.push[Int, P](outer)(
      inner[Int](Delim.shift[Int, Int, Delim + P](stray)(k => k(5)))))
    val e = intercept[NoPrompt](!.run(prog))
    // the error comes from the OUTER machine now, and its stack is the
    // one the user recognises
    assert(e.installed.nonEmpty, "the outer machine reported no delimiters")
  }

  test("`run` still throws: forwarding is opt-in, and every old test depends on it") {
    val stray = Delim.prompt[Int]
    intercept[NoPrompt](
      !.run(Delim.run[Int, P](Delim.shift[Int, Int, P](stray)(k => k(1)))))
  }
}
