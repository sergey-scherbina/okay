package okay.parse

import okay.lex.Json as JsonLex

/**
 * `Parse.full`'s cost must be LINEAR in input size, including a
 * deeply NESTED input (parse-quadratic-stack-length, found
 * 2026-09-10 by json-decode-threshold-trampoline building a test
 * fixture: 50 000 levels of `{"kids":[...]}` took 74 SECONDS).
 *
 * The cause: `if b.stack.length <= 1` in `fullWith`'s per-token loop.
 * `.length` on a `List` walks the whole thing, and `b.stack`'s length
 * IS the current nesting depth — a check that runs once per TOKEN
 * against a stack that grows to the INPUT's own depth is O(depth) run
 * O(depth) times, the textbook shape of an accidental O(n²). Nothing
 * about JSON specifically: `Parse.fullWith`/`reparseWith` are shared
 * by every dialect that calls them (Xml, okay-rag's Code, okay-llm's
 * Structured — Markdown and Yaml use a hand loop and never had this).
 *
 * The test does not assert nanoseconds (a box-load-dependent number,
 * `docs/benchmarks.md` §0) — it asserts that QUADRUPLING the depth
 * does NOT roughly SIXTEEN-FOLD the time, which a real regression
 * would do regardless of the box.
 *
 * IT RUNS IN `sbt integrationTest`, NOT IN THE DEFAULT GATE, and the
 * road to that is worth the paragraph because the obvious fix was
 * tried first and MEASURED to fail.
 *
 * "And stays true even in a slow CI container" is what this file used
 * to say. Four consecutive gates on 2026-09-10 refuted it: at load
 * average 40-67 the ratio read 12.7x, 9.0x and 11.3x against its 8x
 * bound, and the absolute test read 20 324 ms against its 5 000 ms
 * one — while both passed ALONE on the same commit in 0.085 s and
 * 0.073 s.
 *
 * THE MINIMUM OF THREE WAS THE OBVIOUS REPAIR — load can only ADD
 * time, so the fastest of a few runs is the closest thing to an
 * uncontended machine, which is the rule docs/benchmarks.md applies to
 * every lane it publishes. It did not work: the next gate read 19.6x
 * and 11.5x with minima on both sides, and the 50 000-level test,
 * three times as long, hit munit's 30 s timeout. The reason is now
 * clear and is not fixable by sampling: the two sides of the ratio
 * differ 20x in duration (29 ms against 575 ms), so a contended
 * scheduler perturbs the long side far more often than the short one,
 * and no number of repetitions makes the quotient stable while every
 * run is descheduled.
 *
 * So the guard keeps its power and leaves the gate — which is what
 * AGENTS.md already said about suites whose outcome depends on timing
 * the box cannot control. `sbt integrationTest` runs it; a landing
 * does not wait on it.
 */
class TestParseDepth extends munit.FunSuite:

  /** timing-dependent: `sbt integrationTest`, never the default gate
   * (the scaladoc above has the four gates that decided it) */
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  def timeMs(body: => Any): Double =
    val t0 = System.nanoTime()
    val parsed = body                      // held, so nothing is optimised away
    val ms = (System.nanoTime() - t0) / 1e6
    if parsed == null then -1.0 else ms

  /** the fastest of three runs: a loaded box can only ADD time, so the
   * minimum is the closest this can get to the machine uncontended */
  def fastestOfThree(body: => Any): Double =
    var best = Double.MaxValue
    var i = 0
    while i < 3 do
      val t = timeMs(body)
      if t < best then best = t
      i += 1
    best

  test("Parse.full is not quadratic in nesting depth") {
    // warm the JIT on a small case first, so the timed runs are not
    // paying interpreter cost that would swamp a real O(n) vs O(n^2)
    // signal at these sizes
    timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(200))): Unit

    val small = 4000
    val big = small * 4
    val tSmall = fastestOfThree(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(small)))
    val tBig = fastestOfThree(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(big)))
    val ratio = tBig / math.max(tSmall, 0.5)  // floor: a near-zero tSmall must not divide up to a false alarm
    // linear predicts ~4x; quadratic predicts ~16x — 8x is the
    // generous middle that catches the regression. The minima help
    // and do not suffice, which is why this suite is `Live`
    assert(ratio < 8.0, f"4x the depth cost ${ratio}%.1fx the time ($tSmall%.1fms -> $tBig%.1fms) — looks quadratic again")
  }

  test("a 50 000-level document parses in well under a second") {
    // ONE run, not three: at 16 s under load three of them exceeded
    // munit's 30 s timeout, which is a worse failure than the one the
    // repetition was meant to fix
    val t = timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(50000)))
    assert(t < 5000.0, f"50 000 levels took ${t}%.0fms — was 74 000ms before this fix")
  }
