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
 * `docs/benchmarks.md` §0) — it asserts that DOUBLING the depth does
 * NOT roughly QUADRUPLE the time, which a real regression would do
 * regardless of the box, and stays true even in a slow CI container.
 */
class TestParseDepth extends munit.FunSuite:

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  /** GENERIC in the body's type: a timing harness that insists on
   * `Unit` makes every caller discard a real value, which is four
   * E175s at the call sites rather than one decision here */
  def timeMs[A](body: => A): Double =
    val t0 = System.nanoTime()
    body: Unit
    (System.nanoTime() - t0) / 1e6

  test("Parse.full is not quadratic in nesting depth") {
    // warm the JIT on a small case first, so the timed runs are not
    // paying interpreter cost that would swamp a real O(n) vs O(n^2)
    // signal at these sizes
    timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(200)))

    val small = 4000
    val big = small * 4
    val tSmall = timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(small)))
    val tBig = timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(big)))
    val ratio = tBig / math.max(tSmall, 0.5)  // floor: a near-zero tSmall must not divide up to a false alarm
    // linear predicts ~4x; quadratic predicts ~16x — 8x is the
    // generous middle that catches the regression without being a
    // flaky assertion on a loaded box
    assert(ratio < 8.0, f"4x the depth cost ${ratio}%.1fx the time ($tSmall%.1fms -> $tBig%.1fms) — looks quadratic again")
  }

  test("a 50 000-level document parses in well under a second") {
    val t = timeMs(Parse.full(JsonLex.scan, JsonParse.instrs)(jsonChain(50000)))
    assert(t < 5000.0, f"50 000 levels took ${t}%.0fms — was 74 000ms before this fix")
  }
