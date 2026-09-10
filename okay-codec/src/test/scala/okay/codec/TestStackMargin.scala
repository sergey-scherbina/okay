package okay.codec

/**
 * The margin under a full-depth document, as a test rather than a
 * sentence (stack-depth-margin). `Codecs.maxDepth` — the wire limit
 * this file once measured margin under — is gone
 * (remove-codecs-maxdepth); every door here now trampolines past
 * `Codecs.NativeThreshold`, so this measures margin at a depth chosen
 * to comfortably cross that threshold, not at any wire cap.
 *
 * specs/codecs.md claimed 256 was "far under every death measured".
 * That was measured on ONE platform and on the ONE stack sbt happens
 * to give (`-Xss8m`), and nothing would have failed if a door's frames
 * got fatter or a deployment ran on a smaller stack. These doors
 * recurse on the depth of the INPUT, so the stack is part of their
 * contract and belongs in the suite.
 *
 * What is asserted here is deliberately LOOSE, and the measurement is
 * why: frames-to-death moves with JIT state — the same door in one run
 * of the original probe survived 32 000 foreign frames cold and
 * 256 000 warm. So this pins a modest slack that every platform has
 * room for, and the numbers themselves are printed (and recorded in
 * the spec) instead of asserted. `TestStackBytes` measures the JVM in
 * bytes, where the number IS stable.
 */
class TestStackMargin extends munit.FunSuite:

  val platform: String =
    val vm = System.getProperty("java.vm.name", "")
    if vm.contains("Scala.js") then "js" else if vm.contains("Native") then "native" else "jvm"

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def arrays(d: Int): String = ("[" * d) + "1" + ("]" * d)
  def jsonTree(d: Int): String = ("{\"kids\":[" * d) + "{\"kids\":[]}" + ("]}" * d)
  def cborTree(d: Int): Array[Byte] =
    val out = new Cbor.Out
    for _ <- 0 until d do { out.mapHeader(1); out.text("kids"); out.arrayHeader(1) }
    out.mapHeader(1); out.text("kids"); out.arrayHeader(0)
    out.toArray

  /** comfortably past NativeThreshold; two containers per tree level */
  val levels = 100
  val rawDepth = 200

  val doors: List[(String, () => Boolean)] = List(
    "JsonValue.parse" -> (() => JsonValue.parse(arrays(rawDepth)).isDefined),
    "Json.lossless"   -> (() => !Json.lossless(arrays(rawDepth)).isInstanceOf[Json.JErr]),
    "Json.decode"     -> (() => Json.read[Tree](jsonTree(levels)).isRight),
    "Json.readStrict" -> (() => Json.readStrict[Tree](jsonTree(levels)).isRight),
    "Cbor.read"       -> (() => Cbor.read[Tree](cborTree(levels)).isRight),
  )

  /**
   * `k` frames of somebody else's recursion under the door — a decode
   * does not own the stack it runs on, and this is the cheapest way to
   * say so on every platform (no thread API, nothing has to overflow).
   *
   * The arithmetic after the call is load-bearing: the first version of
   * this helper was tail-recursive, Scala turned it into a loop, and it
   * measured nothing at all while reporting success at every depth.
   */
  def under(k: Int)(body: () => Boolean): Int =
    if k <= 0 then (if body() then 1 else 0) else 1 + under(k - 1)(body) - 1

  test("the slack recursion really consumes stack — it is not a loop") {
    // the check needs an overflow, and a deliberate one is only safe
    // where it is an Error rather than a fault. MEASURED 2026-09-10:
    // catchable on the JVM and on Native 0.5.12 (which has stack
    // guards), a JavaScriptException on JS. Native is skipped anyway:
    // a future toolchain without the guard would take the whole test
    // process down, and that is the gate's known false red
    // (native-runner-error), which no assertion is worth.
    assume(platform != "native", "a deliberate stack overflow is not run on Native")
    var deepest = 0
    try
      var k = 1000
      while k <= 100_000_000 do { assert(under(k)(() => true) == 1); deepest = k; k *= 2 }
    catch case _: Throwable => ()
    assert(deepest > 0, "the calibration never ran")
    assert(deepest < 100_000_000, s"a recursion that survives $deepest frames is a loop, not a measurement")
    println(s"[stack] $platform: a trivial non-tail recursion gets ~$deepest frames")
  }

  test("every door reads a full-depth document with 1000 frames of foreign stack under it") {
    for (name, door) <- doors do
      assertEquals(under(1000)(door), 1,
        s"$name could not read a document at the limit with 1000 frames of slack")
  }
