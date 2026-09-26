package okay

import java.io.DataInputStream

/**
 * THE INLINING BUDGET OF THE HOT PATH, as a test (backlog
 * resume-inline-budget-guard, found by the core review 2026-09-26).
 *
 * HotSpot pastes a hot callee into its caller only while the callee's
 * bytecode is at most `FreqInlineSize` bytes: 325 on this JDK (`java
 * -XX:+PrintFlagsFinal -version | grep FreqInlineSize`). Four measured
 * cases (docs/benchmarks.md, and the comment on `Effects[Free].handle` in Effects.scala) show that a
 * method crossing that line in EITHER direction re-decides every caller:
 * `relay` lost 10-12% at 329 bytes, and `resume` shrinking to 323 made
 * `relay` -6% and `handle` +15% until handle's loop was cut to 318.
 * Nothing noticed those crossings except a benchmark someone happened to
 * run. This test does: it reads the code length of each hand-sized
 * method out of the compiled class and fails when one crosses.
 *
 * A red here is NOT a verdict that the change is slower. It says the
 * change moved a method across the line, so the lanes named in the
 * message must be re-measured (HandlerBenchmark: rows `hff-*` in
 * docs/benchmarks.md, `de-*` in Effects.scala's comment on `handle`)
 * before it lands, and the budget below updated with the result.
 *
 * Code length is the JVM's count of the method's bytecode, the number
 * HotSpot compares. It changes with scalac's codegen as well as with our
 * source, so a Scala upgrade can turn this red with no edit here. That
 * is the point: the upgrade then gets re-measured as well.
 */
class TestInlineBudget extends munit.FunSuite {
  val FreqInlineSize = 325

  /**
   * (method name, code length) for every method of `cls`, read straight
   * from the class file (JVMS §4). Not the JDK's `java.lang.classfile`:
   * dotty 3.9 cannot load its sealed model types (their permitted
   * subclasses are `jdk.internal` classes, and the classfile parser fails
   * on them), and a code length is five fields into a method's `Code`
   * attribute, so a reader of our own costs one screen.
   */
  def methods(cls: String): List[(String, Int)] =
    val raw = getClass.getResourceAsStream(s"/okay/$cls.class")
    assert(raw != null, s"okay/$cls.class not on the test classpath")
    val in = DataInputStream(raw)
    try
      assertEquals(in.readInt(), 0xCAFEBABE)
      in.readUnsignedShort(); in.readUnsignedShort()           // minor, major
      val count = in.readUnsignedShort()
      val utf8 = new Array[String](count)
      var i = 1
      while i < count do
        in.readUnsignedByte() match
          case 1 => utf8(i) = in.readUTF()
          case 3 | 4 | 9 | 10 | 11 | 12 | 17 | 18 => in.skipNBytes(4)
          case 5 | 6 => in.skipNBytes(8); i += 1                // a long or double takes two slots
          case 7 | 8 | 16 | 19 | 20 => in.skipNBytes(2)
          case 15 => in.skipNBytes(3)
          case tag => fail(s"$cls: constant pool tag $tag unknown")
        i += 1
      in.skipNBytes(6)                                          // access, this, super
      in.skipNBytes(2L * in.readUnsignedShort())                // interfaces
      def skipAttributes(): Unit =
        for _ <- 0 until in.readUnsignedShort() do
          in.readUnsignedShort(); in.skipNBytes(in.readInt() & 0xFFFFFFFFL)
      for _ <- 0 until in.readUnsignedShort() do                // fields
        in.skipNBytes(6); skipAttributes()
      List.fill(in.readUnsignedShort()) {
        in.readUnsignedShort()
        val name = utf8(in.readUnsignedShort())
        in.readUnsignedShort()
        var code = 0
        for _ <- 0 until in.readUnsignedShort() do
          val attr = utf8(in.readUnsignedShort())
          val len = in.readInt() & 0xFFFFFFFFL
          if attr == "Code" then
            in.readUnsignedShort(); in.readUnsignedShort()      // max_stack, max_locals
            code = in.readInt()
            in.skipNBytes(len - 8)
          else in.skipNBytes(len)
        (name, code)
      }
    finally in.close()

  /** the one method of `cls` whose name matches `name` whole — a local
   * `def loop` compiles to `loop$1`, `loop$2`, ..., so its pattern is
   * `loop\\$\\d+`, which leaves out a public `loop` and a lambda
   * `resume$$anonfun$1` beside `resume` */
  def sizeOf(cls: String, name: String): Int =
    val hits = methods(cls).filter((n, _) => n.matches(name))
    assertEquals(hits.size, 1, s"$cls.$name: expected exactly one method, found ${hits.map(_._1)}")
    hits.head._2

  def within(what: String, bytes: Int, remeasure: String): Unit =
    assert(bytes <= FreqInlineSize,
      s"$what is $bytes bytes, over FreqInlineSize ($FreqInlineSize): HotSpot will stop inlining it " +
      s"into its callers. Re-measure $remeasure before landing (Effects.scala, the comment on Effects[Free].handle).")

  test("Free.resume fits FreqInlineSize: every interpreter loop inlines it") {
    within("Free.resume", sizeOf("Free", "resume"),
      "relayPrebuilt AND handlePrebuilt/handleCapture — a resume crossing the line re-decides both")
  }

  test("relay's loop fits: its body is straight-line, so inlining it is free") {
    within("Effects.relay's loop", sizeOf("Effects$", "loop\\$\\d+"), "relayPrebuilt (rows de-*)")
  }

  test("handle's loop fits: it had to be cut to 318 once resume became inlinable") {
    within("Effects[Free].handle's loop", sizeOf("Effects$package$given_Effects_Free$", "loop\\$\\d+"),
      "handlePrebuilt and handleCapture (rows hff-*, de-*)")
  }

  test("the budget is read from real bytecode, not assumed") {
    // the instrument's own control: a method known to be tiny reads tiny,
    // and the measured hot methods are in the range Effects.scala records (resume 323, handle loop 318)
    assert(sizeOf("Effects$", "relay") < 20, "relay is a one-call wrapper around its loop")
    assert(sizeOf("Free", "resume") > 250, "resume read implausibly small: is this the right method?")
  }
}
