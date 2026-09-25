package okay.codec

import java.nio.charset.StandardCharsets.UTF_8

/**
 * stack-safety-cbor-edn-wire: the wire's own readers and writers walk a
 * tree as deep as the far side sent. `WireCbor.decode` reads bytes a
 * worker chose, and caught only `IllegalStateException` — a
 * StackOverflowError from a deep message escaped it and took the reading
 * thread. `WireJson.whole` walks the repaired tree of a damaged line
 * after a parse that is itself trampolined, so the parse took a depth the
 * walk could not. Run on a SMALL stack (JVM only), where a walk that
 * recursed per level overflows at a few thousand.
 */
class TestWireDepth extends munit.FunSuite:

  private def smallStack[A](body: => A): A =
    var out: Either[Throwable, A] = Left(IllegalStateException("never ran"))
    val t = Thread(null, () => out = try Right(body) catch case e: Throwable => Left(e), "small-stack", 256L * 1024)
    t.start(); t.join()
    out.fold(e => throw e, identity)

  private val n = 20000

  /** `n` arrays of one, around `leaf` — built and measured by loops */
  private def nested(leaf: Json): Json =
    var j = leaf
    var i = 0
    while i < n do { j = Json.JArr(Vector(j)); i += 1 }
    j

  private def depthOf(j0: Json): (Int, Json) =
    var j = j0
    var d = 0
    var go = true
    while go do j match
      case Json.JArr(Vector(one)) => d += 1; j = one
      case _ => go = false
    (d, j)

  test("WireCbor.decode reads a message nested 20 000 deep") {
    // 0x81 is an array of one, 0x01 the integer 1
    val bytes = Array.fill[Byte](n)(0x81.toByte) :+ 0x01.toByte
    val got = smallStack(WireCbor.decode(bytes))
    assertEquals(got.map(depthOf), Right((n, Json.JNum(1))))
  }

  test("WireCbor.decode refuses a deep message cut short, as damage, not as a crash") {
    val bytes = Array.fill[Byte](n)(0x81.toByte)
    val got = smallStack(WireCbor.decode(bytes))
    assert(got.left.exists(_.contains("ended early")), got.toString)
  }

  test("WireCbor.encode writes a tree nested 20 000 deep, byte for byte") {
    val bytes = smallStack(WireCbor.encode(nested(Json.JNum(1))))
    assertEquals(bytes.toVector, (Array.fill[Byte](n)(0x81.toByte) :+ 0x01.toByte).toVector)
  }

  test("maps round-trip deep too: keys and values in order") {
    var j: Json = Json.JStr("x")
    var i = 0
    while i < n do { j = Json.JObj(Vector("k" -> j, "i" -> Json.JNum(i))); i += 1 }
    val back = smallStack(WireCbor.decode(WireCbor.encode(j)))
    // compared by walking, not by ==, which recurses on a deep tree
    var a = back.toOption.get
    var b = j
    var d = 0
    while d < n do (a, b) match
      case (Json.JObj(fa), Json.JObj(fb)) =>
        assertEquals(fa.map(_._1), fb.map(_._1))
        assertEquals(fa(1)._2, fb(1)._2)
        a = fa(0)._2; b = fb(0)._2; d += 1
      case _ => fail(s"shapes part at depth $d")
    assertEquals(a, Json.JStr("x"))
  }

  test("a declared length larger than the bytes left is refused before anything is allocated") {
    // an array of 2^31-1 items in five bytes: a reader that pre-sizes
    // from the header would try to hold two billion slots
    val bytes = Array(0x9a, 0x7f, 0xff, 0xff, 0xff).map(_.toByte)
    val got = WireCbor.decode(bytes)
    assert(got.isLeft, got.toString)
  }

  test("WireJson.whole takes a whole line nested 20 000 deep") {
    val line = "{\"a\":" + ("[" * n) + "1" + ("]" * n) + "}"
    val got = smallStack(WireJson.whole(line))
    got match
      case Json.JObj(Vector(("a", inner))) => assertEquals(depthOf(inner), (n, Json.JNum(1)))
      case other => fail(s"not the object: ${other.getClass}")
  }

  test("WireJson.whole refuses a DAMAGED line nested 20 000 deep as cut short, not with a crash") {
    // balanced, but `x` is no JSON token: the strict parser declines, the
    // total one repairs, and the repaired tree is what gets walked
    val line = "{\"a\":" + ("[" * n) + "x" + ("]" * n) + "}"
    val e = intercept[IllegalStateException](smallStack(WireJson.whole(line)))
    assert(e.getMessage.contains("not whole JSON"), e.getMessage)
  }

  test("shallow messages are unchanged") {
    val j = Json.JObj(Vector("op" -> Json.JStr("call"), "args" -> Json.JArr(Vector(Json.JNum(1.5), Json.JBool(true), Json.JNull)),
      "n" -> Json.JNum(-7)))
    assertEquals(WireCbor.decode(WireCbor.encode(j)), Right(j))
    assertEquals(WireJson.whole(Json.print(j)), j)
    assertEquals(new String(WireCbor.encode(Json.JStr("é")), UTF_8).drop(1), "é")
  }
