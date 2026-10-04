package okay2.codec

import DeepJson.kidsDepth

final case class OnlyA(a: String)

/**
 * Every CBOR walk is native below `Codecs.NativeThreshold` and a
 * `Cont.defer` trampoline past it (okay-codec's TestCborTrampoline,
 * TestCborSkipTrampoline and TestEncodeTrampoline's CBOR half): decode,
 * the skip of an undeclared field, and encode, each far past the depth
 * native recursion survives.
 */
class TestCborTrampoline extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private def cborChain(n: Int): Array[Byte] = {
    val out = new Cbor.Out
    var i = 0
    while (i < n) { out.mapHeader(1); out.text("kids"); out.arrayHeader(1); i += 1 }
    out.mapHeader(1); out.text("kids"); out.arrayHeader(0)
    out.toArray
  }

  /** an OnlyA-shaped frame whose "deep" field is n nested one-element
   * arrays around a leaf integer — undeclared, so it is SKIPPED */
  private def frameWithDeepUnknown(n: Int): Array[Byte] = {
    val out = new Cbor.Out
    out.mapHeader(2)
    out.text("a"); out.text("kept")
    out.text("deep")
    var i = 0
    while (i < n) { out.arrayHeader(1); i += 1 }
    out.integer(1)
    out.toArray
  }

  test("a genuinely deep recursive value decodes correctly — no cap, no stack overflow") {
    val n = 200000
    assertEquals(Cbor.read[Kids](cborChain(n)).map(kidsDepth), Right(n + 1))
  }

  test("ordinary shapes below the threshold are untouched: products, sums, lists, options, isos") {
    val s = Server("db1", Port(5432), Some(UserId(9)))
    assertEquals(Cbor.read[Server](Cbor.write(s)), Right(s))
    assertEquals(Cbor.read[Server](Cbor.write(s.copy(owner = None))), Right(s.copy(owner = None)))
    val e: Expr = Expr.Add(Expr.Add(Expr.Num(1), Expr.Num(2)), Expr.Num(3))
    assertEquals(Cbor.read[Expr](Cbor.write(e)), Right(e))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    implicit val bad: Schema[Bad] = Schema.derived
    assert(Cbor.read[Bad](cborChain(50)).isLeft)
    // a mismatch deep inside a deep chain: the trampoline's own refusal
    val cut = cborChain(50)
    assert(Cbor.read[Kids](cut.dropRight(1)).isLeft)
  }

  test("a deeply nested UNDECLARED field, past NativeThreshold, is still skipped correctly") {
    assertEquals(Cbor.read[OnlyA](frameWithDeepUnknown(40)), Right(OnlyA("kept")))
  }

  test("a genuinely deep UNDECLARED field skips correctly — no cap") {
    assertEquals(Cbor.read[OnlyA](frameWithDeepUnknown(200000)), Right(OnlyA("kept")))
  }

  test("a truncated unknown field past the threshold is still damage, not a silent skip") {
    val whole = frameWithDeepUnknown(40)
    assert(Cbor.read[OnlyA](whole.take(whole.length - 3)).isLeft)
  }

  test("Cbor.write on a genuinely deep recursive value round-trips") {
    val n = 100000
    var t = Kids(Vector.empty)
    var i = 0
    while (i < n) { t = Kids(Vector(t)); i += 1 }
    assertEquals(Cbor.read[Kids](Cbor.write(t)).map(kidsDepth), Right(n + 1))
  }

  test("Cbor.write on a genuinely deep two-field product — the field-order regression witness") {
    val n = 100000
    var t = Tree("leaf", Vector.empty)
    var i = 0
    while (i < n) { t = Tree(s"n$i", Vector(t)); i += 1 }
    Cbor.read[Tree](Cbor.write(t)) match {
      case Left(e) => fail(s"round trip failed: $e")
      case Right(back) =>
        var d = 1
        var at = back
        while (at.kids.nonEmpty) { d += 1; at = at.kids.head }
        assertEquals(d, n + 1)
        assertEquals(back.label, s"n${n - 1}")
    }
  }

  test("Cbor.write on a genuinely deep recursive SUM") {
    val n = 100000
    var c: Chain = Chain.Leaf
    var i = 0
    while (i < n) { c = Chain.Node(c); i += 1 }
    Cbor.read[Chain](Cbor.write(c)) match {
      case Left(e) => fail(s"round trip failed: $e")
      case Right(back) =>
        var d = 0
        var at = back
        var go = true
        while (go) at match {
          case Chain.Node(next) => d += 1; at = next
          case Chain.Leaf => go = false
        }
        assertEquals(d, n)
    }
  }
}
