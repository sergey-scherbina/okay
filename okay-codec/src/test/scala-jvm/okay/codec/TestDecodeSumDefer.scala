package okay.codec

/**
 * decodeC-ssum-defer: `Schema.SSum`'s case in `Json.decodeC` and
 * `Cbor.getC` was the one recursive branch in either fold that called
 * its own recursion directly instead of through `Cont.defer` — found
 * while investigating `stackbytes-json-read-not-flat-on-aarch64`
 * (okay-codec/BUGS.md; it does not explain that entry, since `Tree`
 * there has no sum type, but is real and worth closing).
 *
 * `Schema.derived` always wraps a case's payload in a product, and a
 * product's own recursive field IS deferred — so a DERIVED schema
 * can never observe this: checked directly, a `derives`-built
 * recursive `enum` at 500 levels passes on a 256 KB stack whether or
 * not the fix is applied. The gap is reachable only through a
 * HAND-BUILT `Schema.SSum` whose case schema recurses with no
 * product between, which `SSum`'s own type (`cases:
 * Vector[(String, () => Schema[? <: A])]`) allows and nothing stops.
 * This file builds exactly that: `Wrap` decodes to another `wrap`
 * schema directly, `Leaf` ends it — no product, no defer, before the
 * fix.
 */
class TestDecodeSumDefer extends munit.FunSuite:

  private val leaf: Schema[Unit] = Schema.SProduct[Unit]("Leaf", Vector.empty, _ => (), _ => Seq.empty)

  /** `Wrap` recurses into `wrap` ITSELF — an `SSum` case whose schema
   * is the enclosing sum, no product between. `caseOf` is never used:
   * this file only decodes, so which case a Unit value would encode
   * as is moot. */
  private lazy val wrap: Schema[Unit] = Schema.SSum[Unit]("Wrap",
    Vector("Leaf" -> (() => leaf), "Wrap" -> (() => wrap)), _ => 0)

  private def jsonOf(n: Int): String =
    ("{\"Wrap\":" * n) + "{\"Leaf\":{}}" + ("}" * n)

  private def cborOf(n: Int): Array[Byte] =
    val out = new Cbor.Out
    def go(remaining: Int): Unit =
      out.mapHeader(1)
      if remaining <= 0 then { out.text("Leaf"); out.mapHeader(0) }
      else { out.text("Wrap"); go(remaining - 1) }
    go(n)
    out.toArray

  /** run `body` on a thread with exactly this much stack; the same
   * shape TestStackBytes.onStack uses, kept local since this file's
   * only borrowed idea from it is the technique, not its doors */
  private def onStack(kb: Int)(body: () => Boolean): Boolean =
    var out = false
    val t = new Thread(null, () => out =
      try body() catch case _: StackOverflowError => false,
      s"decodeC-ssum-defer-probe-$kb", kb.toLong * 1024)
    t.start()
    t.join()
    out

  test("a hand-built SSum recursing through its own case decodes at 500 levels on 256 KB, both wires") {
    assert(onStack(256)(() => Json.decode(wrap)(Json.parse(jsonOf(500))).isRight),
      "Json.decodeC overflowed a 256 KB stack at 500 levels through an un-product-wrapped SSum case")
    assert(onStack(256)(() => Cbor.read[Unit](cborOf(500))(using wrap).isRight),
      "Cbor.getC overflowed a 256 KB stack at 500 levels through an un-product-wrapped SSum case")
  }
