package okay.codec

/**
 * The write side of the same recursion `iterative-recursive-decode.md`
 * closed on the read side (encode-side-depth-safety, found while
 * answering an operator question about `Cont`'s fusion budget, not
 * from a bug report). `Json.print`, `Cbor.put`/`write` and
 * `Json.mergePatch` all recurse on a VALUE's own depth — `print`/
 * `put` on the value being written, `mergePatch` on the patch — the
 * exact shape the decode arc fixed, just on the other end of the
 * pipe. `remove-codecs-maxdepth` only ever bounded how deep DECODE
 * could hand a value back; a value that deep can now exist in memory
 * from untrusted input, and writing it back out was still plain
 * native recursion. Every value here is built directly (a loop, not
 * a decode), so these tests do not depend on decode's own depth
 * safety — only on the write side's.
 */
class TestEncodeTrampoline extends munit.FunSuite:

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def deepTree(n: Int): Tree =
    var t = Tree(Vector.empty)
    var i = 0
    while i < n do { t = Tree(Vector(t)); i += 1 }
    t

  def deepJsonArr(n: Int): Json =
    var j: Json = Json.JArr(Vector.empty)
    var i = 0
    while i < n do { j = Json.JArr(Vector(j)); i += 1 }
    j

  def deepJsonObj(n: Int): Json =
    var j: Json = Json.JObj(Vector.empty)
    var i = 0
    while i < n do { j = Json.JObj(Vector("a" -> j)); i += 1 }
    j

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  /** the same node-by-node comparison `TestJsonRawTrampoline` uses:
    * structural `==`/`assertEquals` on a 100 000-deep value is its own
    * StackOverflowError, unrelated to the code under test. Extended
    * with the two chains' own empty base case (`deepJsonArr`/
    * `deepJsonObj` bottom out at an EMPTY container, not a leaf). */
  def sameChain(a: Json, b: Json): Boolean =
    var x = a
    var y = b
    var go = true
    var ok = true
    while go do
      (x, y) match
        case (Json.JArr(Vector(one)), Json.JArr(Vector(two))) => x = one; y = two
        case (Json.JObj(Vector((k1, one))), Json.JObj(Vector((k2, two)))) =>
          if k1 != k2 then { ok = false; go = false } else { x = one; y = two }
        case (Json.JArr(v1), Json.JArr(v2)) => ok = v1.isEmpty && v2.isEmpty; go = false
        case (Json.JObj(f1), Json.JObj(f2)) => ok = f1.isEmpty && f2.isEmpty; go = false
        case (Json.JNum(n1), Json.JNum(n2)) => ok = n1 == n2; go = false
        case _ => ok = false; go = false
    ok

  test("Json.print on a genuinely deep in-memory value — no cap, no stack overflow") {
    val n = 100000
    val text = Json.print(deepJsonArr(n))
    // proving it round-trips is proving print AND parse (already
    // proven safe) agree, not just that print returned SOME string
    assert(sameChain(Json.parse(text), deepJsonArr(n)), "print/parse round trip disagreed")
  }

  test("Cbor.write on a genuinely deep in-memory recursive-schema value — no cap, no stack overflow") {
    val n = 100000
    val t = deepTree(n)
    val bytes = Cbor.write(t)
    Cbor.read[Tree](bytes) match
      case Left(e) => fail(s"round trip failed: $e")
      case Right(back) => assertEquals(depthOf(back), n + 1)
  }

  // a PRODUCT with two fields caught the real defect this lane found
  // (encode-side-depth-safety): `putC`'s SProduct case wrote every
  // field's KEY up front via `eachField`'s own eager loop, then every
  // VALUE after (key,key,value,value, not a CBOR map at all) —
  // `Cbor.read` answered "missing field 'kids'" the moment a second
  // field existed. `Tree` above has only one field and could not have
  // caught it; kept here as the regression witness alongside it.
  final case class Two(label: String, kids: Vector[Two])
  given Schema[Two] = Schema.derived

  def deepTwo(n: Int): Two =
    var t = Two("leaf", Vector.empty)
    var i = 0
    while i < n do { t = Two(s"n$i", Vector(t)); i += 1 }
    t

  test("Cbor.write on a genuinely deep two-field product — the field-order regression witness") {
    val n = 100000
    val t = deepTwo(n)
    Cbor.read[Two](Cbor.write(t)) match
      case Left(e) => fail(s"round trip failed: $e")
      case Right(back) =>
        var d = 1
        var at = back
        while at.kids.nonEmpty do { d += 1; at = at.kids.head }
        assertEquals(d, n + 1)
        assertEquals(back.label, s"n${n - 1}")
  }

  // a recursive SUM, deep past the threshold: `putC`'s SSum case
  // writes its one case-name key eagerly too (`su.theCase` calls its
  // callback once, synchronously) — proven safe by direct reasoning
  // (nothing else can interleave with a single key/value pair), and
  // by this test rather than by reasoning alone
  enum Chain derives Schema:
    case Leaf
    case Node(next: Chain)

  def deepChain(n: Int): Chain =
    var c: Chain = Chain.Leaf
    var i = 0
    while i < n do { c = Chain.Node(c); i += 1 }
    c

  test("Cbor.write on a genuinely deep recursive SUM — no cap, no stack overflow") {
    val n = 100000
    Cbor.read[Chain](Cbor.write(deepChain(n))) match
      case Left(e) => fail(s"round trip failed: $e")
      case Right(back) =>
        var d = 0
        var at = back
        var go = true
        while go do at match
          case Chain.Node(next) => d += 1; at = next
          case Chain.Leaf => go = false
        assertEquals(d, n)
  }

  test("Json.mergePatch on a genuinely deep patch — no cap, no stack overflow") {
    val n = 100000
    val patch = deepJsonObj(n)
    val merged = Json.mergePatch(Json.JObj(Vector.empty), patch)
    // the merge of an empty target with an all-object patch is the
    // patch itself (RFC 7396: nothing to delete, nothing to keep) —
    // `sameChain`, not `assertEquals`, for the same reason as above
    assert(sameChain(merged, patch), "mergePatch of an empty target changed the patch's own shape")
  }

  test("ordinary shapes below the threshold are untouched: print, write, mergePatch") {
    val small = Json.JObj(Vector("a" -> Json.JArr(Vector(Json.JNum(1), Json.JStr("x"), Json.JNull))))
    assertEquals(Json.parse(Json.print(small)), small)

    enum Color derives Schema:
      case Red
      case Green(shade: Int)
    final case class Order(id: Int, tags: List[String], color: Color)
    given Schema[Order] = Schema.derived
    val o = Order(7, List("a", "b"), Color.Green(3))
    assertEquals(Cbor.read[Order](Cbor.write(o)), Right(o))

    val target = Json.JObj(Vector("a" -> Json.JNum(1), "b" -> Json.JNum(2)))
    val patch = Json.JObj(Vector("a" -> Json.JNull, "c" -> Json.JNum(3)))
    assertEquals(Json.mergePatch(target, patch),
      Json.JObj(Vector("b" -> Json.JNum(2), "c" -> Json.JNum(3))))
  }
