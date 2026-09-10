package okay.codec

/**
 * `Json.decode`'s threshold split (json-decode-threshold-trampoline,
 * specs/iterative-recursive-decode.md, root 2 of 2): native recursion
 * below `NativeThreshold`, `Cont.defer` past it, the same design
 * `Cbor.get` already carries (`TestCborTrampoline`).
 */
class TestJsonTrampoline extends munit.FunSuite:

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  /** the same shape as a Json VALUE, built directly — no string, no
    * parser, no `Json.lossless` (which enforces `Codecs.maxDepth`
    * itself and is QUADRATIC in depth for this shape, found while
    * writing this test: 50 000 levels took 74s — a real, separate,
    * out-of-scope bug, BACKLOG's json-lossless-quadratic-depth). This
    * is the only way to hand `Json.decode` a document deeper than
    * `Codecs.maxDepth` that was never cut in the first place. */
  def jsonChainValue(n: Int): Json =
    var v: Json = Json.JObj(Vector("kids" -> Json.JArr(Vector.empty)))
    var i = 0
    while i < n do { v = Json.JObj(Vector("kids" -> Json.JArr(Vector(v)))); i += 1 }
    v

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  test("decode is safe on ANY depth, independent of Codecs.maxDepth — Json.decode has no wire limit of its own") {
    // this is the design point that makes Json.decode different from
    // Cbor.get: the wire-contract refusal lives upstream, in
    // Json.isCut at PARSE time. `Json.decode` called directly on a
    // Json VALUE that never went through the parser's cut (built here
    // rather than parsed, since Json.lossless enforces the cut too)
    // is bound by NOTHING but the trampoline this lane adds
    val n = 200000
    val v = jsonChainValue(n)
    assert(!Json.isCut(v), "a directly-built value was never cut")
    Json.decode(summon[Schema[Tree]])(v) match
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(t) => assertEquals(depthOf(t), n + 1)
  }

  test("Json.read still enforces Codecs.maxDepth — the cut happens before decode ever runs") {
    // a modest depth here (Json.lossless's own cost is quadratic in
    // depth for this shape — see jsonChainValue's comment — so this
    // stays small; the cut fires long before size matters)
    val n = 500
    Json.read[Tree](jsonChain(n)) match
      case Left(e) => assert(e.contains(s"nested deeper than ${Codecs.maxDepth}"), e)
      case Right(v) => fail(s"decoded $v past Codecs.maxDepth through Json.read")
  }

  test("ordinary shapes below the threshold are untouched: products, sums, lists, options, isos") {
    enum Color derives Schema:
      case Red
      case Green(shade: Int)
    final case class Email(value: String)
    given Schema[Email] = Schema.wrap[Email, String](Email(_), _.value)
    final case class Order(id: Int, tags: List[String], color: Color,
                            email: Option[Email], amounts: Vector[Double])
    given Schema[Order] = Schema.derived

    val o = Order(7, List("a", "b"), Color.Green(3), Some(Email("x@y.z")), Vector(1.5, -2.0))
    assertEquals(Json.read[Order](Json.write(o)), Right(o))
    assertEquals(Json.read[Order](Json.write(o.copy(email = None))), Right(o.copy(email = None)))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    given Schema[Bad] = Schema.derived
    val v = jsonChainValue(50)
    assert(Json.decode(summon[Schema[Bad]])(v).isLeft)
  }

  test("a damaged list element past the threshold is still skipped, not a fault") {
    // an element nested past NativeThreshold (a real Tree, decoded via
    // the trampoline) beside a DAMAGED element, both inside a List —
    // proves the trampoline's own list loop still applies the
    // skip-damage rule (input-depth-both-wires/cut-refuses-the-
    // -document), not a plain re-throw of the first Left it sees
    val goodDepth = jsonChainValue(30)
    val v = Json.JArr(Vector(goodDepth, Json.JErr("damaged")))
    Json.decode(summon[Schema[List[Tree]]])(v) match
      case Left(e) => fail(s"expected the damaged element skipped, got: $e")
      case Right(xs) => assertEquals(xs.map(depthOf), List(31))
  }
