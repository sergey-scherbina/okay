package okay.codec

/**
 * `JsonStrict.Reader.get`'s threshold split (jsonstrict-threshold-
 * trampoline, specs/iterative-recursive-decode.md, target 3 — the
 * last of four): native recursion below `Codecs.NativeThreshold`,
 * `Cont.defer` past it, the same design `Cbor.get`/`Json.decode`
 * already carry.
 */
class TestJsonStrictTrampoline extends munit.FunSuite:

  final case class Tree(kids: Vector[Tree])
  given Schema[Tree] = Schema.derived

  def jsonChain(n: Int): String = ("{\"kids\":[" * n) + "{\"kids\":[]}" + ("]}" * n)

  def depthOf(t: Tree): Int =
    var d = 1
    var at = t
    while at.kids.nonEmpty do { d += 1; at = at.kids.head }
    d

  test("at the deepest depth Codecs.maxDepth allows, readStrict answers correctly") {
    val n = Codecs.maxDepth / 2 - 1
    JsonStrict.read[Tree](jsonChain(n)) match
      case Left(e) => fail(s"expected a value at the limit, got: $e")
      case Right(t) => assertEquals(depthOf(t), n + 1)
  }

  test("Codecs.maxDepth still refuses past it, on either side of the threshold") {
    val n = Codecs.maxDepth
    JsonStrict.read[Tree](jsonChain(n)) match
      case Left(e) => assert(e.contains(s"nested deeper than ${Codecs.maxDepth}"), e)
      case Right(t) => fail(s"decoded a ${depthOf(t)}-level tree past Codecs.maxDepth")
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
    assertEquals(JsonStrict.read[Order](Json.write(o)), Right(o))
    assertEquals(JsonStrict.read[Order](Json.write(o.copy(email = None))), Right(o.copy(email = None)))
  }

  test("an unknown field, deeply nested past the threshold, is still skipped") {
    final case class OnlyA(a: String)
    given Schema[OnlyA] = Schema.derived
    val text = "{\"a\":\"kept\",\"deep\":" + jsonChain(Codecs.maxDepth / 2 - 2) + "}"
    JsonStrict.read[OnlyA](text) match
      case Left(e) => fail(s"expected the unknown field skipped, got: $e")
      case Right(v) => assertEquals(v, OnlyA("kept"))
  }

  test("errors past the threshold still refuse, not silently succeed") {
    final case class Bad(a: Int)
    given Schema[Bad] = Schema.derived
    assert(JsonStrict.read[Bad](jsonChain(Codecs.maxDepth / 2 - 1)).isLeft)
  }

  test("Staged.strict agrees with the interpreted reader past the threshold") {
    val n = Codecs.maxDepth / 2 - 1
    val text = jsonChain(n)
    assertEquals(Staged.strict[Tree].decode(text).map(depthOf), Right(n + 1))
    assertEquals(Staged.strict[Tree].decode(text), JsonStrict.read[Tree](text))
  }
