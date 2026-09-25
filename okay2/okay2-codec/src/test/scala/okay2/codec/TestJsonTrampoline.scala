package okay2.codec

import DeepJson._

/** Every recursive walk is native below `Codecs.NativeThreshold` and a
 * `Cont.defer` trampoline past it (okay-codec's TestJsonTrampoline,
 * TestJsonRawTrampoline and TestEncodeTrampoline, JSON halves): the
 * two parsers, the projection, decode, the strict reader, print,
 * encode and merge patch, each at 100 000 levels. */
class TestJsonTrampoline extends munit.FunSuite {

  override val munitTimeout = scala.concurrent.duration.Duration(120, "s")

  private val n = 100000

  test("both parse roads answer the same deep value, arrays and objects") {
    for (text <- List(arrays(n), objects(n))) {
      assert(JsonValue.parse(text).isDefined, "the fast road refused a well-formed deep document")
      val v = Json.parse(text)
      assert(sameChain(v, Json.lossless(text)), "Json.parse and Json.lossless disagree")
      assertEquals(depth(v), n)
    }
  }

  test("Json.decode is safe on a directly-built value at any depth") {
    Json.decode(implicitly[Schema[Kids]])(kidsValue(2 * n)) match {
      case Left(e) => fail(s"expected a value, got: $e")
      case Right(t) => assertEquals(kidsDepth(t), 2 * n + 1)
    }
  }

  test("Json.read and Json.readStrict decode a genuinely deep document") {
    assertEquals(Json.read[Kids](kidsChain(n)).map(kidsDepth), Right(n + 1))
    assertEquals(Json.readStrict[Kids](kidsChain(n)).map(kidsDepth), Right(n + 1))
  }

  test("Json.print and Json.write on a genuinely deep in-memory value") {
    assert(sameChain(Json.parse(Json.print(deepArr(n))), deepArr(n)))
    var t = Kids(Vector.empty)
    var i = 0
    while (i < n) { t = Kids(Vector(t)); i += 1 }
    assertEquals(Json.read[Kids](Json.write(t)).map(kidsDepth), Right(n + 1))
  }

  test("Json.write on a genuinely deep recursive SUM") {
    var c: Chain = Chain.Leaf
    var i = 0
    while (i < n) { c = Chain.Node(c); i += 1 }
    Json.read[Chain](Json.write(c)) match {
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

  test("Json.mergePatch on a genuinely deep patch") {
    val patch = deepObj(n)
    assert(sameChain(Json.mergePatch(Json.JObj(Vector.empty), patch), patch))
  }

  test("errors past the threshold still refuse; a damaged element past it is skipped") {
    final case class Bad(a: Int)
    implicit val bad: Schema[Bad] = Schema.derived
    assert(Json.decode(bad)(kidsValue(50)).isLeft)
    val v = Json.JArr(Vector(kidsValue(30), Json.JErr("damaged")))
    assertEquals(Json.decode(implicitly[Schema[List[Kids]]])(v).map(_.map(kidsDepth)), Right(List(31)))
  }
}
