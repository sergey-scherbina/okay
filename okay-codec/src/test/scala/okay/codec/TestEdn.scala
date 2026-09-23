package okay.codec

import okay.codec.Edn.*

/**
 * EDN (specs/codecs.md, edn-codec): the text round trip, the syntax the
 * edn-format spec names, a Schema's value through EDN and back — and
 * the three things JSON cannot say, said: exact 64-bit integers, a
 * keyword that is not a string, a variant named by a tag.
 */
class TestEdn extends munit.FunSuite {

  enum Shape derives Schema:
    case Circle(r: Double)
    case Rect(w: Double, h: Double)

  final case class Doc(
    name: String,
    count: Long,
    big: BigInt,
    initial: Char,
    tags: List[String],
    sizes: Vector[Int],
    shape: Shape,
    note: Option[String],
    blob: Array[Byte],
  ) derives Schema

  val doc = Doc("okay", Long.MaxValue, BigInt("123456789012345678901234567890"), 'o',
    List("a", "b"), Vector(1, 2, 3), Shape.Rect(2.0, 3.5), None, Array[Byte](1, 2, 3))

  def same(a: Doc, b: Doc): Boolean =
    a.copy(blob = Array.empty) == b.copy(blob = Array.empty) && a.blob.sameElements(b.blob)

  test("a Schema value through EDN and back, every leaf kind") {
    val text = Edn.write(doc)
    val back = Edn.read[Doc](text).fold(e => fail(e), identity)
    assert(same(back, doc), s"$back from $text")
  }

  test("what EDN says that JSON cannot: keywords, an exact Long, N, \\c, a tagged variant") {
    val text = Edn.write(doc)
    assert(text.startsWith("{:name \"okay\" :count 9223372036854775807 :big 123456789012345678901234567890N :initial \\o "), text)
    assert(text.contains(":shape #Shape/Rect {:w 2.0 :h 3.5}"), text)
    assert(text.contains(":tags (\"a\" \"b\") :sizes [1 2 3]"), text)
    assert(text.contains(":note nil"), text)
    assert(text.contains(":blob #okay/bytes \"AQID\""), text)
  }

  test("the edn-format syntax: comments, commas, discard, sets, tags, characters, symbols") {
    val text = """; a comment
      {:a 1, :b [2 3 #_ 99] :c #{:x :y} :d \newline :e my.ns/sym :f #inst "2026-09-23" :g ##Inf :h 1.5M}"""
    Edn.parse(text) match
      case Right(EMap(kvs)) =>
        val m = kvs.collect { case (EKeyword(None, k), v) => k -> v }.toMap
        assertEquals(m("a"), ELong(1))
        assertEquals(m("b"), EVector(Vector(ELong(2), ELong(3))))
        assertEquals(m("c"), ESet(Vector(EKeyword(None, "x"), EKeyword(None, "y"))))
        assertEquals(m("d"), EChar('\n'))
        assertEquals(m("e"), ESymbol(Some("my.ns"), "sym"))
        assertEquals(m("f"), ETagged(None, "inst", EStr("2026-09-23")))
        assertEquals(m("g"), EDouble(Double.PositiveInfinity))
        assertEquals(m("h"), EDec(BigDecimal("1.5")))
      case other => fail(s"parsed as $other")
  }

  test("text round trip: show then parse is the identity on values") {
    val values = Vector(
      ENil, EBool(true), ELong(-42), EBig(BigInt(7)), EDouble(1.0), EDouble(-0.5),
      EStr("q\"uo\\te\n\t"), EChar(' '), EChar('('), EKeyword(Some("a.b"), "c"), ESymbol(None, "+"),
      EList(Vector(ELong(1), EList(Vector.empty))), ESet(Vector(ELong(1))),
      EMap(Vector(EKeyword(None, "k") -> EVector(Vector(ENil)))), ETagged(Some("my"), "tag", EStr("x")))
    for v <- values do
      assertEquals(Edn.parse(Edn.show(v)), Right(v), Edn.show(v))
  }

  test("an integer too big for a Long reads as a BigInt, and is refused for SLong by name") {
    assertEquals(Edn.parse("99999999999999999999"), Right(EBig(BigInt("99999999999999999999"))))
    assert(Edn.read[Long]("99999999999999999999").left.exists(_.contains("does not fit a Long")))
  }

  test("errors are named: a missing field, an unknown case, the wrong kind, trailing input, an open form") {
    assert(Edn.read[Shape]("#Shape/Triangle {}").left.exists(_.contains("unknown case 'Shape/Triangle'")))
    assert(Edn.read[Shape]("#Shape/Circle {}").left.exists(_.contains("missing field :r")))
    assert(Edn.read[Shape]("[1 2]").left.exists(_.contains("expected Shape")))
    assert(Edn.parse("{:a 1} extra").left.exists(_.contains("trailing input")))
    assert(Edn.parse("[1 2").left.exists(_.contains("input ended inside an open form")))
    assert(Edn.parse("{:a}").left.exists(_.contains("odd number of forms")))
  }

  test("an option is nil or the value; a missing optional field reads as None") {
    final case class O(a: Option[Int], b: Int) derives Schema
    assertEquals(Edn.read[O]("{:b 2}"), Right(O(None, 2)))
    assertEquals(Edn.read[O]("{:a 1 :b 2}"), Right(O(Some(1), 2)))
    assertEquals(Edn.write(O(None, 2)), "{:a nil :b 2}")
  }

  final case class Node(value: Int, next: Option[Node]) derives Schema

  test("deep nesting on the default stack: text and Schema, past NativeThreshold") {
    val depth = 20000
    val deepText = "[" * depth + "]" * depth
    val parsed = Edn.parse(deepText).fold(e => fail(e), identity)
    assertEquals(Edn.show(parsed).length, deepText.length)
    val chain = (1 to 5000).foldLeft(Option.empty[Node])((n, i) => Some(Node(i, n))).get
    val back = Edn.read[Node](Edn.write(chain)).fold(e => fail(e), identity)
    assertEquals(Iterator.iterate(Option(back))(_.flatMap(_.next)).takeWhile(_.isDefined).size, 5000)
  }
}
