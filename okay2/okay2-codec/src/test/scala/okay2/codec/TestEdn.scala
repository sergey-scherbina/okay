package okay2.codec

import okay2.codec.Edn._

sealed trait EdShape
object EdShape {
  final case class Circle(r: Double) extends EdShape
  final case class Rect(w: Double, h: Double) extends EdShape
}
final case class EdDoc(name: String, count: Long, big: BigInt, initial: Char, tags: List[String],
                       sizes: Vector[Int], shape: EdShape, note: Option[String], blob: Array[Byte])
final case class EdNode(value: Int, next: Option[EdNode])
object EdNode {
  implicit lazy val schema: Schema[EdNode] = Schema.derived
}
final case class EdO(a: Option[Int], b: Int)

/**
 * EDN (okay-codec's TestEdn): the text round trip, the syntax the
 * edn-format spec names, a Schema's value through EDN and back — and
 * the three things JSON cannot say, said: exact 64-bit integers, a
 * keyword that is not a string, a variant named by a tag.
 */
class TestEdn extends munit.FunSuite {

  private val doc = EdDoc("okay", Long.MaxValue, BigInt("123456789012345678901234567890"), 'o',
    List("a", "b"), Vector(1, 2, 3), EdShape.Rect(2.0, 3.5), None, Array[Byte](1, 2, 3))

  /** equal but for the array, compared by content */
  private def same(a: EdDoc, b: EdDoc): Boolean = {
    val none = Array.emptyByteArray
    a.copy(blob = none) == b.copy(blob = none) && a.blob.sameElements(b.blob)
  }

  test("a Schema value through EDN and back, every leaf kind") {
    val text = Edn.write(doc)
    val back = Edn.read[EdDoc](text).fold(e => fail(e), identity)
    assert(same(back, doc), s"$back from $text")
  }

  test("what EDN says that JSON cannot: keywords, an exact Long, N, \\c, a tagged variant") {
    val text = Edn.write(doc)
    assert(text.startsWith("{:name \"okay\" :count 9223372036854775807 :big 123456789012345678901234567890N :initial \\o "), text)
    assert(text.contains(":shape #EdShape/Rect {:w 2.0 :h 3.5}"), text)
    assert(text.contains(":tags (\"a\" \"b\") :sizes [1 2 3]"), text)
    assert(text.contains(":note nil"), text)
    assert(text.contains(":blob #okay/bytes \"AQID\""), text)
  }

  test("the edn-format syntax: comments, commas, discard, sets, tags, characters, symbols") {
    val text = """; a comment
      {:a 1, :b [2 3 #_ 99] :c #{:x :y} :d \newline :e my.ns/sym :f #inst "2026-09-23" :g ##Inf :h 1.5M}"""
    Edn.parse(text) match {
      case Right(EMap(kvs)) =>
        val m = kvs.collect { case (EKeyword(None, k), v) => k -> v }.toMap
        assertEquals(m("a"), ELong(1): Edn)
        assertEquals(m("b"), EVector(Vector(ELong(2), ELong(3))): Edn)
        assertEquals(m("c"), ESet(Vector(EKeyword(None, "x"), EKeyword(None, "y"))): Edn)
        assertEquals(m("d"), EChar('\n'): Edn)
        assertEquals(m("e"), ESymbol(Some("my.ns"), "sym"): Edn)
        assertEquals(m("f"), ETagged(None, "inst", EStr("2026-09-23")): Edn)
        assertEquals(m("g"), EDouble(Double.PositiveInfinity): Edn)
        assertEquals(m("h"), EDec(BigDecimal("1.5")): Edn)
      case other => fail(s"parsed as $other")
    }
  }

  test("text round trip: show then parse is the identity on values") {
    val values = Vector[Edn](
      ENil, EBool(true), ELong(-42), EBig(BigInt(7)), EDouble(1.0), EDouble(-0.5),
      EStr("q\"uo\\te\n\t"), EChar(' '), EChar('('), EKeyword(Some("a.b"), "c"), ESymbol(None, "+"),
      EList(Vector(ELong(1), EList(Vector.empty))), ESet(Vector(ELong(1))),
      EMap(Vector(EKeyword(None, "k") -> EVector(Vector(ENil)))), ETagged(Some("my"), "tag", EStr("x")))
    for (v <- values) assertEquals(Edn.parse(Edn.show(v)), Right(v), Edn.show(v))
  }

  test("an integer too big for a Long reads as a BigInt, and is refused for SLong by name") {
    assertEquals(Edn.parse("99999999999999999999"), Right(EBig(BigInt("99999999999999999999")): Edn))
    assert(Edn.read[Long]("99999999999999999999").left.exists(_.contains("does not fit a Long")))
  }

  test("errors are named: a missing field, an unknown case, the wrong kind, trailing input, an open form") {
    assert(Edn.read[EdShape]("#EdShape/Triangle {}").left.exists(_.contains("unknown case 'EdShape/Triangle'")))
    assert(Edn.read[EdShape]("#EdShape/Circle {}").left.exists(_.contains("missing field :r")))
    assert(Edn.read[EdShape]("[1 2]").left.exists(_.contains("expected EdShape")))
    assert(Edn.parse("{:a 1} extra").left.exists(_.contains("trailing input")))
    assert(Edn.parse("[1 2").left.exists(_.contains("input ended inside an open form")))
    assert(Edn.parse("{:a}").left.exists(_.contains("odd number of forms")))
  }

  test("an option is nil or the value; a missing optional field reads as None") {
    assertEquals(Edn.read[EdO]("{:b 2}"), Right(EdO(None, 2)))
    assertEquals(Edn.read[EdO]("{:a 1 :b 2}"), Right(EdO(Some(1), 2)))
    assertEquals(Edn.write(EdO(None, 2)), "{:a nil :b 2}")
  }

  test("deep nesting on the default stack: text and Schema, past NativeThreshold") {
    val depth = 20000
    val deepText = "[" * depth + "]" * depth
    val parsed = Edn.parse(deepText).fold(e => fail(e), identity)
    assertEquals(Edn.show(parsed).length, deepText.length)
    val chain = (1 to 5000).foldLeft(Option.empty[EdNode])((n, i) => Some(EdNode(i, n))).get
    val back = Edn.read[EdNode](Edn.write(chain)).fold(e => fail(e), identity)
    assertEquals(Iterator.iterate(Option(back))(_.flatMap(_.next)).takeWhile(_.isDefined).size, 5000)
  }
}
