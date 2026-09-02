package okay.codec

import Json.*

/**
 * specs/codecs.md, the STAGED fold mode: the staged codec and the
 * interpreted fold are ONE algebra in two modes, so they must agree
 * byte-for-byte on every encode and Left-for-Left on every decode —
 * including the totality doors (absent field, declared default,
 * damaged optional, damaged elements, unknown case, wrong shape) and
 * the delegation cases (an Iso field, a recursive type).
 */
class TestStaged extends munit.FunSuite {

  final case class Address(city: String, zip: String, line: Option[String])
  final case class Order(id: Long, user: String, amount: Double, active: Boolean,
                         tags: List[String], addr: Address, note: Option[String],
                         priority: Int = 3, scores: Vector[Double] = Vector(1.5))
  given Schema[Address] = Schema.derived
  given Schema[Order] = Schema.derived

  enum Shape:
    case Circle(r: Double)
    case Square(side: Double, name: String)
    case Dot
  given Schema[Shape] = Schema.derived
  final case class Drawing(shapes: List[Shape], main: Shape)
  given Schema[Drawing] = Schema.derived

  // a newtype: travels as its underlying String in BOTH modes
  final case class Email(value: String)
  given Schema[Email] = Schema.wrap[Email, String](Email(_), _.value)
  final case class Contact(email: Email, name: String)
  given Schema[Contact] = Schema.derived

  // recursion: the type meets itself, the staged code delegates the inner fold
  final case class Tree(label: String, kids: List[Tree])
  given Schema[Tree] = Schema.derived

  val orderCodec = Staged.json[Order]
  val drawingCodec = Staged.json[Drawing]
  val contactCodec = Staged.json[Contact]
  val treeCodec = Staged.json[Tree]

  def agree[A](codec: JsonCodec[A], schema: Schema[A])(a: A)(using munit.Location): Unit =
    val staged = codec.encode(a)
    val folded = Json.encode(schema)(a)
    assertEquals(staged, folded, "encode disagrees")
    val j = Json.parse(folded)
    assertEquals(codec.decode(j), Json.decode(schema)(j), "decode disagrees")
    assertEquals(codec.decode(j), Right(a), "round trip")

  def agreeText[A](codec: JsonCodec[A], schema: Schema[A])(text: String)(using munit.Location): Unit =
    val j = Json.parse(text)
    assertEquals(codec.decode(j), Json.decode(schema)(j), s"decode disagrees on $text")

  val orders = Seq(
    Order(42L, "ada", 12.5, true, List("new", "vip"), Address("Kyiv", "01001", None), Some("leave at door")),
    Order(0L, "", 0.0, false, Nil, Address("", "", Some("")), None),
    Order(-7L, "q\"uo\\te\n", -1e300, true, List("a\tb"), Address("x", "y", Some("z")), Some("")),
    Order(1L, "u", 3.0, true, List("t"), Address("c", "z", None), None, priority = 9, scores = Vector()))

  test("products, nested products, Option, List, Vector: encode byte-for-byte, decode Left-for-Left") {
    orders.foreach(agree(orderCodec, summon[Schema[Order]]))
  }

  test("the totality doors: absent with default, absent optional, absent required, damaged optional") {
    val s = summon[Schema[Order]]
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z","line":null},"note":"n"}""")
    agreeText(orderCodec, s)("""{"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"},"note":""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":["a",{"bad":},"b"],"addr":{"city":"c","zip":"z"}}""")
  }

  test("wrong shapes refuse with the fold's own words") {
    val s = summon[Schema[Order]]
    agreeText(orderCodec, s)("""[1,2]""")
    agreeText(orderCodec, s)("""{"id":"one","user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":"yes","tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":"none","addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":7}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"},"scores":[1,"x"]}""")
  }

  test("sums: every case, unknown case, a sum inside a list and a product") {
    val s = summon[Schema[Drawing]]
    Seq(
      Drawing(List(Shape.Circle(1.0), Shape.Dot, Shape.Square(2.0, "s")), Shape.Dot),
      Drawing(Nil, Shape.Circle(0.5)),
      Drawing(List(Shape.Square(1, "a\"b")), Shape.Square(2, "")))
      .foreach(agree(drawingCodec, s))
    agreeText(drawingCodec, s)("""{"shapes":[{"Circle":{"r":1}},{"Triangle":{}}],"main":{"Dot":{}}}""")
    agreeText(drawingCodec, s)("""{"shapes":[],"main":{"Circle":{"r":1},"Dot":{}}}""")
    agreeText(drawingCodec, s)("""{"shapes":[],"main":5}""")
  }

  test("an Iso field is delegated: the newtype travels as its underlying type in both modes") {
    val s = summon[Schema[Contact]]
    agree(contactCodec, s)(Contact(Email("a@b"), "ada"))
    assertEquals(contactCodec.encode(Contact(Email("a@b"), "ada")), """{"email":"a@b","name":"ada"}""")
    agreeText(contactCodec, s)("""{"email":7,"name":"ada"}""")
  }

  test("the staged path is the one TAKEN for a derived schema, and not for a wrapped one") {
    assert(Staged.productShape(summon[Schema[Order]], List("id", "user", "amount", "active", "tags", "addr", "note", "priority", "scores")))
    assert(Staged.productShape(summon[Schema[Address]], List("city", "zip", "line")))
    assert(Staged.sumShape(summon[Schema[Shape]], List("Circle", "Square", "Dot")))
    assert(!Staged.productShape(summon[Schema[Email]], List("value")), "an Iso is not the Mirror's shape")
    assert(!Staged.productShape(summon[Schema[Address]], List("zip", "city", "line")), "order is part of the shape")
  }

  test("recursion: the type inside itself folds at run time, and agrees") {
    val s = summon[Schema[Tree]]
    agree(treeCodec, s)(Tree("root", List(Tree("a", Nil), Tree("b", List(Tree("c", Nil))))))
    agreeText(treeCodec, s)("""{"label":"r","kids":[{"label":"a"}]}""")
  }
}
