package okay.codec

/**
 * specs/codecs.md, the STAGED fold mode's CBOR twin: the staged
 * codec and the interpreted fold are ONE algebra in two modes, so
 * they must agree byte-for-byte on every encode and Left-for-Left on
 * every decode — including the totality doors, the delegation cases,
 * and (CBOR's own hazard, absent in JSON) field REORDER and
 * duplicate keys, since a CBOR map carries no order guarantee.
 */
class TestStagedCbor extends munit.FunSuite {

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

  final case class Email(value: String)
  given Schema[Email] = Schema.wrap[Email, String](Email(_), _.value)
  final case class Contact(email: Email, name: String)
  given Schema[Contact] = Schema.derived

  final case class Tree(label: String, kids: List[Tree])
  given Schema[Tree] = Schema.derived

  val orderCodec = Staged.cbor[Order]
  val drawingCodec = Staged.cbor[Drawing]
  val contactCodec = Staged.cbor[Contact]
  val treeCodec = Staged.cbor[Tree]

  def agree[A](codec: CborCodec[A], schema: Schema[A])(a: A)(using munit.Location): Unit =
    val staged = codec.encode(a)
    val folded = Cbor.write(a)(using schema)
    assertEquals(staged.toVector, folded.toVector, "encode disagrees")
    assertEquals(codec.decode(folded), Cbor.read(folded)(using schema), "decode disagrees")
    assertEquals(codec.decode(staged), Right(a), "round trip")

  val orders = Seq(
    Order(42L, "ada", 12.5, true, List("new", "vip"), Address("Kyiv", "01001", None), Some("leave at door")),
    Order(0L, "", 0.0, false, Nil, Address("", "", Some("")), None),
    Order(-7L, "q\"uo\\te\n", -1e300, true, List("a\tb"), Address("x", "y", Some("z")), Some("")),
    Order(1L, "u", 3.0, true, List("t"), Address("c", "z", None), None, priority = 9, scores = Vector()))

  test("products, nested products, Option, List, Vector: encode item-for-item, decode Left-for-Left") {
    orders.foreach(agree(orderCodec, summon[Schema[Order]]))
  }

  test("sums: every case, a sum inside a list and a product") {
    val s = summon[Schema[Drawing]]
    Seq(
      Drawing(List(Shape.Circle(1.0), Shape.Dot, Shape.Square(2.0, "s")), Shape.Dot),
      Drawing(Nil, Shape.Circle(0.5)),
      Drawing(List(Shape.Square(1, "a\"b")), Shape.Square(2, "")))
      .foreach(agree(drawingCodec, s))
  }

  test("an Iso field is delegated: the newtype travels as its underlying type in both modes") {
    val s = summon[Schema[Contact]]
    agree(contactCodec, s)(Contact(Email("a@b"), "ada"))
  }

  test("recursion: the type inside itself folds at run time, and agrees") {
    val s = summon[Schema[Tree]]
    agree(treeCodec, s)(Tree("root", List(Tree("a", Nil), Tree("b", List(Tree("c", Nil))))))
  }

  test("the totality doors on a hand-built map: absent with default, absent optional, absent required") {
    // a hand-assembled map, all required fields, no priority/scores/note
    val minimal =
      val out = new Cbor.Out
      out.mapHeader(6)
      out.text("id"); out.integer(1L)
      out.text("user"); out.text("u")
      out.text("amount"); out.double(1.0)
      out.text("active"); out.bool(true)
      out.text("tags"); out.arrayHeader(0)
      out.text("addr")
      out.mapHeader(2); out.text("city"); out.text("c"); out.text("zip"); out.text("z")
      out.toArray
    assertEquals(orderCodec.decode(minimal), Cbor.read[Order](minimal))
    assertEquals(orderCodec.decode(minimal), Right(Order(1L, "u", 1.0, true, Nil, Address("c", "z", None), None)))
  }

  test("CBOR's own hazard: field order and duplicate keys are not the wire's problem") {
    val canonical = orders.head
    val out = new Cbor.Out
    // the same fields as the canonical encoding, REVERSED, plus a
    // duplicate "id" that must lose to the one that arrives last —
    // a shape no encoder here produces, on purpose
    out.mapHeader(8)
    out.text("note"); out.text("leave at door")
    out.text("addr"); out.mapHeader(2); out.text("city"); out.text("Kyiv"); out.text("zip"); out.text("01001")
    out.text("tags"); out.arrayHeader(2); out.text("new"); out.text("vip")
    out.text("active"); out.bool(true)
    out.text("amount"); out.double(12.5)
    out.text("user"); out.text("ada")
    out.text("id"); out.integer(0L)
    out.text("id"); out.integer(42L)
    val reordered = out.toArray
    assertEquals(orderCodec.decode(reordered), Cbor.read[Order](reordered))
    assertEquals(orderCodec.decode(reordered), Right(canonical))
  }

  test("wrong shapes refuse with the fold's own words") {
    val notAMap = Cbor.write(42)(using Schema.SInt)
    assertEquals(orderCodec.decode(notAMap), Cbor.read[Order](notAMap))
    val unknownField =
      val out = new Cbor.Out
      out.mapHeader(1); out.text("bogus"); out.integer(1L)
      out.toArray
    assertEquals(orderCodec.decode(unknownField), Cbor.read[Order](unknownField))
    val truncated = Cbor.write(orders.head).dropRight(3)
    assertEquals(orderCodec.decode(truncated), Cbor.read[Order](truncated))
  }

  test("the staged path is the one TAKEN for a derived schema, and not for a wrapped one") {
    assert(Staged.productShape(summon[Schema[Order]], List("id", "user", "amount", "active", "tags", "addr", "note", "priority", "scores")))
    assert(Staged.sumShape(summon[Schema[Shape]], List("Circle", "Square", "Dot")))
    assert(!Staged.productShape(summon[Schema[Email]], List("value")), "an Iso is not the Mirror's shape")
  }
}
