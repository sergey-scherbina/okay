package okay2.codec

import StagedModels._

/**
 * The STAGED fold mode (okay-codec's TestStaged): the staged codec and
 * the interpreted fold are ONE algebra in two modes, so they must agree
 * byte-for-byte on every encode and Left-for-Left on every decode —
 * including the totality doors (absent field, declared default, damaged
 * optional, damaged elements, unknown case, wrong shape) and the
 * delegation cases (an iso field, a recursive type).
 */
class TestStaged extends munit.FunSuite {

  private val orderSchema: Schema[SgOrder] = implicitly
  private val drawingSchema: Schema[SgDrawing] = implicitly
  private val contactSchema: Schema[SgContact] = implicitly

  private val orderCodec = Staged.json[SgOrder]
  private val drawingCodec = Staged.json[SgDrawing]
  private val contactCodec = Staged.json[SgContact]
  private val treeCodec = Staged.json[SgTree]

  private def agree[A](codec: JsonCodec[A], schema: Schema[A])(a: A)(implicit loc: munit.Location): Unit = {
    val folded = Json.encode(schema)(a)
    assertEquals(codec.encode(a), folded, "encode disagrees")
    val j = Json.parse(folded)
    assertEquals(codec.decode(j), Json.decode(schema)(j), "decode disagrees")
    assertEquals(codec.decode(j), Right(a), "round trip")
  }

  private def agreeText[A](codec: JsonCodec[A], schema: Schema[A])(text: String)(implicit loc: munit.Location): Unit = {
    val j = Json.parse(text)
    assertEquals(codec.decode(j), Json.decode(schema)(j), s"decode disagrees on $text")
  }

  test("products, nested products, Option, List, Vector: encode byte-for-byte, decode Left-for-Left") {
    orders.foreach(agree(orderCodec, orderSchema))
  }

  test("the totality doors: absent with default, absent optional, absent required, damaged optional") {
    val s = orderSchema
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z","line":null},"note":"n"}""")
    agreeText(orderCodec, s)("""{"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"},"note":""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":["a",{"bad":},"b"],"addr":{"city":"c","zip":"z"}}""")
  }

  test("wrong shapes refuse with the fold's own words") {
    val s = orderSchema
    agreeText(orderCodec, s)("""[1,2]""")
    agreeText(orderCodec, s)("""{"id":"one","user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":"yes","tags":[],"addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":"none","addr":{"city":"c","zip":"z"}}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":7}""")
    agreeText(orderCodec, s)("""{"id":1,"user":"u","amount":1,"active":true,"tags":[],"addr":{"city":"c","zip":"z"},"scores":[1,"x"]}""")
  }

  test("sums: every case, unknown case, a sum inside a list and a product") {
    drawings.foreach(agree(drawingCodec, drawingSchema))
    agreeText(drawingCodec, drawingSchema)("""{"shapes":[{"Circle":{"r":1}},{"Triangle":{}}],"main":{"Dot":{}}}""")
    agreeText(drawingCodec, drawingSchema)("""{"shapes":[],"main":{"Circle":{"r":1},"Dot":{}}}""")
    agreeText(drawingCodec, drawingSchema)("""{"shapes":[],"main":5}""")
  }

  test("an iso field is delegated: the newtype travels as its underlying type in both modes") {
    agree(contactCodec, contactSchema)(SgContact(SgEmail("a@b"), "ada"))
    assertEquals(contactCodec.encode(SgContact(SgEmail("a@b"), "ada")), """{"email":"a@b","name":"ada"}""")
    agreeText(contactCodec, contactSchema)("""{"email":7,"name":"ada"}""")
  }

  test("the staged path is the one TAKEN for a derived schema, and not for a wrapped one") {
    assert(Staged.productShape(orderSchema, List("id", "user", "amount", "active", "tags", "addr", "note", "priority", "scores")))
    assert(Staged.productShape(implicitly[Schema[SgAddress]], List("city", "zip", "line")))
    assert(Staged.sumShape(implicitly[Schema[SgShape]], List("Circle", "Square", "Dot")))
    assert(!Staged.productShape(SgEmail.schema, List("value")), "an iso is not the class's shape")
    assert(!Staged.productShape(implicitly[Schema[SgAddress]], List("zip", "city", "line")), "order is part of the shape")
  }

  test("recursion: the type inside itself folds at run time, and agrees") {
    agree(treeCodec, SgTree.schema)(SgTree("root", List(SgTree("a", Nil), SgTree("b", List(SgTree("c", Nil))))))
    agreeText(treeCodec, SgTree.schema)("""{"label":"r","kids":[{"label":"a"}]}""")
  }

  test("an Int field refuses a number past Int, staged as folded") {
    val box = Staged.json[IntBox]
    for (text <- List("""{"n":3000000000}""", """{"n":1.5}""")) {
      assert(box.decode(Json.parse(text)).isLeft, text)
      assertEquals(box.decode(Json.parse(text)), Json.read[IntBox](text))
    }
  }
}
