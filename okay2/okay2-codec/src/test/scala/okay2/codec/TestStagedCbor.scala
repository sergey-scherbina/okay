package okay2.codec

import StagedModels._

/**
 * The STAGED fold mode's CBOR twin (okay-codec's TestStagedCbor): the
 * staged codec and the interpreted fold agree item-for-item on encode
 * and Left-for-Left on decode — including the totality doors, the
 * delegation cases, and CBOR's own hazard: field REORDER and duplicate
 * keys, since a CBOR map carries no order guarantee.
 */
class TestStagedCbor extends munit.FunSuite {

  private val orderSchema: Schema[SgOrder] = implicitly
  private val drawingSchema: Schema[SgDrawing] = implicitly
  private val contactSchema: Schema[SgContact] = implicitly

  private val orderCodec = Staged.cbor[SgOrder]
  private val drawingCodec = Staged.cbor[SgDrawing]
  private val contactCodec = Staged.cbor[SgContact]
  private val treeCodec = Staged.cbor[SgTree]

  private def agree[A](codec: CborCodec[A], schema: Schema[A])(a: A)(implicit loc: munit.Location): Unit = {
    val folded = Cbor.write(a)(schema)
    assertEquals(codec.encode(a).toVector, folded.toVector, "encode disagrees")
    assertEquals(codec.decode(folded), Cbor.read[A](folded)(schema), "decode disagrees")
    assertEquals(codec.decode(codec.encode(a)), Right(a), "round trip")
  }

  test("products, nested products, Option, List, Vector: encode item-for-item, decode Left-for-Left") {
    orders.foreach(agree(orderCodec, orderSchema))
  }

  test("sums: every case, a sum inside a list and a product") {
    drawings.foreach(agree(drawingCodec, drawingSchema))
  }

  test("an iso field is delegated, and recursion folds at run time") {
    agree(contactCodec, contactSchema)(SgContact(SgEmail("a@b"), "ada"))
    agree(treeCodec, SgTree.schema)(SgTree("root", List(SgTree("a", Nil), SgTree("b", List(SgTree("c", Nil))))))
  }

  test("the totality doors on a hand-built map: absent with default, absent optional, absent required") {
    val out = new Cbor.Out
    out.mapHeader(6)
    out.text("id"); out.integer(1L)
    out.text("user"); out.text("u")
    out.text("amount"); out.double(1.0)
    out.text("active"); out.bool(true)
    out.text("tags"); out.arrayHeader(0)
    out.text("addr")
    out.mapHeader(2); out.text("city"); out.text("c"); out.text("zip"); out.text("z")
    val minimal = out.toArray
    assertEquals(orderCodec.decode(minimal), Cbor.read[SgOrder](minimal))
    assertEquals(orderCodec.decode(minimal), Right(SgOrder(1L, "u", 1.0, true, Nil, SgAddress("c", "z", None), None)))
  }

  test("CBOR's own hazard: field order and duplicate keys are not the wire's problem") {
    val out = new Cbor.Out
    // the canonical fields REVERSED, plus a duplicate "id" that loses
    // to the one that arrives last
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
    assertEquals(orderCodec.decode(reordered), Cbor.read[SgOrder](reordered))
    assertEquals(orderCodec.decode(reordered), Right(orders.head))
  }

  test("wrong shapes refuse with the fold's own words; lengths past the bytes left too") {
    val notAMap = Cbor.write(42)
    assertEquals(orderCodec.decode(notAMap), Cbor.read[SgOrder](notAMap))
    val unknownOnly = { val o = new Cbor.Out; o.mapHeader(1); o.text("bogus"); o.integer(1L); o.toArray }
    assertEquals(orderCodec.decode(unknownOnly), Cbor.read[SgOrder](unknownOnly))
    val truncated = Cbor.write(orders.head).dropRight(3)
    assertEquals(orderCodec.decode(truncated), Cbor.read[SgOrder](truncated))
    def unhex(s: String): Array[Byte] = s.grouped(2).map(Integer.parseInt(_, 16).toByte).toArray
    val box = Staged.cbor[IntBox]
    assert(box.decode(unhex("bb8000000000000001" + "616e01")).isLeft)
    assert(box.decode(unhex("a2" + "616e" + "01" + "6178" + "5b0000000100000001" + "01")).isLeft)
    assert(box.decode(Cbor.write(LongBox(1L << 32))).isLeft)
    assertEquals(box.decode(Cbor.write(IntBox(Int.MaxValue))), Right(IntBox(Int.MaxValue)))
  }
}
