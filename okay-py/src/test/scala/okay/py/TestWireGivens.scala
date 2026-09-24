package okay.py

import okay.codec.Json

/** stage 5a's codecs and refusals, without a far side (default gate) */
class TestWireGivens extends munit.FunSuite:

  private val tree = Json.JObj(Vector(
    "id" -> Json.JNum(7), "ok" -> Json.JObj(Vector(
      "args" -> Json.JArr(Vector(Json.JArr(Vector(Json.JNum(1), Json.JNum(-2))))), "k" -> Json.JNum(1),
      "perform" -> Json.JStr("choose"), "price" -> Json.JNum(4.5), "none" -> Json.JNull, "yes" -> Json.JBool(true),
      "big" -> Json.JNum(1e300), "text" -> Json.JStr("чай ☕")))))

  test("CBOR carries the wire's tree and back, unchanged") {
    assertEquals(WireCbor.decode(WireCbor.encode(tree)), Right(tree))
  }

  test("a CBOR message cut short, at any byte, is refused") {
    val bytes = WireCbor.encode(tree)
    val accepted = (1 until bytes.length).filter(n => WireCbor.decode(bytes.dropRight(n)).isRight)
    assertEquals(accepted.toVector, Vector.empty)
  }

  test("DEFLATE round-trips, and an inflated message cut short is refused") {
    val d = WireCompression.Deflate.deflate
    val bytes = WireCbor.encode(tree)
    assertEquals(d.decompress(d.compress(bytes)).toVector, bytes.toVector)
    val cut = d.compress(bytes).dropRight(3)
    assert(scala.util.Try(d.decompress(cut)).isFailure)
  }

  test("a far side that did not announce the given format is refused by name") {
    import WireFormat.Cbor.given
    val link = new WireLink:
      def hello(): Option[String] = Some("""{"shim":6,"python":"haskell"}""")
      def roundTrip(line: String): Option[String] = None
      def exchange(message: Array[Byte]): Option[Array[Byte]] = None
      def close(): Unit = ()
    val e = intercept[IllegalStateException](ForeignWorker.over(link, "the Haskell worker"))
    assert(e.getMessage.contains("speaks the formats json; this host's given WireFormat is cbor"), e.getMessage)
  }
