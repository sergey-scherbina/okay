package okay2.codec

/**
 * The seam (okay-codec's TestCodecs): every generic door answers
 * through `Codecs`, the interpreter until something is installed, and
 * an installed provider is what the doors answer from then on.
 */
final case class CdP(a: Int, b: Option[String], c: List[Double])

class TestCodecs extends munit.FunSuite {

  implicit val schemaP: Schema[CdP] = Schema.derived
  private val p = CdP(1, Some("x"), List(1.5))

  override def afterEach(context: AfterEach): Unit = Codecs.reset()

  test("the default is the interpreter, and its answers are the fold's") {
    assertEquals(Codecs.provider.name, "interpreter")
    assertEquals(Codecs.writeJson(p), Json.write(p))
    assertEquals(Codecs.readJson[CdP](Json.write(p)), Right(p))
    assertEquals(Codecs.readJson[CdP]("""{"a":"one"}"""), Json.read[CdP]("""{"a":"one"}"""))
    assertEquals(Codecs.readStrict[CdP](Json.write(p)), Right(p))
    assert(java.util.Arrays.equals(Codecs.writeCbor(p), Cbor.write(p)))
    assertEquals(Codecs.readCbor[CdP](Cbor.write(p)), Right(p))
    assertEquals(Codecs.json(schemaP).decode(Json.parse("[]")), Json.decode(schemaP)(Json.parse("[]")))
  }

  test("an installed provider is what every door answers; reset returns to the interpreter") {
    var asked = Vector.empty[String]
    val fake = new Codecs.Provider {
      def name = "fake"
      def json[A](s: Schema[A]): JsonCodec[A] = { asked :+= "json"; Codecs.Interpreter.json(s) }
      def cbor[A](s: Schema[A]): CborCodec[A] = { asked :+= "cbor"; Codecs.Interpreter.cbor(s) }
    }
    Codecs.install(fake)
    assertEquals(Codecs.provider.name, "fake")
    assertEquals(Codecs.writeJson(p), Json.write(p))
    assertEquals(Codecs.readCbor[CdP](Cbor.write(p)), Right(p))
    assertEquals(asked, Vector("json", "cbor"))
    Codecs.reset()
    assertEquals(Codecs.provider.name, "interpreter")
  }
}
