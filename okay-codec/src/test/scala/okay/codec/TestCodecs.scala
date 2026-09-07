package okay.codec

/**
 * The seam (staging-seam): every generic door answers through
 * `Codecs`, the interpreter until something is installed, and an
 * installed provider is what the doors answer from then on — on every
 * platform, with no compiler in sight.
 */
class TestCodecs extends munit.FunSuite {

  final case class P(a: Int, b: Option[String], c: List[Double]) derives Schema
  private val p = P(1, Some("x"), List(1.5))

  override def afterEach(ctx: AfterEach): Unit = Codecs.reset()

  test("the default is the interpreter, and its answers are the fold's") {
    assertEquals(Codecs.provider.name, "interpreter")
    assertEquals(Codecs.writeJson(p), Json.write(p))
    assertEquals(Codecs.readJson[P](Json.write(p)), Right(p))
    assertEquals(Codecs.readJson[P]("""{"a":"one"}"""), Json.read[P]("""{"a":"one"}"""))
    assert(java.util.Arrays.equals(Codecs.writeCbor(p), Cbor.write(p)))
    assertEquals(Codecs.readCbor[P](Cbor.write(p)), Right(p))
    assertEquals(Codecs.json(summon[Schema[P]]).decode(Json.parse("""[]""")), Json.decode(summon[Schema[P]])(Json.parse("[]")))
  }

  test("an installed provider is what every door answers; reset returns to the interpreter") {
    var asked = Vector.empty[String]
    val fake = new Codecs.Provider:
      def name = "fake"
      def json[A](s: Schema[A]): JsonCodec[A] =
        asked :+= "json"; Codecs.Interpreter.json(s)
      def cbor[A](s: Schema[A]): CborCodec[A] =
        asked :+= "cbor"; Codecs.Interpreter.cbor(s)
    Codecs.install(fake)
    assertEquals(Codecs.provider.name, "fake")
    assertEquals(Codecs.writeJson(p), Json.write(p))
    assertEquals(Codecs.readCbor[P](Cbor.write(p)), Right(p))
    assertEquals(asked, Vector("json", "cbor"))
    Codecs.reset()
    assertEquals(Codecs.provider.name, "interpreter")
  }
}
