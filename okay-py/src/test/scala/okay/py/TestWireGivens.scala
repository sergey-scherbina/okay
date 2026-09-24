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

  /** a far side that announces `speaks` (or nothing) and records the
   * configure it was sent */
  private final class Fake(speaks: String, override val inProcess: Boolean = false) extends WireLink:
    var asked = Vector.empty[String]
    def hello(): Option[String] = Some(s"""{"shim":6,"python":"fake"$speaks}""")
    def roundTrip(line: String): Option[String] =
      asked :+= line
      Some("""{"id":null,"ok":{}}""")
    def exchange(message: Array[Byte]): Option[Array[Byte]] = None
    def close(): Unit = ()

  private val speaksDeflate = ""","speaks":{"format":["json","cbor"],"compress":["deflate"]}"""

  test("DEFLATE is the default: with no import, a far side that speaks it is asked for it") {
    val link = Fake(speaksDeflate)
    val w = ForeignWorker.over(link)
    assertEquals(w.wire, "json/deflate")
    assertEquals(link.asked, Vector("""{"op":"configure","format":"json","compress":"deflate"}"""))
  }

  test("the default is a preference: a far side without DEFLATE keeps the plain wire, unrefused") {
    val link = Fake(""","speaks":{"format":["json","cbor"],"compress":[]}""")
    assertEquals(ForeignWorker.over(link).wire, "json/none")
    assertEquals(link.asked, Vector.empty)
    val old = Fake("")
    assertEquals(ForeignWorker.over(old).wire, "json/none")
    assertEquals(old.asked, Vector.empty)
  }

  test("the default does not compress in-process, where a message is a memory copy") {
    val link = Fake(speaksDeflate, inProcess = true)
    assertEquals(ForeignWorker.over(link).wire, "json/none")
    assertEquals(link.asked, Vector.empty)
  }

  test("Off turns it off: a far side that speaks DEFLATE is not asked") {
    import WireCompression.Off.given
    val link = Fake(speaksDeflate)
    assertEquals(ForeignWorker.over(link).wire, "json/none")
    assertEquals(link.asked, Vector.empty)
  }

  test("an EXPLICIT Deflate is strict: refused by name where it is not spoken, used in-process too") {
    import WireCompression.Deflate.given
    val e = intercept[IllegalStateException](ForeignWorker.over(Fake(""), "the old worker"))
    assert(e.getMessage.contains("the old worker speaks the compressions none; this host's given WireCompression is deflate"), e.getMessage)
    assertEquals(ForeignWorker.over(Fake(speaksDeflate, inProcess = true)).wire, "json/deflate")
  }

  test("CBOR with the default compression: both, where both are spoken") {
    import WireFormat.Cbor.given
    val link = Fake(speaksDeflate)
    assertEquals(ForeignWorker.over(link).wire, "cbor/deflate")
    val plain = Fake(""","speaks":{"format":["json","cbor"],"compress":[]}""")
    assertEquals(ForeignWorker.over(plain).wire, "cbor/none")
    assertEquals(plain.asked, Vector("""{"op":"configure","format":"cbor","compress":"none"}"""))
  }

  test("zlib round-trips, a cut one and a damaged checksum are refused") {
    val z = WireCompression.Zlib.zlib
    val bytes = WireCbor.encode(tree)
    assertEquals(z.decompress(z.compress(bytes)).toVector, bytes.toVector)
    assert(scala.util.Try(z.decompress(z.compress(bytes).dropRight(2))).isFailure)
    val bad = z.compress(bytes)
    bad(bad.length - 1) = (bad(bad.length - 1) ^ 1).toByte
    assert(scala.util.Try(z.decompress(bad)).isFailure)
  }

  test("the default is an ORDER: deflate where spoken, else zlib (R), else nothing") {
    assertEquals(ForeignWorker.over(Fake(""","speaks":{"compress":["zlib","deflate"]}""")).wire, "json/deflate")
    val r = Fake(""","speaks":{"format":["json","cbor"],"compress":"zlib"}""")
    assertEquals(ForeignWorker.over(r).wire, "json/zlib")
    assertEquals(r.asked, Vector("""{"op":"configure","format":"json","compress":"zlib"}"""))
    assertEquals(ForeignWorker.over(Fake(""","speaks":{"compress":["zlib"]}""", inProcess = true)).wire, "json/none")
  }

  test("an explicit Zlib is strict, like Deflate") {
    import WireCompression.Zlib.given
    val e = intercept[IllegalStateException](ForeignWorker.over(Fake(speaksDeflate), "the Go worker"))
    assert(e.getMessage.contains("the Go worker speaks the compressions none, deflate; this host's given WireCompression is zlib"), e.getMessage)
  }

  test("WireAuth.mac is HMAC-SHA256: RFC 4231's test case 2") {
    assertEquals(WireAuth.mac("Jefe".getBytes, "what do ya want for nothing?"),
      "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
  }

  /** a server with a secret: it answers the auth as the Go and Rust servers
   * do, or, `lying`, with a mac that proves nothing */
  private final class Guarded(secret: String, lying: Boolean = false) extends WireLink:
    val ns = "00112233445566778899aabbccddeeff"
    var asked = Vector.empty[String]
    def hello(): Option[String] = Some(s"""{"shim":6,"python":"fake","auth":{"scheme":"hmac-sha256","nonce":"$ns"}}""")
    def roundTrip(line: String): Option[String] =
      asked :+= line
      Json.parse(line) match
        case Json.JObj(fs) =>
          val m = fs.toMap
          val nc = m.get("nonce").collect { case Json.JStr(s) => s }.getOrElse("")
          val mac = m.get("mac").collect { case Json.JStr(s) => s }.getOrElse("")
          if mac != WireAuth.mac(secret.getBytes, s"okay-wire client|$ns|$nc") then
            Some("""{"id":null,"condition":{"kind":"PermissionError","message":"authentication refused"}}""")
          else
            val proof = if lying then "00" * 32 else WireAuth.mac(secret.getBytes, s"okay-wire server|$ns|$nc")
            Some(s"""{"id":null,"ok":{"mac":"$proof"}}""")
        case _ => None
    def exchange(message: Array[Byte]): Option[Array[Byte]] = None
    def close(): Unit = ()

  test("WireAuth: both sides prove the secret, and the host's proof is sent once, before anything else") {
    given WireAuth = WireAuth.secret("tea for two".getBytes)
    val link = Guarded("tea for two")
    assertEquals(ForeignWorker.over(link).wire, "json/none")
    assertEquals(link.asked.size, 1)
    assert(link.asked.head.startsWith("""{"op":"auth","nonce":"""), link.asked.head)
    assert(!link.asked.head.contains("tea for two"), "the secret itself never crosses")
  }

  test("WireAuth is MUTUAL: a server whose answer does not prove the secret is refused") {
    given WireAuth = WireAuth.secret("tea for two".getBytes)
    val e = intercept[IllegalStateException](ForeignWorker.over(Guarded("tea for two", lying = true), "the relay"))
    assert(e.getMessage.contains("the relay answered with a mac that does not prove the secret from a secret in the program"), e.getMessage)
  }

  test("WireAuth refusals name what is missing, on each side") {
    val none = intercept[IllegalStateException](ForeignWorker.over(Guarded("x"), "the Go worker"))
    assert(none.getMessage.contains("the Go worker requires hmac-sha256 authentication; this host has no given WireAuth"), none.getMessage)
    locally {
      given WireAuth = WireAuth.fromEnv("OKAY_TEST_NO_SUCH_SECRET")
      val unset = intercept[IllegalStateException](ForeignWorker.over(Guarded("x")))
      assert(unset.getMessage.contains("the wire secret's environment variable OKAY_TEST_NO_SUCH_SECRET is not set"), unset.getMessage)
      val plain = intercept[IllegalStateException](ForeignWorker.over(Fake(""), "the old worker"))
      assert(plain.getMessage.contains("from the environment variable OKAY_TEST_NO_SUCH_SECRET) requires the old worker to authenticate; it announced none"),
        plain.getMessage)
    }
  }

  test("WireAuth.fromFile drops the newline an editor adds") {
    val f = java.nio.file.Files.createTempFile("okay-secret", ".txt")
    java.nio.file.Files.writeString(f, "tea for two\n"): Unit
    given WireAuth = WireAuth.fromFile(f)
    assertEquals(ForeignWorker.over(Guarded("tea for two")).wire, "json/none")
  }
