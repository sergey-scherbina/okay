package okay2.codec

import java.nio.file.Files

/**
 * The wire's codecs, compressions, negotiation and authentication,
 * without a far side (okay-py's TestWireGivens, the part that is
 * okay-codec's: `WireNegotiation` directly, where okay drives it through
 * `ForeignWorker.over`).
 */
class TestWire extends munit.FunSuite {

  private val tree: Json = Json.JObj(Vector(
    "id" -> Json.JNum(7), "ok" -> Json.JObj(Vector(
      "args" -> Json.JArr(Vector(Json.JArr(Vector(Json.JNum(1), Json.JNum(-2))))), "k" -> Json.JNum(1),
      "perform" -> Json.JStr("choose"), "price" -> Json.JNum(4.5), "none" -> Json.JNull, "yes" -> Json.JBool(true),
      "big" -> Json.JNum(1e300), "text" -> Json.JStr("чай ☕")))))

  /** a hello announcing `speaks` (or nothing) */
  private def hello(speaks: String): Json = Json.parse(s"""{"shim":1,"python":"fake"$speaks}""")
  private val speaksDeflate = ""","speaks":{"format":["json","cbor"],"compress":["deflate"]}"""

  /** the wire the negotiation settles on, as okay's `ForeignWorker.wire` names it */
  private def wire(h: Json, network: Boolean = true, name: String = "the worker")
                  (implicit f: WireFormat, c: WireCompression): Either[String, String] =
    WireNegotiation.choose(h, network, name).map {
      case None => "json/none"
      case Some((fo, co)) => s"${fo.name}/${co.name}"
    }

  test("CBOR carries the wire's tree and back, unchanged") {
    assertEquals(WireCbor.decode(WireCbor.encode(tree)), Right(tree))
    assertEquals(WireFormat.Cbor.cbor.decode(WireFormat.Cbor.cbor.encode(tree)), tree)
    assertEquals(WireFormat.json.decode(WireFormat.json.encode(tree)), tree)
  }

  test("a CBOR message cut short, at any byte, is refused") {
    val bytes = WireCbor.encode(tree)
    assertEquals((1 until bytes.length).filter(n => WireCbor.decode(bytes.dropRight(n)).isRight).toVector, Vector.empty[Int])
  }

  test("DEFLATE and zlib round-trip; a cut message and a damaged checksum are refused") {
    val d = WireCompression.Deflate.deflate
    val bytes = WireCbor.encode(tree)
    assertEquals(d.decompress(d.compress(bytes)).toVector, bytes.toVector)
    assert(scala.util.Try(d.decompress(d.compress(bytes).dropRight(3))).isFailure)
    val z = WireCompression.Zlib.zlib
    assertEquals(z.decompress(z.compress(bytes)).toVector, bytes.toVector)
    assert(scala.util.Try(z.decompress(z.compress(bytes).dropRight(2))).isFailure)
    val bad = z.compress(bytes)
    bad(bad.length - 1) = (bad(bad.length - 1) ^ 1).toByte
    assert(scala.util.Try(z.decompress(bad)).isFailure)
  }

  test("a kept Deflater and Inflater carry nothing between messages, and a refusal does not poison the next") {
    for (c <- Seq(WireCompression.Deflate.deflate, WireCompression.Zlib.zlib)) {
      val a = WireCbor.encode(tree)
      val b = "x".repeat(40000).getBytes
      for (m <- Seq(a, b, a, Array.emptyByteArray, b)) assertEquals(c.decompress(c.compress(m)).toVector, m.toVector)
      assert(scala.util.Try(c.decompress(c.compress(b).dropRight(3))).isFailure)
      assertEquals(c.decompress(c.compress(a)).toVector, a.toVector)
      val many = (1 to 64).map(n => java.util.concurrent.CompletableFuture.supplyAsync { () =>
        val m = s"message $n ".repeat(n * 10).getBytes
        c.decompress(c.compress(m)).toVector == m.toVector
      })
      assert(many.forall(_.join()))
    }
  }

  test("a far side that did not announce the implicit format is refused by name") {
    import WireFormat.Cbor.cbor
    assert(wire(hello(""), name = "the Haskell worker").left.exists(_.contains(
      "the Haskell worker speaks the formats json; this host's implicit WireFormat is cbor")))
  }

  test("DEFLATE is the default over a network, a preference elsewhere, never in-process or on a pipe") {
    assertEquals(wire(hello(speaksDeflate)), Right("json/deflate"))
    assertEquals(WireNegotiation.configure(WireFormat.json, WireCompression.Deflate.deflate),
      """{"op":"configure","format":"json","compress":"deflate"}""")
    assertEquals(wire(hello(""","speaks":{"format":["json","cbor"],"compress":[]}""")), Right("json/none"))
    assertEquals(wire(hello("")), Right("json/none"))
    assertEquals(wire(hello(speaksDeflate), network = false), Right("json/none"))
  }

  test("the default is an ORDER: deflate where spoken, else zlib (R, unboxed), else nothing") {
    assertEquals(wire(hello(""","speaks":{"compress":["zlib","deflate"]}""")), Right("json/deflate"))
    assertEquals(wire(hello(""","speaks":{"format":["json","cbor"],"compress":"zlib"}""")), Right("json/zlib"))
    assertEquals(wire(hello(""","speaks":{"compress":["zlib"]}"""), network = false), Right("json/none"))
  }

  test("Off turns it off; an EXPLICIT Deflate or Zlib is strict, and used off the network too") {
    locally {
      import WireCompression.Off.off
      assertEquals(wire(hello(speaksDeflate)), Right("json/none"))
    }
    locally {
      import WireCompression.Deflate.deflate
      assert(wire(hello(""), name = "the old worker").left.exists(_.contains(
        "the old worker speaks the compressions none; this host's implicit WireCompression is deflate")))
      assertEquals(wire(hello(speaksDeflate), network = false), Right("json/deflate"))
    }
    locally {
      import WireCompression.Zlib.zlib
      assert(wire(hello(speaksDeflate), name = "the Go worker").left.exists(_.contains(
        "the Go worker speaks the compressions none, deflate; this host's implicit WireCompression is zlib")))
    }
  }

  test("CBOR with the default compression: both, where both are spoken; CBOR alone otherwise") {
    import WireFormat.Cbor.cbor
    assertEquals(wire(hello(speaksDeflate)), Right("cbor/deflate"))
    assertEquals(wire(hello(""","speaks":{"format":["json","cbor"],"compress":[]}""")), Right("cbor/none"))
  }

  test("frames: Arrow where spoken, JSON otherwise, a strict Arrow refused by name") {
    val arrow = hello(""","speaks":{"frames":["arrow"]}""")
    assertEquals(WireNegotiation.chooseFrames(arrow, "w"), Right(true))
    assertEquals(WireNegotiation.chooseFrames(hello(""), "w"), Right(false))
    assertEquals(WireNegotiation.chooseFrames(arrow, "w")(FrameFormat.Json.json), Right(false))
    assert(WireNegotiation.chooseFrames(hello(""), "the R worker")(FrameFormat.Arrow.arrow).left.exists(_.contains("the R worker speaks the frames json")))
    assertEquals(WireNegotiation.configure(WireFormat.json, WireCompression.Off.off, arrow = true),
      """{"op":"configure","format":"json","compress":"none","frames":"arrow"}""")
  }

  test("confirmed: ok, a refusal, a closed wire") {
    val (f, c) = (WireFormat.json, WireCompression.Off.off)
    assertEquals(WireNegotiation.confirmed("w", f, c, Some(Json.parse("""{"id":null,"ok":{}}"""))), Right(()))
    assert(WireNegotiation.confirmed("w", f, c, Some(Json.parse("""{"error":"no"}"""))).left.exists(_.contains("w refused the configuration json/none")))
    assert(WireNegotiation.confirmed("w", f, c, None).left.exists(_.contains("closed the wire")))
  }

  test("WireAuth.mac is HMAC-SHA256: RFC 4231's test case 2") {
    assertEquals(WireAuth.mac("Jefe".getBytes, "what do ya want for nothing?"),
      "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
  }

  /** a server with a secret: it answers the auth as the Go and Rust
   * servers do, or, `lying`, with a mac that proves nothing */
  private final class Guarded(secret: String, lying: Boolean = false) {
    val ns = "00112233445566778899aabbccddeeff"
    var asked = Vector.empty[String]
    val hello: Json = Json.parse(s"""{"shim":1,"python":"fake","auth":{"scheme":"hmac-sha256","nonce":"$ns"}}""")
    def roundTrip(line: String): Option[Json] = {
      asked :+= line
      Json.parse(line) match {
        case Json.JObj(fs) =>
          val m = fs.toMap
          val nc = m.get("nonce").collect { case Json.JStr(s) => s }.getOrElse("")
          val mac = m.get("mac").collect { case Json.JStr(s) => s }.getOrElse("")
          if (mac != WireAuth.mac(secret.getBytes, s"okay-wire client|$ns|$nc"))
            Some(Json.parse("""{"id":null,"condition":{"kind":"PermissionError","message":"authentication refused"}}"""))
          else {
            val proof = if (lying) "00" * 32 else WireAuth.mac(secret.getBytes, s"okay-wire server|$ns|$nc")
            Some(Json.parse(s"""{"id":null,"ok":{"mac":"$proof"}}"""))
          }
        case _ => None
      }
    }
    def auth(name: String)(implicit a: WireAuth): Either[String, Unit] = WireNegotiation.authenticate(hello, name, roundTrip)
  }

  test("WireAuth: both sides prove the secret, and the host's proof is sent once, the secret never") {
    implicit val auth: WireAuth = WireAuth.secret("tea for two".getBytes)
    val g = new Guarded("tea for two")
    assertEquals(g.auth("w"), Right(()))
    assertEquals(g.asked.size, 1)
    assert(g.asked.head.startsWith("""{"op":"auth","nonce":"""), g.asked.head)
    assert(!g.asked.head.contains("tea for two"), "the secret itself never crosses")
  }

  test("WireAuth is MUTUAL: a server whose answer does not prove the secret is refused; a wrong secret too") {
    implicit val auth: WireAuth = WireAuth.secret("tea for two".getBytes)
    assert(new Guarded("tea for two", lying = true).auth("the relay").left.exists(_.contains(
      "the relay answered with a mac that does not prove the secret from a secret in the program")))
    assert(new Guarded("other").auth("w").left.exists(_.contains("w refused this host's authentication")))
  }

  test("WireAuth refusals name what is missing, on each side") {
    assert(new Guarded("x").auth("the Go worker")(WireAuth.none).left.exists(_.contains(
      "the Go worker requires hmac-sha256 authentication; this host has no implicit WireAuth")))
    implicit val auth: WireAuth = WireAuth.fromEnv("OKAY_TEST_NO_SUCH_SECRET")
    val unset = intercept[IllegalStateException](new Guarded("x").auth("w"))
    assert(unset.getMessage.contains("the wire secret's environment variable OKAY_TEST_NO_SUCH_SECRET is not set"), unset.getMessage)
    assert(WireNegotiation.authenticate(hello(""), "the old worker", _ => None).left.exists(_.contains(
      "from the environment variable OKAY_TEST_NO_SUCH_SECRET) requires the old worker to authenticate; it announced none")))
  }

  test("WireAuth.fromFile drops the newline an editor adds") {
    val f = Files.createTempFile("okay-secret", ".txt")
    val _ = Files.writeString(f, "tea for two\n")
    implicit val auth: WireAuth = WireAuth.fromFile(f)
    assertEquals(new Guarded("tea for two").auth("w"), Right(()))
  }

  test("byName picks the same implicits an import would, and refuses an unknown name by naming it") {
    assertEquals(WireFormat.byName("json").map(_.name), Right("json"))
    assertEquals(WireFormat.byName("cbor").map(_.name), Right("cbor"))
    assertEquals(WireFormat.byName("bson"), Left("unknown wire format 'bson' (json, cbor)"))
    assertEquals(WireCompression.byName("auto").map(_.name), Right(WireCompression.preferred.name))
    assertEquals(WireCompression.byName("none").map(_.name), Right("none"))
    assertEquals(WireCompression.byName("deflate").map(_.name), Right("deflate"))
    assertEquals(WireCompression.byName("zlib").map(_.name), Right("zlib"))
    assertEquals(WireCompression.byName("gzip"), Left("unknown wire compression 'gzip' (auto, none, deflate, zlib)"))
    assertEquals(FrameFormat.byName("auto"), Right(FrameFormat.preferred))
    assertEquals(FrameFormat.byName("json"), Right(FrameFormat.Json.json))
    assertEquals(FrameFormat.byName("arrow"), Right(FrameFormat.Arrow.arrow))
    assertEquals(FrameFormat.byName("parquet"), Left("unknown frame format 'parquet' (auto, json, arrow)"))
  }

  test("WireChoice.default is exactly what the implicits pick with no import; named composes the three") {
    val d = WireChoice.default
    assertEquals(d.format.name, implicitly[WireFormat].name)
    assertEquals(d.compression.name, implicitly[WireCompression].name)
    assertEquals(d.frames, implicitly[FrameFormat])
    assertEquals(d.deadline, implicitly[WireDeadline])
    assertEquals(WireChoice.named(format = "cbor", compression = "none", frames = "json").map(w => (w.format.name, w.compression.name, w.frames)),
      Right(("cbor", "none", FrameFormat.Json.json)))
    assertEquals(WireChoice.named(format = "yaml"), Left("unknown wire format 'yaml' (json, cbor)"))
  }

  test("frames on a byte stream: lines until a switch, then length-prefixed messages, nothing lost between") {
    val out = new java.io.ByteArrayOutputStream()
    WireFrames.writeLine(out, """{"op":"configure"}""")
    WireFrames.writeFrame(out, Array[Byte](1, 2, 3))
    val in = new java.io.BufferedInputStream(new java.io.ByteArrayInputStream(out.toByteArray))
    assertEquals(WireFrames.readLine(in), Some("""{"op":"configure"}"""))
    assertEquals(WireFrames.readFrame(in).map(_.toVector), Some(Vector[Byte](1, 2, 3)))
    assertEquals(WireFrames.readFrame(in), None)
  }

  test("WireJson.whole refuses a line cut short, even one that still parses as a smaller object") {
    assertEquals(WireJson.whole("""{"a":[1,{"b":2}]}"""), Json.parse("""{"a":[1,{"b":2}]}"""))
    intercept[IllegalStateException](WireJson.whole("""{"a":[1,{"b":2}"""))
    intercept[IllegalStateException](WireJson.whole("""{"a":{"b":2}}, "c":1}"""))
  }
}
