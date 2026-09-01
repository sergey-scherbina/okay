package okay.security

import okay.codec.Json

/**
 * ES256 with real keys — the JVM half of the stage. The wire is the
 * proof: a signature segment of exactly 64 bytes says JOSE, and JCA
 * accepting our DER on the way back closes the loop in both
 * directions. The confusion battery extends stage 0's: the key still
 * decides the algorithm, now with three kinds in the ring.
 */
class TestEs256Jvm extends munit.FunSuite {

  val now = 1_700_000_000L
  def claims(exp: Long = now + 600): Claims = Claims(
    subject = Some("u1"), audience = Vector("api"), expires = Some(exp),
    json = Json.JObj(Vector("name" -> Json.JStr("Ada"))))

  private def ecPair() =
    val g = java.security.KeyPairGenerator.getInstance("EC")
    g.initialize(java.security.spec.ECGenParameterSpec("secp256r1"))
    g.generateKeyPair()

  lazy val ec = ecPair()
  lazy val key = Keys.ecPair(ec.getPublic, ec.getPrivate)
  lazy val pub = Keys.ecPublic(ec.getPublic)

  test("ES256 round-trips, and the wire carries 64 raw bytes — JOSE, not DER") {
    val t = Jwt.sign(claims(), key, kid = Some("k1"))
    val sig = java.util.Base64.getUrlDecoder.decode(t.split('.')(2))
    assertEquals(sig.length, 64)
    Jwt.verify(t, _ => Some(pub), Some("api"), now) match
      case Verified.Ok(p) => assertEquals(p.id, "u1"); assertEquals(p.name, "Ada")
      case Verified.No(why) => fail(why)
  }

  test("tampering, a stranger's key, a mangled signature — refused, never thrown") {
    val t = Jwt.sign(claims(), key)
    val Array(h, p, s) = t.split('.')
    val forged = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(
      """{"sub":"u1","aud":"api","scope":"admin"}""".getBytes("UTF-8"))
    assert(Jwt.verify(h + "." + forged + "." + s, _ => Some(pub), Some("api"), now)
      .isInstanceOf[Verified.No], "tampered payload")

    val stranger = ecPair()
    assert(Jwt.verify(t, _ => Some(Keys.ecPublic(stranger.getPublic)), Some("api"), now)
      .isInstanceOf[Verified.No], "stranger's key")

    // 63 bytes is not a JOSE ES256 signature; the refusal is a value
    val short = java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(new Array[Byte](63))
    assert(Jwt.verify(h + "." + p + "." + short, _ => Some(pub), Some("api"), now)
      .isInstanceOf[Verified.No], "63-byte signature")
  }

  test("confusion refused all ways round: the key decides, the token only agrees") {
    val secret = "a-shared-secret-of-decent-length".getBytes("UTF-8")
    val rsa =
      val g = java.security.KeyPairGenerator.getInstance("RSA")
      g.initialize(2048); g.generateKeyPair()
    val es = Jwt.sign(claims(), key)
    val hs = Jwt.sign(claims(), Jwt.Key.Hmac(secret))
    val rs = Jwt.sign(claims(), Keys.rsaPair(rsa.getPublic, rsa.getPrivate))
    for (t, k, name) <- Seq(
      (es, Jwt.Key.Hmac(secret), "ES256 token vs HMAC key"),
      (es, Keys.rsaPublic(rsa.getPublic), "ES256 token vs RSA key"),
      (hs, pub, "HS256 token vs EC key"),
      (rs, pub, "RS256 token vs EC key"))
    do assert(Jwt.verify(t, _ => Some(k), Some("api"), now).isInstanceOf[Verified.No], name)
  }

  test("a public key cannot sign — a broken invariant, so it throws") {
    intercept[IllegalArgumentException](Jwt.sign(claims(), pub))
  }

  test("JWKS: the EC entry verifies; a damaged and a P-384 entry are skipped around it") {
    val point = ec.getPublic.asInstanceOf[java.security.interfaces.ECPublicKey].getW
    def coord(b: java.math.BigInteger) =
      java.util.Base64.getUrlEncoder.withoutPadding.encodeToString(BigInt(b).toByteArray.dropWhile(_ == 0))
    val x = coord(point.getAffineX); val y = coord(point.getAffineY)
    val jwks = Json.parse(s"""{"keys":[
      {"kty":"EC","crv":"P-256","kid":"good","x":"$x","y":"$y"},
      {"kty":"EC","crv":"P-256","kid":"noy","x":"$x"},
      {"kty":"EC","crv":"P-384","kid":"wrong-curve","x":"$x","y":"$y"}
    ]}""")
    val keys = Jwks.parse(jwks)
    assertEquals(keys.keySet, Set("good"))
    val t = Jwt.sign(claims(), key, kid = Some("good"))
    assert(Jwt.verify(t, keys.get, Some("api"), now).isInstanceOf[Verified.Ok])
  }
}
