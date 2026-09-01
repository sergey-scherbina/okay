package okay.pg

import java.nio.charset.StandardCharsets.UTF_8

/**
 * SCRAM against the RFC 7677 test vector (user "user", password
 * "pencil", the canonical nonces) — the handshake's bytes are pinned
 * to the RFC, through the phase objects AND through the one-object
 * adapter; and the typestate's point is proven both ways: an
 * out-of-order step does not compile on the phases, and is a NAMED
 * refusal (not an NPE) on the adapter.
 */
class TestScram extends munit.FunSuite {

  val clientNonce = "rOprNGfwEbeRWgbNEkqO"
  val serverFirst =
    ("r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0," +
      "s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096").getBytes(UTF_8)
  val serverFinal = "v=6rriTRBi23WpRR/wtup+mMhUZUn/dB5nLTJRsjl95G4=".getBytes(UTF_8)
  val expectedFirst = "n,,n=user,r=rOprNGfwEbeRWgbNEkqO"
  val expectedFinal =
    "c=biws,r=rOprNGfwEbeRWgbNEkqO%hvYDpWUa2RaTCAfuxFIlj)hNlF$k0," +
      "p=dHzbZapWIk4jUhN+Ute9ytag9zjfMHgsqmmiz7AndVQ="

  test("the phases speak the RFC 7677 vector byte for byte, verification included") {
    val p0 = Scram.start("user", "pencil", clientNonce)
    assertEquals(new String(p0.message, UTF_8), expectedFirst)
    val p1 = p0.serverFirst(serverFirst)
    assertEquals(new String(p1.message, UTF_8), expectedFinal)
    p1.serverFinal(serverFinal)                       // verifies, or throws
    // a wrong signature is REFUSED — mutual auth is not decorative
    intercept[PgError](p1.serverFinal(
      "v=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=".getBytes(UTF_8)))
  }

  test("the one-object adapter speaks the same bytes, and misorder is NAMED") {
    val s = Scram("user", "pencil", clientNonce)
    // out of order BEFORE the handshake: a PgError, not an NPE
    intercept[PgError](s.verifyServerFinal(serverFinal))
    assertEquals(new String(s.clientFirst, UTF_8), expectedFirst)
    assertEquals(new String(s.clientFinal(serverFirst), UTF_8), expectedFinal)
    s.verifyServerFinal(serverFinal)
  }

  test("a replayed challenge (server nonce not extending ours) is refused") {
    val p0 = Scram.start("user", "pencil", clientNonce)
    val replayed = ("r=SOMEBODYELSESNONCEabcdef," +
      "s=W22ZaJ0SNY7soEsUEjb6gQ==,i=4096").getBytes(UTF_8)
    intercept[PgError](p0.serverFirst(replayed))
  }

  test("the typestate: an out-of-order step does not EXIST as a method") {
    val e1 = compileErrors(
      "okay.pg.Scram.start(\"u\", \"p\").serverFinal(Array.empty[Byte])")
    assert(e1.contains("serverFinal") && e1.contains("not a member"), e1)
    val e2 = compileErrors(
      "val p: okay.pg.Scram.ClientFinal = null.asInstanceOf[okay.pg.Scram.ClientFinal]\n" +
      "p.serverFirst(Array.empty[Byte])")
    assert(e2.contains("serverFirst") && e2.contains("not a member"), e2)
  }
}
