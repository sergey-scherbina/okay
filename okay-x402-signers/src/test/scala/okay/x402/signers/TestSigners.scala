package okay.x402.signers

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyPairGenerator, Signature}
import java.security.KeyFactory
import java.security.spec.{ECGenParameterSpec, ECPrivateKeySpec, ECPublicKeySpec, MGF1ParameterSpec}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{OAEPParameterSpec, PSource}
import okay.*
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Body, Http, Request, Response}
import okay.x402.*
import okay.x402.evm.{AuthorizationSigner, Eip712, Evm, EvmPayer, ExactEvm}
import scala.concurrent.Future

/**
 * Circle, Turnkey and Web3Signer against FAKES that check what each
 * provider must check — Circle: the ciphertext DECRYPTS to the entity
 * secret under the private half, and no two requests share one; Turnkey:
 * the stamp verifies under the registered API key over the body
 * received — and then sign the typed data they were sent, with a local
 * secp256k1 key standing in for the provider's. What `EvmPayer` builds
 * through each must verify under `ExactEvm.verify`.
 */
class TestSigners extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val held = BigInteger("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318", 16)
  private val account = Evm.addressOf(held)
  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val now = 1_800_000_000L

  private def at(j: Json, path: String*): Json = path.foldLeft(j) {
    case (JObj(fs), k) => fs.collectFirst { case (`k`, v) => v }.getOrElse(fail(s"no $k in $j"))
    case (other, k) => fail(s"$k in $other")
  }
  private def s(j: Json, path: String*): String = at(j, path*) match
    case JStr(x) => x
    case other => fail(s"${path.mkString(".")}: $other")
  private def text(r: Request): String = r.body match
    case Body.Text(t) => t
    case other => fail(s"not a text body: $other")
  private def header(r: Request, n: String): String =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }.getOrElse(fail(s"no header $n"))
  private def ok(json: String): Response = Response(200, Nil, Http.one(json.getBytes(UTF_8)))

  /** what the provider's enclave does: sign the typed data it was sent */
  private def signTyped(td: Json, key: BigInteger = held): Array[Byte] =
    val (d, m) = (at(td, "domain"), at(td, "message"))
    val chain = at(d, "chainId") match { case JNum(n) => BigInt(n.toLong); case o => fail(s"chainId $o") }
    Evm.sign(Eip712.digest(Eip712.Domain(s(d, "name"), s(d, "version"), chain, s(d, "verifyingContract")),
      Eip712.Authorization(s(m, "from"), s(m, "to"), BigInt(s(m, "value")), BigInt(s(m, "validAfter")),
        BigInt(s(m, "validBefore")), Evm.unhex(s(m, "nonce")))), key)

  private def pay(signer: AuthorizationSigner): Future[PaymentPayload] =
    Async.runAsync(EvmPayer(signer, () => now).pay(price, ResourceInfo("https://api.example.com/report"))).map(_.get)
  private def verifies(p: PaymentPayload): Unit =
    assertEquals(ExactEvm.verify(p, price, now), VerifyResponse(true, None, Some(account)))

  // ---- Circle

  private val rsa = { val g = KeyPairGenerator.getInstance("RSA"); g.initialize(2048); g.generateKeyPair() }
  private val entitySecret = Array.tabulate[Byte](32)(i => (i * 7 + 1).toByte)

  final class FakeCircle(signWith: BigInteger = held) extends Http:
    @volatile var ciphertexts = Vector.empty[String]
    @volatile var keyFetches = 0
    def send(r: Request): Response ! Async = okay.async {
      assertEquals(header(r, "authorization"), "Bearer TEST_API_KEY:id:secret")
      if r.url.endsWith("/v1/w3s/config/entity/publicKey") then
        keyFetches += 1
        val pem = "-----BEGIN PUBLIC KEY-----\n" + Base64.getMimeEncoder.encodeToString(rsa.getPublic.getEncoded) + "\n-----END PUBLIC KEY-----\n"
        ok(Json.print(JObj(Vector("data" -> JObj(Vector("publicKey" -> JStr(pem)))))))
      else
        assert(r.url.endsWith("/v1/w3s/developer/sign/typedData"), r.url)
        val b = Json.parse(text(r))
        assertEquals(s(b, "walletId"), "wallet-1")
        val ct = s(b, "entitySecretCiphertext")
        assert(!ciphertexts.contains(ct), "a ciphertext was reused")
        ciphertexts :+= ct
        val c = Cipher.getInstance("RSA/ECB/OAEPPadding")
        c.init(Cipher.DECRYPT_MODE, rsa.getPrivate, OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT))
        assertEquals(c.doFinal(Base64.getDecoder.decode(ct)).toSeq, entitySecret.toSeq)
        val sig = signTyped(Json.parse(s(b, "data")), signWith)
        ok(s"""{"data":{"signature":"0x${Evm.hex(sig)}"}}""")
    }

  private def circle(http: Http) = CircleSigner(http, account, "wallet-1", "TEST_API_KEY:id:secret", entitySecret, "https://circle.test")

  test("Circle: a fresh ciphertext per request that decrypts to the entity secret; the payments verify") {
    val fake = FakeCircle()
    val signer = circle(fake)
    for a <- pay(signer); b <- pay(signer)
    yield
      verifies(a); verifies(b)
      assertEquals((fake.ciphertexts.size, fake.keyFetches), (2, 1))
  }

  test("Circle: a signature from another key is refused naming both") {
    pay(circle(FakeCircle(signWith = BigInteger.ONE))).failed.map(e =>
      assert(e.getMessage.contains(Evm.addressOf(BigInteger.ONE)) && e.getMessage.contains(account), e.getMessage))
  }

  // ---- Turnkey

  private val p256 = { val g = KeyPairGenerator.getInstance("EC"); g.initialize(ECGenParameterSpec("secp256r1")); g.generateKeyPair() }
  private val tkPublic =
    val w = KeyFactory.getInstance("EC").getKeySpec(p256.getPublic, classOf[ECPublicKeySpec]).getW
    val x = w.getAffineX.toByteArray.dropWhile(_ == 0)
    (if w.getAffineY.testBit(0) then "03" else "02") + Evm.hex(new Array[Byte](32 - x.length) ++ x)
  private val tkPrivate =
    val d = KeyFactory.getInstance("EC").getKeySpec(p256.getPrivate, classOf[ECPrivateKeySpec]).getS.toByteArray.dropWhile(_ == 0)
    Evm.hex(new Array[Byte](32 - d.length) ++ d)

  final class FakeTurnkey(status: String = "ACTIVITY_STATUS_COMPLETED") extends Http:
    def send(r: Request): Response ! Async = okay.async {
      assert(r.url.endsWith("/public/v1/submit/sign_raw_payload"), r.url)
      val body = text(r)
      val stamp = Json.parse(String(Base64.getUrlDecoder.decode(header(r, "x-stamp")), UTF_8))
      assertEquals((s(stamp, "publicKey"), s(stamp, "scheme")), (tkPublic, "SIGNATURE_SCHEME_TK_API_P256"))
      val v = Signature.getInstance("SHA256withECDSA")
      v.initVerify(p256.getPublic); v.update(body.getBytes(UTF_8))
      assert(v.verify(Evm.unhex(s(stamp, "signature"))), "the stamp does not verify over the body")
      val b = Json.parse(body)
      assertEquals((s(b, "type"), s(b, "organizationId")), ("ACTIVITY_TYPE_SIGN_RAW_PAYLOAD_V2", "org-1"))
      assertEquals((s(b, "parameters", "encoding"), s(b, "parameters", "hashFunction")), ("PAYLOAD_ENCODING_EIP712", "HASH_FUNCTION_NO_OP"))
      val sig = signTyped(Json.parse(s(b, "parameters", "payload")))
      val result = s"""{"r":"${Evm.hex(sig.take(32))}","s":"${Evm.hex(sig.slice(32, 64))}","v":"0${(sig(64) & 0xFF) - 27}"}"""
      ok(s"""{"activity":{"id":"a1","status":"$status","result":{"signRawPayloadResult":$result}}}""")
    }

  private def turnkey(http: Http) = TurnkeySigner(http, account, "org-1", tkPublic, tkPrivate, "https://turnkey.test", () => now * 1000)

  test("Turnkey: the stamp verifies over the body, r/s/v assemble, the payment verifies") {
    pay(turnkey(FakeTurnkey())).map(verifies)
  }

  test("Turnkey: an activity that needs consensus is a refusal naming its status") {
    pay(turnkey(FakeTurnkey("ACTIVITY_STATUS_CONSENSUS_NEEDED"))).failed.map(e =>
      assert(e.getMessage.contains("ACTIVITY_STATUS_CONSENSUS_NEEDED"), e.getMessage))
  }

  // ---- Web3Signer

  final class FakeWeb3Signer(fail: Boolean = false) extends Http:
    def send(r: Request): Response ! Async = okay.async {
      val b = Json.parse(text(r))
      assertEquals(s(b, "method"), "eth_signTypedData")
      val (addr, td) = at(b, "params") match
        case JArr(Vector(JStr(a), t)) => (a, t)
        case other => this.fail(s"params $other")
      assertEquals(addr, account)
      if fail then ok("""{"jsonrpc":"2.0","id":1,"error":{"code":-32000,"message":"Signer not found"}}""")
      else ok(s"""{"jsonrpc":"2.0","id":1,"result":"0x${Evm.hex(signTyped(td))}"}""")
    }
    private def fail(m: String): Nothing = throw AssertionError(m)

  test("Web3Signer: eth_signTypedData with the typed data object; the payment verifies; an error is a refusal") {
    for p <- pay(Web3Signer(FakeWeb3Signer(), account, "http://signer.internal:9000"))
        e <- pay(Web3Signer(FakeWeb3Signer(fail = true), account, "http://signer.internal:9000")).failed
    yield
      verifies(p)
      assert(e.getMessage.contains("Signer not found"), e.getMessage)
  }

  // ---- settings

  test("settings: secrets are references resolved at once; a missing or malformed one names itself") {
    val secrets = okay.conf.Secrets.memory(Map("env:CIRCLE_KEY" -> "TEST_API_KEY:id:secret",
      "env:CIRCLE_ENTITY" -> Evm.hex(entitySecret), "env:TK_KEY" -> tkPrivate))
    val c = CircleConf(account, "wallet-1", okay.conf.Secret("env:CIRCLE_KEY"), okay.conf.Secret("env:CIRCLE_ENTITY"))
    assert(Signers.circle(c, FakeCircle(), secrets).isRight)
    assert(Signers.circle(c.copy(entitySecret = okay.conf.Secret("env:NOPE")), FakeCircle(), secrets).left.exists(_.contains("env:NOPE")))
    val short = okay.conf.Secrets.memory(Map("env:CIRCLE_KEY" -> "k", "env:CIRCLE_ENTITY" -> "abcd"))
    assert(Signers.circle(c, FakeCircle(), short).left.exists(_.contains("32 bytes")))
    val t = TurnkeyConf(account, "org-1", tkPublic, okay.conf.Secret("env:TK_KEY"))
    assert(Signers.turnkey(t, FakeTurnkey(), secrets).isRight)
  }
