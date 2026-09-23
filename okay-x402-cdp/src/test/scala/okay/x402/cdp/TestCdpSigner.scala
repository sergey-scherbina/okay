package okay.x402.cdp

import java.math.BigInteger
import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyPair, KeyPairGenerator, PublicKey, Signature}
import java.security.spec.ECGenParameterSpec
import java.util.Base64
import okay.*
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Http, Request, Response}
import okay.x402.*
import okay.x402.evm.{Eip712, Evm, EvmPayer, ExactEvm}
import org.bouncycastle.asn1.{ASN1Integer, DERSequence}
import org.bouncycastle.asn1.pkcs.PrivateKeyInfo
import scala.concurrent.Future

/**
 * `CdpSigner` against a FAKE CDP: a service that checks both tokens the
 * way CDP must — signature under the registered public key, algorithm,
 * claims, `uris`, and `reqHash` against the body it RECEIVED — and then
 * signs the typed data it was sent, with a local secp256k1 key standing
 * in for the enclave's. What `EvmPayer` builds through it must verify.
 */
class TestCdpSigner extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private def pair(alg: String, spec: Option[String] = None): KeyPair =
    val g = KeyPairGenerator.getInstance(alg)
    spec.foreach(s => g.initialize(ECGenParameterSpec(s)))
    g.generateKeyPair()
  private val b64 = Base64.getEncoder
  private val ed = pair("Ed25519")
  private val ec = pair("EC", Some("secp256r1"))
  private val wal = pair("EC", Some("secp256r1"))
  /** the forms CDP issues: Ed25519 as 64 bytes (seed ‖ public), EC as a
   * SEC1 PEM, the Wallet Secret as base64 PKCS#8 */
  private val edSecret = b64.encodeToString(ed.getPrivate.getEncoded.takeRight(32) ++ ed.getPublic.getEncoded.takeRight(32))
  private val ecPem = "-----BEGIN EC PRIVATE KEY-----\n" +
    b64.encodeToString(PrivateKeyInfo.getInstance(ec.getPrivate.getEncoded).parsePrivateKey().toASN1Primitive.getEncoded) +
    "\n-----END EC PRIVATE KEY-----\n"
  private val walletSecret = b64.encodeToString(wal.getPrivate.getEncoded)

  private val enclave = BigInteger("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318", 16)
  private val account = Evm.addressOf(enclave)
  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val now = 1_800_000_000L

  private def part(t: String, i: Int): Json = Json.parse(String(Base64.getUrlDecoder.decode(t.split('.')(i)), UTF_8))
  private def field(j: Json, k: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, v) => v }
    case _ => None
  private def str(j: Json, k: String): String = field(j, k) match
    case Some(JStr(s)) => s
    case other => fail(s"$k: $other")
  private def verified(t: String, key: PublicKey): Boolean =
    val Array(h, c, s) = t.split('.')
    val sig = Base64.getUrlDecoder.decode(s)
    val (alg, bytes) =
      if key.getAlgorithm == "EC" then
        ("SHA256withECDSA", DERSequence(Array[org.bouncycastle.asn1.ASN1Encodable](ASN1Integer(BigInteger(1, sig.take(32))), ASN1Integer(BigInteger(1, sig.drop(32))))).getEncoded)
      else ("Ed25519", sig)
    val v = Signature.getInstance(alg); v.initVerify(key); v.update(s"$h.$c".getBytes(UTF_8)); v.verify(bytes)

  /** the fake CDP: checks what CDP checks, signs what it is sent */
  final class FakeCdp(apiKey: PublicKey, signWith: BigInteger = enclave) extends Http:
    @volatile var seen: Vector[String] = Vector.empty
    def send(r: Request): Response ! Async = okay.async {
      val body = r.body match
        case okay.http.Body.Text(t) => t
        case other => fail(s"not a text body: $other")
      val h = (n: String) => r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }.getOrElse(fail(s"no $n"))
      val bearer = h("authorization").stripPrefix("Bearer ")
      val wallet = h("x-wallet-auth")
      val path = r.url.stripPrefix("https://cdp.test")
      val uri = JArr(Vector(JStr(s"POST cdp.test$path")))
      assert(verified(bearer, apiKey), "bearer signature")
      assert(verified(wallet, wal.getPublic), "wallet signature")
      assertEquals(str(part(bearer, 0), "kid"), "organizations/o/apiKeys/k")
      assertEquals(str(part(bearer, 0), "nonce").length, 32)
      assertEquals((str(part(bearer, 1), "sub"), str(part(bearer, 1), "iss")), ("organizations/o/apiKeys/k", "cdp"))
      assertEquals(field(part(bearer, 1), "exp"), Some(JNum((now + 120).toDouble)))
      assertEquals(field(part(bearer, 1), "uris"), Some(uri))
      assertEquals(field(part(wallet, 1), "uris"), Some(uri))
      assertEquals(str(part(wallet, 1), "reqHash"), CdpAuth.sha256Hex(Json.print(CdpAuth.sorted(Json.parse(body)))))
      seen :+= path
      // sign the typed data received, as the enclave would
      val j = Json.parse(body)
      val (d, m) = (field(j, "domain").get, field(j, "message").get)
      val a = Eip712.Authorization(str(m, "from"), str(m, "to"), BigInt(str(m, "value")), BigInt(str(m, "validAfter")),
        BigInt(str(m, "validBefore")), Evm.unhex(str(m, "nonce")))
      val chain = field(d, "chainId") match { case Some(JNum(n)) => BigInt(n.toLong); case o => fail(s"chainId $o") }
      val digest = Eip712.digest(Eip712.domainSeparator(str(d, "name"), str(d, "version"), chain, str(d, "verifyingContract")), a)
      Response(200, Nil, Http.one(s"""{"signature":"0x${Evm.hex(Evm.sign(digest, signWith))}"}""".getBytes(UTF_8)))
    }

  private def signer(apiSecret: String, http: Http): CdpSigner =
    val creds = (for k <- CdpAuth.apiKey(apiSecret); w <- CdpAuth.walletKey(walletSecret)
      yield CdpCredentials("organizations/o/apiKeys/k", k, w)).fold(e => fail(e), identity)
    CdpSigner(http, account, creds, "https://cdp.test", () => now)

  private def pay(s: CdpSigner): Future[Option[PaymentPayload]] =
    Async.runAsync(EvmPayer(s, () => now).pay(price, ResourceInfo("https://api.example.com/report")))

  test("an Ed25519 API key: both tokens verify at the fake CDP, and the payment verifies under ExactEvm") {
    val cdp = FakeCdp(ed.getPublic)
    pay(signer(edSecret, cdp)).map { p =>
      assertEquals(ExactEvm.verify(p.get, price, now), VerifyResponse(true, None, Some(account)))
      assertEquals(cdp.seen, Vector(s"/platform/v2/evm/accounts/$account/sign/typed-data"))
    }
  }

  test("an EC (SEC1 PEM) API key: ES256, the same payment") {
    pay(signer(ecPem, FakeCdp(ec.getPublic))).map(p =>
      assertEquals(ExactEvm.verify(p.get, price, now).isValid, true))
  }

  test("a signature from another key is refused, naming both addresses") {
    val other = BigInteger("1")
    pay(signer(edSecret, FakeCdp(ed.getPublic, signWith = other))).failed.map { e =>
      assert(e.getMessage.contains(Evm.addressOf(other)) && e.getMessage.contains(account), e.getMessage)
    }
  }

  test("CDP refusing (a policy, an auth error) fails the payment with its status and body") {
    val refusing = new Http:
      def send(r: Request) = pure(Response(403, Nil, Http.one("""{"errorType":"policy_violation"}""".getBytes(UTF_8))))
    pay(signer(edSecret, refusing)).failed.map(e =>
      assert(e.getMessage.contains("403") && e.getMessage.contains("policy_violation"), e.getMessage))
  }

  test("CdpConf: secrets are references, resolved at once; a missing one names itself") {
    val conf = CdpConf(account, "organizations/o/apiKeys/k", okay.conf.Secret("env:CDP_KEY"), okay.conf.Secret("env:CDP_WALLET"), Some("https://cdp.test"))
    val all = okay.conf.Secrets.memory(Map("env:CDP_KEY" -> edSecret, "env:CDP_WALLET" -> walletSecret))
    assert(CdpSigner.fromConf(conf, FakeCdp(ed.getPublic), all).isRight)
    val missing = CdpSigner.fromConf(conf, FakeCdp(ed.getPublic), okay.conf.Secrets.memory(Map("env:CDP_KEY" -> edSecret)))
    assert(missing.left.exists(_.contains("env:CDP_WALLET")), missing.toString)
  }
