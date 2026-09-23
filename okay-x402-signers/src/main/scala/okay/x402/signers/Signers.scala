package okay.x402.signers

import java.nio.charset.StandardCharsets.UTF_8
import java.security.{KeyFactory, PublicKey, Signature}
import java.security.spec.{ECPrivateKeySpec, X509EncodedKeySpec}
import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.{OAEPParameterSpec, PSource}
import java.security.spec.MGF1ParameterSpec
import okay.*
import okay.codec.{Json, Schema}
import okay.codec.Json.*
import okay.conf.{Secret, Secrets}
import okay.http.{Body, Http, Request, Response}
import okay.x402.evm.{AuthorizationSigner, Eip712, Evm}

/** what the three share: a JSON answer read, or the call refused naming
 * the provider, its status and its body */
private object Answer:
  def json(provider: String, resp: Response): Json ! Async = Http.text(resp).map { text =>
    if resp.status / 100 != 2 then throw IllegalStateException(s"$provider refused (${resp.status}): ${text.take(300)}")
    Json.parse(text) match
      case JErr(e) => throw IllegalStateException(s"$provider answered no JSON: $e")
      case j => j
  }
  def at(j: Json, path: String*): Option[Json] = path.foldLeft(Option(j)) {
    case (Some(JObj(fs)), k) => fs.collectFirst { case (`k`, v) => v }
    case _ => None
  }
  def str(j: Json, path: String*): Option[String] = at(j, path*).collect { case JStr(s) => s }
  def checked(provider: String, d: Eip712.Domain, a: Eip712.Authorization, raw: Array[Byte], account: String): Array[Byte] =
    Eip712.checked(d, a, raw, account).fold(e => throw IllegalStateException(s"$provider: $e"), identity)

// ---------------------------------------------------------------- Circle

/**
 * Circle developer-controlled wallets (specs/x402.md stage 4c): `POST
 * /v1/w3s/developer/sign/typedData` with the wallet's id, the typed data
 * as a JSON STRING, and an `entitySecretCiphertext` made FRESH for every
 * request — Circle refuses a reused one. The ciphertext is the 32-byte
 * entity secret under RSA-OAEP (SHA-256, MGF1 SHA-256) with Circle's
 * entity public key, base64 (circlefin/w3s-entity-secret-sample-code).
 * The public key is fetched once, from `GET /v1/w3s/config/entity/publicKey`.
 */
final class CircleSigner(http: Http, val address: String, walletId: String, apiKey: String,
                         entitySecret: Array[Byte], base: String = CircleSigner.Api) extends AuthorizationSigner:
  require(entitySecret.length == 32, s"the entity secret is 32 bytes, this one is ${entitySecret.length}")
  @volatile private var publicKey: Option[PublicKey] = None
  private def auth = Seq("authorization" -> s"Bearer $apiKey")

  private def entityKey: PublicKey ! Async = publicKey match
    case Some(k) => pure(k)
    case None => http.send(Request.get(s"$base/v1/w3s/config/entity/publicKey", auth))
      .flatMap(Answer.json("Circle", _)).map { j =>
        val pem = Answer.str(j, "data", "publicKey").getOrElse(throw IllegalStateException(s"Circle answered no public key: $j"))
        val k = CircleSigner.pem(pem)
        publicKey = Some(k)
        k
      }

  def signAuthorization(d: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async =
    entityKey.flatMap { k =>
      val body = JObj(Vector(
        "walletId" -> JStr(walletId),
        "data" -> JStr(Json.print(Eip712.typedData(d, a))),
        "entitySecretCiphertext" -> JStr(CircleSigner.ciphertext(entitySecret, k))))
      http.send(Request.post(s"$base/v1/w3s/developer/sign/typedData", Body.Text(Json.print(body)),
        auth ++ Seq("content-type" -> "application/json", "x-request-id" -> java.util.UUID.randomUUID().toString)))
    }.flatMap(Answer.json("Circle", _)).map { j =>
      val sig = Answer.str(j, "data", "signature").getOrElse(throw IllegalStateException(s"Circle answered no signature: $j"))
      Answer.checked("Circle", d, a, Evm.unhex(sig), address)
    }

object CircleSigner:
  val Api = "https://api.circle.com"

  /** a PKIX (SubjectPublicKeyInfo) PEM, as Circle serves it */
  def pem(s: String): PublicKey =
    val der = Base64.getMimeDecoder.decode(s.linesIterator.filterNot(_.startsWith("-----")).mkString)
    KeyFactory.getInstance("RSA").generatePublic(X509EncodedKeySpec(der))

  /** RSA-OAEP, SHA-256 for the hash AND for MGF1 — the JDK's
   * "OAEPWithSHA-256AndMGF1Padding" means MGF1 with SHA-1, so the
   * parameters are spelled out */
  def ciphertext(secret: Array[Byte], key: PublicKey): String =
    val c = Cipher.getInstance("RSA/ECB/OAEPPadding")
    c.init(Cipher.ENCRYPT_MODE, key, OAEPParameterSpec("SHA-256", "MGF1", MGF1ParameterSpec.SHA256, PSource.PSpecified.DEFAULT))
    Base64.getEncoder.encodeToString(c.doFinal(secret))

// ---------------------------------------------------------------- Turnkey

/**
 * Turnkey (specs/x402.md stage 4c; tkhq/sdk `api-key-stamper` and
 * `viem`): `POST /public/v1/submit/sign_raw_payload` — activity
 * `ACTIVITY_TYPE_SIGN_RAW_PAYLOAD_V2`, the typed data as the payload,
 * `PAYLOAD_ENCODING_EIP712`, `HASH_FUNCTION_NO_OP` — with the body
 * STAMPED: `X-Stamp` is base64url (no padding) of `{publicKey, scheme,
 * signature}`, the signature ECDSA-P256-SHA256 over the exact body bytes
 * (DER, hex) with the API key. The answer's `{r, s, v}` has v as the
 * y-parity. An activity not COMPLETED — a policy that needs consensus —
 * is a refusal naming its status.
 */
final class TurnkeySigner(http: Http, val address: String, organizationId: String,
                          apiPublicKey: String, apiPrivateKey: String,
                          base: String = TurnkeySigner.Api,
                          clock: () => Long = () => java.lang.System.currentTimeMillis()) extends AuthorizationSigner:
  private val key = TurnkeySigner.privateKey(apiPrivateKey)

  def signAuthorization(d: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async =
    val body = Json.print(JObj(Vector(
      "type" -> JStr("ACTIVITY_TYPE_SIGN_RAW_PAYLOAD_V2"),
      "timestampMs" -> JStr(clock().toString),
      "organizationId" -> JStr(organizationId),
      "parameters" -> JObj(Vector(
        "signWith" -> JStr(address),
        "payload" -> JStr(Json.print(Eip712.typedData(d, a))),
        "encoding" -> JStr("PAYLOAD_ENCODING_EIP712"),
        "hashFunction" -> JStr("HASH_FUNCTION_NO_OP"))))))
    val headers = Seq("content-type" -> "application/json", "x-stamp" -> TurnkeySigner.stamp(body, apiPublicKey, key))
    http.send(Request.post(s"$base/public/v1/submit/sign_raw_payload", Body.Text(body), headers))
      .flatMap(Answer.json("Turnkey", _)).map { j =>
        Answer.str(j, "activity", "status") match
          case Some("ACTIVITY_STATUS_COMPLETED") => ()
          case other => throw IllegalStateException(s"Turnkey did not complete the signature: ${other.getOrElse("no status")}")
        val result = Seq("r", "s", "v").map(k => Answer.str(j, "activity", "result", "signRawPayloadResult", k))
        result match
          case Seq(Some(r), Some(s), Some(v)) =>
            def word(h: String) = { val b = Evm.unhex(h); new Array[Byte](32 - b.length) ++ b }
            val parity = BigInt(v, 16).toInt
            Answer.checked("Turnkey", d, a, word(r) ++ word(s) :+ (27 + parity).toByte, address)
          case _ => throw IllegalStateException(s"Turnkey answered no signature: $j")
      }

object TurnkeySigner:
  val Api = "https://api.turnkey.com"

  /** a Turnkey API private key: the P-256 scalar as hex */
  def privateKey(hex: String): java.security.PrivateKey =
    val params = java.security.AlgorithmParameters.getInstance("EC")
    params.init(java.security.spec.ECGenParameterSpec("secp256r1"))
    val spec = params.getParameterSpec(classOf[java.security.spec.ECParameterSpec])
    KeyFactory.getInstance("EC").generatePrivate(ECPrivateKeySpec(java.math.BigInteger(1, Evm.unhex(hex)), spec))

  def stamp(body: String, publicKeyHex: String, key: java.security.PrivateKey): String =
    val s = Signature.getInstance("SHA256withECDSA")
    s.initSign(key); s.update(body.getBytes(UTF_8))
    val json = Json.print(JObj(Vector("publicKey" -> JStr(publicKeyHex),
      "scheme" -> JStr("SIGNATURE_SCHEME_TK_API_P256"), "signature" -> JStr(Evm.hex(s.sign())))))
    Base64.getUrlEncoder.withoutPadding.encodeToString(json.getBytes(UTF_8))

// ---------------------------------------------------------------- Web3Signer

/**
 * Web3Signer (Consensys), or anything else speaking `eth_signTypedData`
 * over JSON-RPC — geth's Clef among them (specs/x402.md stage 4c). The
 * keys live in the signer (a keystore, Vault, a cloud KMS behind it). It
 * has no authentication of its own: it belongs on a private network or
 * behind mTLS, which is the `Http` handed in here.
 */
final class Web3Signer(http: Http, val address: String, url: String) extends AuthorizationSigner:
  private val ids = java.util.concurrent.atomic.AtomicLong()
  def signAuthorization(d: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async =
    val body = Json.print(JObj(Vector("jsonrpc" -> JStr("2.0"), "id" -> JNum(ids.incrementAndGet().toDouble),
      "method" -> JStr("eth_signTypedData"), "params" -> JArr(Vector(JStr(address), Eip712.typedData(d, a))))))
    http.send(Request.post(url, Body.Text(body), Seq("content-type" -> "application/json")))
      .flatMap(Answer.json("Web3Signer", _)).map { j =>
        Answer.at(j, "error").foreach(e => throw IllegalStateException(s"Web3Signer refused: ${Json.print(e).take(300)}"))
        val sig = Answer.str(j, "result").getOrElse(throw IllegalStateException(s"Web3Signer answered no result: $j"))
        Answer.checked("Web3Signer", d, a, Evm.unhex(sig), address)
      }

// ---------------------------------------------------------------- settings

/** the settings of each, secrets as REFERENCES (okay-conf) */
final case class CircleConf(address: String, walletId: String, apiKey: Secret, entitySecret: Secret,
                            base: Option[String] = None) derives Schema
final case class TurnkeyConf(address: String, organizationId: String, apiPublicKey: String, apiPrivateKey: Secret,
                             base: Option[String] = None) derives Schema
final case class Web3SignerConf(address: String, url: String) derives Schema

object Signers:
  /** every secret resolved NOW: a missing one fails at startup, naming itself */
  def circle(c: CircleConf, http: Http, secrets: Secrets): Either[String, CircleSigner] =
    for
      key <- secrets.get(c.apiKey)
      secret <- secrets.get(c.entitySecret)
      bytes <- scala.util.Try(Evm.unhex(secret.trim)).toEither.left.map(_ => "the entity secret is not hex")
      _ <- Either.cond(bytes.length == 32, (), s"the entity secret is 32 bytes (64 hex), this one is ${bytes.length}")
    yield CircleSigner(http, c.address, c.walletId, key, bytes, c.base.getOrElse(CircleSigner.Api))

  def turnkey(c: TurnkeyConf, http: Http, secrets: Secrets): Either[String, TurnkeySigner] =
    secrets.get(c.apiPrivateKey).flatMap(k =>
      scala.util.Try(TurnkeySigner(http, c.address, c.organizationId, c.apiPublicKey, k.trim, c.base.getOrElse(TurnkeySigner.Api)))
        .toEither.left.map(e => s"the Turnkey API private key is not a P-256 scalar in hex: ${e.getMessage}"))

  def web3signer(c: Web3SignerConf, http: Http): Web3Signer = Web3Signer(http, c.address, c.url)
