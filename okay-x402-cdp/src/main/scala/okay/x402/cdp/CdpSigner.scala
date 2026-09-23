package okay.x402.cdp

import java.security.PrivateKey
import okay.*
import okay.codec.{Json, Schema}
import okay.codec.Json.*
import okay.conf.{Secret, Secrets}
import okay.http.{Body, Http, Request}
import okay.x402.evm.{AuthorizationSigner, Eip712, Evm}

/** a CDP API key and the Wallet Secret, resolved */
final case class CdpCredentials(apiKeyId: String, apiKey: PrivateKey, walletKey: PrivateKey)

/**
 * The settings, secrets as REFERENCES (okay-conf): `account` is the
 * Server Wallet's EVM address, the three others are what the CDP portal
 * issues.
 */
final case class CdpConf(account: String, apiKeyId: String, apiKeySecret: Secret, walletSecret: Secret,
                         base: Option[String] = None) derives Schema

/**
 * x402's `exact` authorization signed by a Coinbase CDP Server Wallet
 * (specs/x402.md stage 4b): `POST /platform/v2/evm/accounts/{address}/
 * sign/typed-data` with the EIP-712 typed data itself — so CDP sees what
 * it signs, and a wallet policy set in CDP (a spend limit, an allowed
 * token) can refuse it there. The key never leaves CDP's enclave.
 *
 * The answer is not trusted blindly: the signature is RECOVERED against
 * the digest this process computes and must be `address`'s, or the
 * payment fails naming both — a service answering for another key does
 * not get to sign our payments. CDP refusing (a policy, an auth error)
 * fails the payment with its status and body.
 */
final class CdpSigner(http: Http, val address: String, credentials: CdpCredentials,
                      base: String = CdpSigner.Api,
                      clock: () => Long = () => java.lang.System.currentTimeMillis() / 1000) extends AuthorizationSigner:
  private val host = java.net.URI(base).getHost

  def signAuthorization(d: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async =
    val path = s"/platform/v2/evm/accounts/$address/sign/typed-data"
    val body = CdpSigner.typedData(d, a)
    val now = clock()
    val headers = Seq(
      "authorization" -> s"Bearer ${CdpAuth.bearer(credentials.apiKeyId, credentials.apiKey, "POST", host, path, now)}",
      "x-wallet-auth" -> CdpAuth.wallet(credentials.walletKey, "POST", host, path, body, now),
      "content-type" -> "application/json")
    http.send(Request.post(base + path, Body.Text(Json.print(body)), headers)).flatMap { resp =>
      Http.text(resp).map { text =>
        if resp.status / 100 != 2 then throw IllegalStateException(s"CDP refused to sign (${resp.status}): ${text.take(300)}")
        val sig = Json.parse(text) match
          case JObj(fs) => fs.collectFirst { case ("signature", JStr(s)) => Evm.unhex(s) }
          case _ => None
        sig match
          case None => throw IllegalStateException(s"CDP answered no signature: ${text.take(300)}")
          case Some(raw) =>
            Eip712.checked(d, a, raw, address).fold(e => throw IllegalStateException(s"CDP: $e"), identity)
      }
    }

object CdpSigner:
  val Api = "https://api.cdp.coinbase.com"

  /** the typed data of an EIP-3009 transfer, as CDP's endpoint takes it */
  def typedData(d: Eip712.Domain, a: Eip712.Authorization): Json = Eip712.typedData(d, a)

  /** from the settings: every secret resolved NOW, so a missing one fails
   * at startup naming its reference */
  def fromConf(c: CdpConf, http: Http, secrets: Secrets): Either[String, CdpSigner] =
    for
      apiSecret <- secrets.get(c.apiKeySecret)
      walletSecret <- secrets.get(c.walletSecret)
      apiKey <- CdpAuth.apiKey(apiSecret)
      walletKey <- CdpAuth.walletKey(walletSecret)
    yield CdpSigner(http, c.account, CdpCredentials(c.apiKeyId, apiKey, walletKey), c.base.getOrElse(Api))
