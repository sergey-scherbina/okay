package okay.x402

import okay.chain.Network
import okay.codec.Schema
import okay.conf.{Conf, Secret, Secrets}
import okay.http.Http

/**
 * x402's settings as a config file (specs/x402.md stage 4; okay-conf's
 * rules: a case class with a derived `Schema`, secrets as REFERENCES —
 * `env:X402_FACILITATOR_KEY`, `file:/run/secrets/x402` — never values).
 *
 * {{{
 * { "client": { "maxAmount": "10000", "networks": ["eip155:8453"],
 *               "assets": ["0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"],
 *               "payTo": ["0x209693Bc6afc0C5328bA36FaF03C514EF312287C"],
 *               "budget": { "total": "50000", "network": "eip155:8453",
 *                           "asset": "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913",
 *                           "windowSeconds": 86400 } },
 *   "facilitator": { "url": "https://x402.org/facilitator",
 *                    "apiKey": "env:X402_FACILITATOR_KEY" } }
 * }}}
 */
final case class BudgetConf(total: BigInt, network: Network, asset: String,
                            windowSeconds: Option[Long] = None, id: Option[String] = None) derives Schema

/** what a paying client may pay: at most `maxAmount` per payment, on
 * these networks and assets, to these recipients (`payTo`) and for these
 * resources (URL prefixes) when given, within the budget when given */
final case class ClientConf(maxAmount: BigInt, networks: Vector[Network], assets: Vector[String],
                            payTo: Option[Vector[String]] = None,
                            resources: Option[Vector[String]] = None,
                            budget: Option[BudgetConf] = None) derives Schema

/** a remote facilitator; the key, when there is one, is a reference, sent
 * as `apiKeyHeader` (default `Authorization: Bearer <key>`) */
final case class FacilitatorConf(url: String, apiKey: Option[Secret] = None,
                                 apiKeyHeader: Option[String] = None) derives Schema

final case class X402Conf(client: Option[ClientConf] = None,
                          facilitator: Option[FacilitatorConf] = None) derives Schema

object X402Conf:
  def read(json: String): Either[String, X402Conf] = Conf.read[X402Conf](json)
  def load(path: String): Either[String, X402Conf] = Conf.load[X402Conf](path)

  /** the policy and the consent a client config describes; the budget is
   * folded from `journal`, and every decision is written there too */
  def client(c: ClientConf, journal: PaymentJournal,
             clock: () => Long = () => System.currentTimeMillis()): (Policy, Consent) =
    val limit = Policy.upTo(c.maxAmount, c.networks.toSet, c.assets.toSet)
    val policy = c.payTo.fold(limit)(to => limit and Policy.payTo(to.toSet))
    val audited = Consent.audit(journal, clock = clock)
    val scoped = c.resources.fold(audited)(ps => audited and Consent.resources(u => ps.exists(u.startsWith)))
    val consent = c.budget.fold(scoped)(b =>
      scoped and Budget(b.id.getOrElse("budget"), b.total, b.network, b.asset,
        b.windowSeconds.map(_ * 1000), journal, clock))
    (policy, consent)

  /** the facilitator a config describes, its key resolved NOW — a missing
   * key is a refusal naming the reference, not a facilitator that fails
   * on its first payment */
  def facilitator(f: FacilitatorConf, http: Http, secrets: Secrets): Either[String, HttpFacilitator] =
    f.apiKey match
      case None => Right(HttpFacilitator(http, f.url))
      case Some(ref) => secrets.get(ref).map { key =>
        val header = f.apiKeyHeader.fold("authorization" -> s"Bearer $key")(_ -> key)
        HttpFacilitator(http, f.url, () => Seq(header))
      }
