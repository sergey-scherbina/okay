package okay.x402

import okay.*
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Body, Http, Request, Response}

/**
 * Who verifies and settles a payment (x402 §7): usually a FACILITATOR
 * service; verifying `exact` locally needs keccak-256, secp256k1 and
 * EIP-712, which okay-crypto does not have (specs/x402.md, Decisions).
 */
trait Facilitator:
  def verify(payment: PaymentPayload, requirements: PaymentRequirements): VerifyResponse ! Async
  def settle(payment: PaymentPayload, requirements: PaymentRequirements): SettlementResponse ! Async
  def supported: Vector[SupportedKind] ! Async

/** a remote facilitator over okay-http: `POST {base}/verify`, `POST
 * {base}/settle` with `{paymentPayload, paymentRequirements}`, `GET
 * {base}/supported`. A transport failure or an unreadable answer is a
 * refusal with its reason — never a payment taken as good. */
final class HttpFacilitator(http: Http, base: String) extends Facilitator:
  private def post(path: String, p: PaymentPayload, r: PaymentRequirements): Either[String, Json] ! Async =
    val body = Json.print(JObj(Vector("paymentPayload" -> X402.toJson(p), "paymentRequirements" -> X402.toJson(r))))
    http.send(Request.post(s"$base/$path", Body.Text(body), Seq("content-type" -> "application/json")))
      .flatMap(read(path, _))

  private def read(path: String, resp: Response): Either[String, Json] ! Async =
    Http.text(resp).map { text =>
      Json.parse(text) match
        case JErr(m) => Left(s"facilitator $path answered ${resp.status} with no JSON: $m")
        case j => Right(j)
    }

  def verify(p: PaymentPayload, r: PaymentRequirements): VerifyResponse ! Async =
    post("verify", p, r).map(_.flatMap(X402.verification).fold(e => VerifyResponse(false, Some(e)), identity))

  def settle(p: PaymentPayload, r: PaymentRequirements): SettlementResponse ! Async =
    post("settle", p, r).map(_.flatMap(X402.settlement)
      .fold(e => SettlementResponse(false, "", r.network, None, Some(e)), identity))

  def supported: Vector[SupportedKind] ! Async =
    http.send(Request.get(s"$base/supported")).flatMap(read("supported", _)).map {
      case Right(JObj(fs)) => fs.collectFirst { case ("kinds", JArr(ks)) => ks }.getOrElse(Vector.empty).flatMap {
        case JObj(xs) =>
          val f = (n: String) => xs.collectFirst { case (`n`, JStr(s)) => s }
          for s <- f("scheme"); n <- f("network").flatMap(Network.parse(_).toOption) yield SupportedKind(s, n)
        case _ => None
      }
      case _ => Vector.empty
    }

/** payments already settled, so a replayed signature is not charged or
 * served twice; `claim` is atomic (two requests with one payment race) */
trait Settled:
  def claim(key: String): Boolean
  def release(key: String): Unit

object Settled:
  def inMemory(): Settled = new Settled:
    private val keys = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    def claim(key: String): Boolean = keys.add(key)
    def release(key: String): Unit = keys.remove(key): Unit

/**
 * The part of a 402 gate that is not a transport (specs/x402.md stage
 * 3): HTTP carries the payment in a header and MCP in `_meta`, and
 * both then do exactly this — so the replay record and the order
 * (claim BEFORE verify, release on every road that does not settle)
 * are written once, not once per transport.
 */
object Charge:
  /** a payment admitted for `requirements`, claimed under `key` */
  final case class Held(payment: PaymentPayload, requirements: PaymentRequirements, key: String)

  /** match → claim → verify; `Left` is the reason for a 402 */
  def admit(required: PaymentRequired, p: PaymentPayload,
            facilitator: Facilitator, settled: Settled): Either[String, Held] ! Async =
    required.accepts.find(Gate.matches(_, p.accepted)) match
      case None => pure(Left("the payment matches none of the accepted requirements"))
      case Some(reqs) =>
        val key = Json.print(p.payload)
        if !settled.claim(key) then pure(Left("this payment was already used"))
        else facilitator.verify(p, reqs).map { v =>
          if v.isValid then Right(Held(p, reqs, key))
          else { settled.release(key); Left(s"payment invalid: ${v.invalidReason.getOrElse("unspecified")}") }
        }

  /** the resource was not delivered: the payment may be used again */
  def release(h: Held, settled: Settled): Unit = settled.release(h.key)

  /** the resource was delivered: settle, and release on a failure */
  def settle(h: Held, facilitator: Facilitator, settled: Settled): SettlementResponse ! Async =
    facilitator.settle(h.payment, h.requirements).map { s =>
      if !s.success then release(h, settled)
      s
    }

/**
 * The 402 GATE for an okay-http route (specs/x402.md §2). A priced
 * request without `PAYMENT-SIGNATURE` gets `402` and `PAYMENT-REQUIRED`;
 * with one, the payment must match an accepted requirement, must not
 * have been used, and must VERIFY; then the route runs, and only a
 * successful (2xx) answer is SETTLED — the resource is not delivered
 * unpaid, and a payment is not taken for a failure. A settled answer
 * carries `PAYMENT-RESPONSE`.
 */
object Gate:
  private def header(r: Request, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  def paymentRequired(p: PaymentRequired, settlement: Option[SettlementResponse] = None): Response =
    val json = Json.print(X402.toJson(p))
    Response(402,
      Seq("content-type" -> "application/json", X402.Required -> X402.header(X402.toJson(p))) ++
        settlement.map(s => X402.Response -> X402.header(X402.toJson(s))),
      Http.one(json.getBytes("UTF-8")))

  /** the same scheme, network, asset, recipient and amount */
  def matches(accepted: PaymentRequirements, offered: PaymentRequirements): Boolean =
    accepted.scheme == offered.scheme && accepted.network == offered.network &&
      accepted.asset.equalsIgnoreCase(offered.asset) && accepted.payTo.equalsIgnoreCase(offered.payTo) &&
      accepted.amount == offered.amount

  def apply(price: Request => Option[PaymentRequired], facilitator: Facilitator, settled: Settled = Settled.inMemory())
           (route: Request => Response ! Async): Request => Response ! Async = req =>
    price(req) match
      case None => route(req)
      case Some(required) =>
        def refuse(why: String): Response ! Async = pure(paymentRequired(required.copy(error = Some(why))))
        header(req, X402.Signature) match
          case None => pure(paymentRequired(required.copy(error = Some(s"${X402.Signature} header is required"))))
          case Some(h) => X402.unheader(h).flatMap(X402.paymentPayload) match
            case Left(e) => refuse(s"invalid ${X402.Signature}: $e")
            case Right(p) => Charge.admit(required, p, facilitator, settled).flatMap {
              case Left(why) => refuse(why)
              case Right(held) => route(req).flatMap { resp =>
                if !resp.ok then { Charge.release(held, settled); pure(resp) }
                else Charge.settle(held, facilitator, settled).flatMap { s =>
                  if s.success then pure(resp.copy(headers = resp.headers :+ (X402.Response -> X402.header(X402.toJson(s)))))
                  else resp.release.map(_ => paymentRequired(
                    required.copy(error = Some(s"settlement failed: ${s.errorReason.getOrElse("unspecified")}")), Some(s)))
                }
              }
            }

/** which of the accepted requirements a client will pay — never "pay
 * whatever is asked" */
final case class Policy(accept: PaymentRequirements => Boolean)

object Policy:
  /** at most `max` atomic units, on these networks, in these assets */
  def upTo(max: BigInt, networks: Set[Network], assets: Set[String]): Policy =
    Policy(r => r.amount <= max && networks(r.network) && assets.exists(_.equalsIgnoreCase(r.asset)))

/** signs a payment for requirements — keys live behind this, never in
 * okay-x402; `None` declines */
trait Payer:
  def pay(requirements: PaymentRequirements, resource: ResourceInfo): Option[PaymentPayload] ! Async

/**
 * The paying CLIENT: an `Http` that, on a `402` with `PAYMENT-REQUIRED`,
 * picks the first accepted requirement the policy allows, asks the payer
 * to sign, and repeats the request once with `PAYMENT-SIGNATURE`. When
 * the policy allows nothing or the payer declines, the 402 is the answer.
 */
object Paying:
  def apply(http: Http, policy: Policy, payer: Payer): Http = new Http:
    def send(r: Request): Response ! Async =
      http.send(r).flatMap { resp =>
        val required =
          if resp.status != 402 then None
          else resp.header(X402.Required).flatMap(h => X402.unheader(h).flatMap(X402.paymentRequired).toOption)
        required.flatMap(q => q.accepts.find(policy.accept).map(q -> _)) match
          case None => pure(resp)
          case Some((q, choice)) => payer.pay(choice, q.resource).flatMap {
            case None => pure(resp)
            case Some(p) =>
              val signed = r.copy(headers = r.headers.filterNot(_._1.equalsIgnoreCase(X402.Signature)) :+
                (X402.Signature -> X402.header(X402.toJson(p))))
              resp.release.flatMap(_ => http.send(signed))
          }
      }
