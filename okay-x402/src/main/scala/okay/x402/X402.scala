package okay.x402

import okay.chain.Network
import okay.codec.{Base64, Json}
import okay.codec.Json.*

/**
 * x402 v2 (specs/x402.md): the protocol's objects, and their wire form.
 *
 * The codec is written over `Json` values rather than derived from a
 * `Schema`, for two reasons found building it: x402 carries OPEN JSON
 * (a scheme's `payload`, `extra`, `extensions`) and `Schema` has no case
 * for an arbitrary JSON value; and x402 OMITS an absent optional field
 * (a settlement that succeeded has no `errorReason`), where a derived
 * encoder writes `null` — which a strict receiver may reject.
 *
 * Amounts are `BigInt` in atomic units and travel as digit strings, as
 * okay's `SBigInt` does; networks are okay-chain's CAIP-2 `Network`.
 */
final case class ResourceInfo(url: String, description: Option[String] = None, mimeType: Option[String] = None)

final case class PaymentRequirements(scheme: String, network: Network, amount: BigInt, asset: String,
                                     payTo: String, maxTimeoutSeconds: Int, extra: Option[Json] = None)

/** the server's 402: what it accepts for which resource */
final case class PaymentRequired(resource: ResourceInfo, accepts: Vector[PaymentRequirements],
                                 error: Option[String] = None, extensions: Option[Json] = None,
                                 x402Version: Int = 2)

/** the client's payment: the requirements it chose and a scheme-specific
 * signed authorization (`exact` on EVM: an EIP-3009 authorization and
 * its EIP-712 signature) */
final case class PaymentPayload(accepted: PaymentRequirements, payload: Json,
                                resource: Option[ResourceInfo] = None, extensions: Option[Json] = None,
                                x402Version: Int = 2)

final case class SettlementResponse(success: Boolean, transaction: String, network: Network,
                                    payer: Option[String] = None, errorReason: Option[String] = None)

final case class VerifyResponse(isValid: Boolean, invalidReason: Option[String] = None, payer: Option[String] = None)

final case class SupportedKind(scheme: String, network: Network, x402Version: Int = 2)

object X402:
  /** the HTTP transport's three headers (x402 transports-v2/http.md) */
  val Required = "PAYMENT-REQUIRED"
  val Signature = "PAYMENT-SIGNATURE"
  val Response = "PAYMENT-RESPONSE"

  // ---- reading ---------------------------------------------------------

  private def field(j: Json, k: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (`k`, v) if v != JNull => v }
    case _ => None
  private def need(j: Json, k: String): Either[String, Json] = field(j, k).toRight(s"missing '$k'")
  private def str(j: Json, k: String): Either[String, String] = need(j, k).flatMap {
    case JStr(s) => Right(s); case other => Left(s"'$k' is not a string: $other") }
  private def optStr(j: Json, k: String): Either[String, Option[String]] = field(j, k) match
    case None => Right(None)
    case Some(JStr(s)) => Right(Some(s))
    case Some(other) => Left(s"'$k' is not a string: $other")
  private def int(j: Json, k: String): Either[String, Int] = need(j, k).flatMap {
    case JNum(n) if n.isWhole => Right(n.toInt); case other => Left(s"'$k' is not an integer: $other") }
  private def bool(j: Json, k: String): Either[String, Boolean] = need(j, k).flatMap {
    case JBool(b) => Right(b); case other => Left(s"'$k' is not a boolean: $other") }
  private def network(j: Json, k: String): Either[String, Network] = str(j, k).flatMap(Network.parse)
  private def amount(j: Json, k: String): Either[String, BigInt] = str(j, k).flatMap(s =>
    if s.nonEmpty && s.forall(_.isDigit) then Right(BigInt(s)) else Left(s"'$k' is not an amount in atomic units: '$s'"))
  private def version(j: Json): Either[String, Int] = int(j, "x402Version").flatMap(v =>
    if v == 2 then Right(v) else Left(s"x402Version $v is not supported (this speaks 2)"))

  def resource(j: Json): Either[String, ResourceInfo] =
    for u <- str(j, "url"); d <- optStr(j, "description"); m <- optStr(j, "mimeType") yield ResourceInfo(u, d, m)

  def requirements(j: Json): Either[String, PaymentRequirements] =
    for
      scheme <- str(j, "scheme"); net <- network(j, "network"); amt <- amount(j, "amount")
      asset <- str(j, "asset"); payTo <- str(j, "payTo"); t <- int(j, "maxTimeoutSeconds")
    yield PaymentRequirements(scheme, net, amt, asset, payTo, t, field(j, "extra"))

  def paymentRequired(j: Json): Either[String, PaymentRequired] =
    for
      v <- version(j); r <- need(j, "resource").flatMap(resource); e <- optStr(j, "error")
      as <- need(j, "accepts").flatMap {
        case JArr(xs) => xs.foldLeft[Either[String, Vector[PaymentRequirements]]](Right(Vector.empty))((acc, x) =>
          acc.flatMap(v => requirements(x).map(v :+ _)))
        case other => Left(s"'accepts' is not an array: $other") }
    yield PaymentRequired(r, as, e, field(j, "extensions"), v)

  def paymentPayload(j: Json): Either[String, PaymentPayload] =
    for
      v <- version(j); a <- need(j, "accepted").flatMap(requirements); p <- need(j, "payload")
      r <- field(j, "resource").fold(Right(None))(resource(_).map(Some(_)))
    yield PaymentPayload(a, p, r, field(j, "extensions"), v)

  def settlement(j: Json): Either[String, SettlementResponse] =
    for
      ok <- bool(j, "success"); tx <- str(j, "transaction"); net <- network(j, "network")
      payer <- optStr(j, "payer"); why <- optStr(j, "errorReason")
    yield SettlementResponse(ok, tx, net, payer, why)

  def verification(j: Json): Either[String, VerifyResponse] =
    for ok <- bool(j, "isValid"); why <- optStr(j, "invalidReason"); payer <- optStr(j, "payer")
    yield VerifyResponse(ok, why, payer)

  // ---- writing: absent optionals are OMITTED --------------------------

  private def obj(fs: (String, Option[Json])*): Json = JObj(fs.collect { case (k, Some(v)) => k -> v }.toVector)
  private def s(x: String): Option[Json] = Some(JStr(x))
  private def os(x: Option[String]): Option[Json] = x.map(JStr(_))

  def toJson(r: ResourceInfo): Json = obj("url" -> s(r.url), "description" -> os(r.description), "mimeType" -> os(r.mimeType))

  def toJson(r: PaymentRequirements): Json = obj(
    "scheme" -> s(r.scheme), "network" -> s(r.network.toString), "amount" -> s(r.amount.toString),
    "asset" -> s(r.asset), "payTo" -> s(r.payTo), "maxTimeoutSeconds" -> Some(JNum(r.maxTimeoutSeconds)),
    "extra" -> r.extra)

  def toJson(p: PaymentRequired): Json = obj(
    "x402Version" -> Some(JNum(p.x402Version)), "error" -> os(p.error), "resource" -> Some(toJson(p.resource)),
    "accepts" -> Some(JArr(p.accepts.map(toJson))), "extensions" -> p.extensions)

  def toJson(p: PaymentPayload): Json = obj(
    "x402Version" -> Some(JNum(p.x402Version)), "resource" -> p.resource.map(toJson),
    "accepted" -> Some(toJson(p.accepted)), "payload" -> Some(p.payload), "extensions" -> p.extensions)

  def toJson(r: SettlementResponse): Json = obj(
    "success" -> Some(JBool(r.success)), "errorReason" -> os(r.errorReason), "payer" -> os(r.payer),
    "transaction" -> s(r.transaction), "network" -> s(r.network.toString))

  def toJson(r: VerifyResponse): Json = obj(
    "isValid" -> Some(JBool(r.isValid)), "invalidReason" -> os(r.invalidReason), "payer" -> os(r.payer))

  // ---- the headers: base64 of the compact JSON -------------------------

  def header(j: Json): String = Base64.encode(Json.print(j).getBytes("UTF-8"))

  def unheader(b64: String): Either[String, Json] =
    Base64.decode(b64.trim).flatMap { bs =>
      Json.parse(String(bs, "UTF-8")) match
        case JErr(m) => Left(s"not JSON: $m")
        case j => Right(j)
    }
