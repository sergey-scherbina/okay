package okay.x402.mcp

import okay.*
import okay.given
import okay.agent.{Tool, ToolCall}
import okay.codec.Json
import okay.codec.Json.*
import okay.mcp.{Mcp, Rpc, Server, Session}
import okay.x402.*

/**
 * x402 over MCP (x402 `specs/transports-v2/mcp.md`; specs/x402.md
 * stage 3). The objects are the HTTP transport's, the places are
 * JSON-RPC's own:
 *
 *  - payment required: a JSON-RPC ERROR, code 402, `PaymentRequired`
 *    in `error.data`;
 *  - the payment: `params._meta["x402/payment"]`, a `PaymentPayload`;
 *  - the receipt: `result._meta["x402/payment-response"]`, a
 *    `SettlementResponse` — or, when settling fails, a 402 whose data
 *    is the `PaymentRequired` again with the failed settlement under
 *    the same key.
 */
object X402Mcp:
  val Payment = "x402/payment"
  val PaymentResponse = "x402/payment-response"
  /** JSON-RPC has no status line, so the transport borrows HTTP's number */
  val PaymentRequiredCode = 402

  private def fields(j: Json): Vector[(String, Json)] = j match
    case JObj(fs) => fs
    case _ => Vector.empty

  /** `j` with `_meta[key] = value`, keeping everything else in both */
  def withMeta(j: Json, key: String, value: Json): Json =
    val fs = fields(j)
    val meta = fs.collectFirst { case ("_meta", m) => fields(m) }.getOrElse(Vector.empty)
      .filterNot(_._1 == key) :+ (key -> value)
    JObj(fs.filterNot(_._1 == "_meta") :+ ("_meta" -> JObj(meta)))

  /** `_meta[key]` of a request's params or an answer's result */
  def meta(j: Json, key: String): Option[Json] =
    Rpc.field(j, "_meta").flatMap(Rpc.field(_, key))

  /** the settlement an answer carries, when it carries one */
  def receipt(result: Json): Option[SettlementResponse] =
    meta(result, PaymentResponse).flatMap(X402.settlement(_).toOption)

  /** the refusal the transport spec prints: code 402, message = the
   * reason, data = the `PaymentRequired` (and a failed settlement) */
  def paymentRequired(id: Json, required: PaymentRequired,
                      settlement: Option[SettlementResponse] = None): Rpc.Failed =
    val data = X402.toJson(required)
    Rpc.Failed(id, PaymentRequiredCode, required.error.getOrElse("Payment required"),
      Some(settlement.fold(data)(s => JObj(fields(data) :+ (PaymentResponse -> X402.toJson(s))))))

  /** a tool answer that says it failed (`isError`) did not deliver the
   * resource, so it is not charged — MCP's form of HTTP's non-2xx */
  private def delivered(result: Json): Boolean =
    !Rpc.field(result, "isError").contains(JBool(true))

  /**
   * Price `tools/call` by tool name: `accepts(name)` empty is a free
   * tool. The resource is `mcp://tool/<name>`, the URL form the
   * transport spec's own examples use.
   */
  def byTool(accepts: String => Vector[PaymentRequirements],
             description: String => Option[String] = _ => None): Rpc.Request => Option[PaymentRequired] = r =>
    if r.method != Mcp.ToolsCall then None
    else Rpc.str(r.params, "name").flatMap { name =>
      val as = accepts(name)
      Option.when(as.nonEmpty)(PaymentRequired(
        ResourceInfo(s"mcp://tool/$name", description(name), Some("application/json")), as,
        Some("Payment required to access this resource")))
    }

  /**
   * The server side: an `Around` for `Server.serveIn`/`Server.run`. A
   * priced request without a payment is refused with 402; a MALFORMED
   * one with -32602 (the transport spec's table: invalid params, not a
   * price); an admitted one goes on, and its reply is SETTLED only when
   * it delivered — an answer without `isError`. The order and the
   * replay record are okay-x402's `Charge`, the HTTP gate's own.
   */
  def gate(price: Rpc.Request => Option[PaymentRequired], facilitator: Facilitator,
           settled: Settled = Settled.inMemory()): Server.Around[Async] = r =>
    def pass(leave: Rpc => Rpc ! Async): Either[Rpc, Server.Around.Pass[Async]] =
      Right(Server.Around.Pass[Async](r, leave))
    price(r) match
      case None => pure(pass(out => pure(out)))
      case Some(required) =>
        def refuse(why: String): Either[Rpc, Server.Around.Pass[Async]] =
          Left(paymentRequired(r.id, required.copy(error = Some(why))))
        meta(r.params, Payment) match
          case None => pure(Left(paymentRequired(r.id, required)))
          case Some(j) => X402.paymentPayload(j) match
            case Left(e) => pure(Left(Rpc.Failed(r.id, Rpc.InvalidParams,
              s"Invalid parameters: malformed payment payload in _meta['$Payment']: $e")))
            case Right(p) => Charge.admit(required, p, facilitator, settled).map {
              case Left(why) => refuse(why)
              case Right(held) => pass {
                case Rpc.Answer(id, result) if delivered(result) =>
                  Charge.settle(held, facilitator, settled).map { s =>
                    if s.success then Rpc.Answer(id, withMeta(result, PaymentResponse, X402.toJson(s)))
                    else paymentRequired(id, required.copy(error = Some(
                      s"Payment settlement failed: ${s.errorReason.getOrElse("unspecified")}")), Some(s))
                  }
                case other =>
                  Charge.release(held, settled)
                  pure(other)
              }
            }

  /**
   * The client side: a `Session` that pays. A request refused with 402
   * and a readable `PaymentRequired` is paid ONCE — the first accepted
   * requirement the policy allows, if the `Consent` approves, signed by
   * the payer — and repeated with the payment in `_meta`. Anything else,
   * including a policy that allows nothing, a consent that refuses or a
   * payer that declines, is the answer as it came. A repeat whose answer
   * carries no successful receipt was not paid, and the consent is told.
   */
  final class Paying(session: Session, policy: Policy, payer: Payer, consent: Consent = Consent.always):
    def requestRpc(method: String, params: Json): Session.Outcome ! Async =
      session.requestRpc(method, params).flatMap {
        case first @ Session.Outcome.Refused(Rpc.Failed(_, PaymentRequiredCode, _, Some(data))) =>
          X402.paymentRequired(data).toOption
            .flatMap(q => q.accepts.find(policy.accept).map(q -> _)) match
            case None => pure(first)
            case Some((q, choice)) => consent.approve(choice, q.resource).flatMap {
              case false => pure(first)
              case true => payer.pay(choice, q.resource).flatMap {
                case None => consent.returned(choice); pure(first)
                case Some(p) =>
                  session.requestRpc(method, withMeta(params, Payment, X402.toJson(p))).map { again =>
                    if !paid(again) then consent.returned(choice)
                    again
                  }
              }
            }
        case other => pure(other)
      }

    private def paid(o: Session.Outcome): Boolean = o match
      case Session.Outcome.Answered(result) => receipt(result).exists(_.success)
      case _ => false

    /** `Session.call`, paying: the tool's text, or `error: <why>` */
    def call(c: ToolCall): String ! Async =
      requestRpc(Mcp.ToolsCall, Mcp.callParams(c)).map {
        case Session.Outcome.Answered(result) => Mcp.textOf(result)
        case Session.Outcome.Refused(f) => s"error: ${f.code} ${f.message}"
        case Session.Outcome.Ended => "error: the MCP link ended"
      }

    /**
     * The paying session AS the agent's `Tool` handler, the two forms
     * `Session` has: an agent program does not change by one character
     * when its tools cost money — the price is met by the policy, the
     * consent and the payer, all of them outside the program, and a
     * refusal reaches the model as the `error: 402 …` answer it can read.
     */
    def interpret: Tool ==> ([X] =>> X ! Async) =
      [X] => (t: Tool[X]) => t match
        // Tool is covariant, so matching `Call` gives String <: X — an
        // upcast, not an assertion
        case Tool.Call(c) => call(c).map(s => (s: X))

    def handler(using CanBlock): Handler[Tool] = new:
      def handle[A](e: Tool[A]): A = e match
        case Tool.Call(c) => call(c).runWith
