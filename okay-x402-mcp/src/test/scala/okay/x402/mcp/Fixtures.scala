package okay.x402.mcp

import okay.*
import okay.agent.{ToolCall, ToolSpec}
import okay.chain.Network
import okay.codec.Json.*
import okay.mcp.{Mcp, Rpc, Server}
import okay.x402.*

/** the transport spec's own price, a facilitator that counts, a payer
 * that signs with a word, and a server with a priced, a free and a
 * failing tool */
object Fixtures:
  val usdc = "0x036CbD53842c5426634e7929541eC2318f3dCF7e"
  val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000), usdc,
    "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  val info = Mcp.Info("paid-tools", "1")

  final class Counting(settleOk: Boolean = true) extends Facilitator:
    @volatile var settles = 0
    def verify(p: PaymentPayload, r: PaymentRequirements): VerifyResponse ! Async =
      pure(p.payload match
        case JObj(fs) if fs.contains("signature" -> JStr("good")) => VerifyResponse(true, None, Some("0xpayer"))
        case _ => VerifyResponse(false, Some("invalid_signature")))
    def settle(p: PaymentPayload, r: PaymentRequirements): SettlementResponse ! Async =
      settles += 1
      pure(if settleOk then SettlementResponse(true, "0xtx", r.network, Some("0xpayer"))
           else SettlementResponse(false, "", r.network, Some("0xpayer"), Some("insufficient_funds")))
    def supported: Vector[SupportedKind] ! Async = pure(Vector(SupportedKind("exact", price.network)))

  final class Signing(signature: String) extends Payer:
    @volatile var calls = 0
    def pay(r: PaymentRequirements, res: ResourceInfo): Option[PaymentPayload] ! Async =
      calls += 1
      pure(Some(PaymentPayload(r, JObj(Vector("signature" -> JStr(signature),
        "authorization" -> JObj(Vector("nonce" -> JStr(s"0x$calls"))))), Some(res))))

  final class Tools:
    @volatile var ran = 0
    val serving = Server.Serving(info,
      Seq(ToolSpec("analysis", "priced", JObj(Vector.empty)), ToolSpec("free", "free", JObj(Vector.empty)),
        ToolSpec("broken", "priced, fails", JObj(Vector.empty))),
      Map("analysis" -> (_ => { ran += 1; "strong fundamentals" }),
        "free" -> (_ => "gratis"),
        "broken" -> (_ => { ran += 1; throw RuntimeException("upstream down") })))

  val prices: Rpc.Request => Option[PaymentRequired] =
    X402Mcp.byTool(n => if n == "free" then Vector.empty else Vector(price))

  def call(id: Int, tool: String, payment: Option[PaymentPayload] = None): Rpc.Request =
    val params = Mcp.callParams(ToolCall(id.toString, tool, JObj(Vector.empty)))
    Rpc.Request(JNum(id), Mcp.ToolsCall,
      payment.fold(params)(p => X402Mcp.withMeta(params, X402Mcp.Payment, X402.toJson(p))))

  def payment(signature: String, nonce: String = "0x1"): PaymentPayload =
    PaymentPayload(price, JObj(Vector("signature" -> JStr(signature),
      "authorization" -> JObj(Vector("nonce" -> JStr(nonce))))), None)

  val hello = Rpc.Request(JNum(0), Mcp.Initialize, Mcp.initializeParams(Mcp.Info("client", "1")))
