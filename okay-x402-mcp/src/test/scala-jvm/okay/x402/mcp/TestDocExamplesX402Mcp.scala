package okay.x402.mcp

import okay.*
import okay.given
import okay.agent.ToolCall
import okay.chain.Network
import okay.codec.Json.*
import okay.mcp.{Client, Link, Mcp, Rpc, Server, Session}
import okay.x402.*

/** the snippet of docs/modules/okay-x402-mcp.md, VERBATIM between the
 * markers, over an in-memory wire (the facilitator is a stand-in) */
class TestDocExamplesX402Mcp extends munit.FunSuite:

  private val usdcOnBase = "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"
  private val requirements = PaymentRequirements("exact", Network.base, BigInt(10000), usdcOnBase,
    "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", maxTimeoutSeconds = 60)
  private val facilitator = new Facilitator:
    def verify(p: PaymentPayload, r: PaymentRequirements) = pure(VerifyResponse(true, None, Some("0xpayer")))
    def settle(p: PaymentPayload, r: PaymentRequirements) = pure(SettlementResponse(true, "0xtx", r.network, Some("0xpayer")))
    def supported = pure(Vector.empty)
  private val payer = new Payer:
    def pay(r: PaymentRequirements, res: ResourceInfo) = pure(Some(PaymentPayload(r, JObj(Vector("signature" -> JStr("0xsig"))), Some(res))))
  private val serving = Server.Serving(Mcp.Info("reports", "1"),
    Seq(okay.agent.ToolSpec("report", "the report", JObj(Vector.empty))), Map("report" -> (_ => "the report")))

  private val (clientLink, serverLink) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  test("docs: a priced tool and a paying session") {
    Async.spawn(locally {
      // ---- snippet: x402-mcp-server
      // the SERVER: which tools cost what, and a facilitator to verify and settle
      val price = X402Mcp.byTool(name => if name == "report" then Vector(requirements) else Vector.empty)
      Server.run(serverLink, serving, X402Mcp.gate(price, facilitator))
      // ---- snippet ends
    }): Unit
    val session: Session = Client.connect(clientLink, Mcp.Info("agent", "1")).runWith
    // ---- snippet: x402-mcp-client
    // the CLIENT: a Session that pays what the policy allows, keys behind `payer`
    val paying = X402Mcp.Paying(session, Policy.upTo(BigInt(50000), Set(Network.base), Set(usdcOnBase)), payer)
    val report = paying.call(ToolCall("1", "report", Rpc.obj()))
    // "the report" — and requestRpc's answer has result._meta["x402/payment-response"]
    // ---- snippet ends
    assertEquals(report.runWith, "the report")
  }
