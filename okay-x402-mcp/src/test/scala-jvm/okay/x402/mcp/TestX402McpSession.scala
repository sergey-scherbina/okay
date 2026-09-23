package okay.x402.mcp

import okay.*
import okay.given
import okay.agent.ToolCall
import okay.codec.Json.*
import okay.mcp.{Client, Link, Mcp, Server, Session}
import okay.x402.*
import Fixtures.*

/** a paying client and a gated server over a real (in-memory) wire:
 * the 402, the payment, the retry and the receipt, end to end */
class TestX402McpSession extends munit.FunSuite:

  private def wire(): (Link, Link) =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    (link(up, down), link(down, up))

  private def connected(t: Tools, f: Facilitator): Session =
    val (client, server) = wire()
    Async.spawn(Server.run(server, t.serving, X402Mcp.gate(prices, f))): Unit
    Client.connect(client, Mcp.Info("payer", "1")).runWith

  private val anyUsdc = Policy.upTo(BigInt(20000), Set(price.network), Set(usdc))
  private val call = ToolCall("1", "analysis", JObj(Vector.empty))

  test("the paying session pays once on the 402, gets the tool's answer and the receipt") {
    val t = Tools(); val f = Counting(); val payer = Signing("good")
    val paying = X402Mcp.Paying(connected(t, f), anyUsdc, payer)
    paying.requestRpc(Mcp.ToolsCall, Mcp.callParams(call)).runWith match
      case Session.Outcome.Answered(result) =>
        assertEquals(Mcp.textOf(result), "strong fundamentals")
        assertEquals(X402Mcp.receipt(result).map(_.transaction), Some("0xtx"))
      case other => fail(s"$other")
    assertEquals((payer.calls, t.ran, f.settles), (1, 1, 1))
  }

  test("call answers the text; a free tool costs nothing and the payer is never asked") {
    val t = Tools(); val payer = Signing("good")
    val paying = X402Mcp.Paying(connected(t, Counting()), anyUsdc, payer)
    assertEquals(paying.call(call).runWith, "strong fundamentals")
    assertEquals(paying.call(ToolCall("2", "free", JObj(Vector.empty))).runWith, "gratis")
    assertEquals(payer.calls, 1)
  }

  test("a price the policy does not allow is not paid: the 402 is the answer") {
    val t = Tools(); val payer = Signing("good")
    val stingy = Policy.upTo(BigInt(1), Set(price.network), Set(usdc))
    val paying = X402Mcp.Paying(connected(t, Counting()), stingy, payer)
    assert(paying.call(call).runWith.startsWith("error: 402"))
    assertEquals((payer.calls, t.ran), (0, 0))
  }

  test("a payment the server refuses is not retried: one signature, one 402") {
    val t = Tools(); val payer = Signing("forged")
    val paying = X402Mcp.Paying(connected(t, Counting()), anyUsdc, payer)
    assertEquals(paying.call(call).runWith, "error: 402 payment invalid: invalid_signature")
    assertEquals((payer.calls, t.ran), (1, 0))
  }
