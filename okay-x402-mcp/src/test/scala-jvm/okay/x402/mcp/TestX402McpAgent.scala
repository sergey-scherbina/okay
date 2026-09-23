package okay.x402.mcp

import okay.*
import okay.given
import okay.agent.*
import okay.codec.Json.*
import okay.mcp.{Client, Link, Mcp, Server}
import okay.x402.*
import Fixtures.*

/**
 * specs/x402.md §3, for an agent: the SAME agent program against a free
 * tool table and against a PAID MCP server — nothing in the program
 * mentions money. The price is met outside it, by policy, consent and
 * payer; a refusal reaches the model as an answer it can read.
 */
class TestX402McpAgent extends munit.FunSuite:

  private def paidServer(t: Tools, f: Facilitator): okay.mcp.Session =
    val up = Channel[String]()
    val down = Channel[String]()
    def link(out: Channel[String], in: Channel[String]): Link = new Link:
      def send(line: String): Unit ! Async = out.send(line).map(_ => ())
      def lines: Source[String] = Writer.of(in)
    Async.spawn(Server.run(link(down, up), t.serving, X402Mcp.gate(prices, f))): Unit
    Client.connect(link(up, down), Mcp.Info("agent", "1")).runWith

  private val analysis = ToolCall("c1", "analysis", JObj(Vector.empty))

  /** the program: ask twice, answer what the tools said */
  private def program: String ! Agent = Agent.converse("analyse", Nil)

  private def run(tool: Handler[Tool], replies: Reply*): String =
    given Handler[Model] = Handlers.scripted(replies)
    given Handler[Tool] = tool
    given Handler[Context] = Handlers.context(Compact.all)._2
    given rowCA: Handler[Context + Async] = Handler.union[Context, Async]
    given rowTCA: Handler[Tool + (Context + Async)] = Handler.union[Tool, Context + Async]
    given rowAll: Handler[Agent] = Handler.union[Model, Tool + (Context + Async)]
    program.runWith

  private val anyUsdc = Policy.upTo(BigInt(20000), Set(price.network), Set(usdc))

  test("the same agent program, a free tool table and a paid MCP server: same answer, one payment") {
    val t = Tools(); val f = Counting(); val payer = Signing("good")
    val local = run(Handlers.tools(Map("analysis" -> (_ => "strong fundamentals"))),
      Reply("looking", Seq(analysis)), Reply("done", Nil))
    val paid = run(X402Mcp.Paying(paidServer(t, f), anyUsdc, payer).handler,
      Reply("looking", Seq(analysis)), Reply("done", Nil))
    assertEquals(paid, local)
    assertEquals((payer.calls, t.ran, f.settles), (1, 1, 1))
  }

  test("a budget stops the agent paying: the second call's 402 is what the tool answers") {
    val t = Tools(); val f = Counting(); val payer = Signing("good")
    val budget = Consent.budget(BigInt(15000), price.network, usdc)
    val paying = X402Mcp.Paying(paidServer(t, f), anyUsdc, payer, budget)
    assertEquals(paying.call(analysis).runWith, "strong fundamentals")
    val second = paying.call(analysis.copy(id = "c2")).runWith
    assert(second.startsWith("error: 402"), second)
    assertEquals((budget.remaining, payer.calls, t.ran), (BigInt(5000), 1, 1))
  }

  test("a forged payment over MCP gives the budget back") {
    val t = Tools()
    val budget = Consent.budget(BigInt(15000), price.network, usdc)
    val paying = X402Mcp.Paying(paidServer(t, Counting()), anyUsdc, Signing("forged"), budget)
    assert(paying.call(analysis).runWith.startsWith("error: 402"))
    assertEquals((budget.remaining, t.ran), (BigInt(15000), 0))
  }
