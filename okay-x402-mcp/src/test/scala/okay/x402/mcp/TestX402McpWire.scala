package okay.x402.mcp

import okay.codec.Json
import okay.mcp.{Mcp, Rpc}
import okay.x402.*

/** golden: the transport spec's own examples (x402
 * `specs/transports-v2/mcp.md`), read by our codec and written back */
class TestX402McpWire extends munit.FunSuite:

  private val paidCall = """{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"financial_analysis","arguments":{"ticker":"AAPL","analysis_type":"deep"},"_meta":{"x402/payment":{"x402Version":2,"resource":{"url":"mcp://tool/financial_analysis","description":"Advanced financial analysis tool","mimeType":"application/json"},"accepted":{"scheme":"exact","network":"eip155:84532","amount":"10000","asset":"0x036CbD53842c5426634e7929541eC2318f3dCF7e","payTo":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","maxTimeoutSeconds":60,"extra":{"name":"USDC","version":"2"}},"payload":{"signature":"0x2d6a7588d6acca505cbf0d9a4a227e0c52c6c34008c8e8986a1283259764173608a2ce6496642e377d6da8dbbf5836e9bd15092f9ecab05ded3d6293af148b571c","authorization":{"from":"0x857b06519E91e3A54538791bDbb0E22373e36b66","to":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","value":"10000","validAfter":"1740672089","validBefore":"1740672154","nonce":"0xf3746613c2d920b5fdabc0856f2aeb2d4f88ee6037b8cc5d04a71a4462f13480"}}}}}}"""

  private val paidAnswer = """{"jsonrpc":"2.0","id":1,"result":{"content":[{"type":"text","text":"Financial analysis for AAPL: Strong fundamentals with positive outlook..."}],"_meta":{"x402/payment-response":{"success":true,"transaction":"0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef","network":"eip155:84532","payer":"0x857b06519E91e3A54538791bDbb0E22373e36b66"}}}}"""

  private val required = """{"jsonrpc":"2.0","id":1,"error":{"code":402,"message":"Payment required to access this resource","data":{"x402Version":2,"error":"Payment required to access this resource","resource":{"url":"mcp://tool/financial_analysis","description":"Advanced financial analysis tool","mimeType":"application/json"},"accepts":[{"scheme":"exact","network":"eip155:84532","amount":"10000","asset":"0x036CbD53842c5426634e7929541eC2318f3dCF7e","payTo":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","maxTimeoutSeconds":60,"extra":{"name":"USDC","version":"2"}}]}}}"""

  test("the spec's paid tools/call: the payment is read out of params._meta, and the call is still a call") {
    Rpc.decode(paidCall) match
      case Rpc.Request(_, Mcp.ToolsCall, params) =>
        val p = X402Mcp.meta(params, X402Mcp.Payment).map(X402.paymentPayload)
        assertEquals(p.map(_.map(x => (x.accepted.amount, x.accepted.payTo))),
          Some(Right((BigInt(10000), "0x209693Bc6afc0C5328bA36FaF03C514EF312287C"))))
        assertEquals(Mcp.callOf(params, "1").map(_.name), Some("financial_analysis"))
      case other => fail(s"$other")
  }

  test("the spec's settled answer: the receipt is read out of result._meta") {
    Rpc.decode(paidAnswer) match
      case Rpc.Answer(_, result) =>
        assertEquals(X402Mcp.receipt(result).map(s => (s.success, s.payer)),
          Some((true, Some("0x857b06519E91e3A54538791bDbb0E22373e36b66"))))
      case other => fail(s"$other")
  }

  test("the spec's 402 is what paymentRequired writes, byte for byte") {
    Rpc.decode(required) match
      case Rpc.Failed(id, 402, _, Some(data)) =>
        val q = X402.paymentRequired(data).toOption.get
        assertEquals(Rpc.encode(X402Mcp.paymentRequired(id, q)), required)
      case other => fail(s"$other")
  }

  test("withMeta keeps what is there and replaces only its own key") {
    val j = Json.parse("""{"name":"t","_meta":{"progressToken":7,"x402/payment":1}}""")
    assertEquals(Json.print(X402Mcp.withMeta(j, X402Mcp.Payment, Json.JNum(2))),
      """{"name":"t","_meta":{"progressToken":7,"x402/payment":2}}""")
  }
