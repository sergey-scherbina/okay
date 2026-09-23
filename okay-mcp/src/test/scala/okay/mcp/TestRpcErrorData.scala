package okay.mcp

import okay.codec.Json

/** JSON-RPC 2.0's `error.data` (§5.1): carried both ways, omitted when
 * absent — x402 over MCP puts its PaymentRequired there, under 402 */
class TestRpcErrorData extends munit.FunSuite:

  test("a refusal with data round-trips; without data the member is absent") {
    val data = Json.parse("""{"x402Version":2,"accepts":[]}""")
    val f = Rpc.Failed(Json.JNum(1), 402, "Payment required", Some(data))
    assertEquals(Rpc.decode(Rpc.encode(f)), f)
    val plain = Rpc.Failed(Json.JNum(2), Rpc.MethodNotFound, "nope")
    assert(!Rpc.encode(plain).contains("\"data\""), Rpc.encode(plain))
    assertEquals(Rpc.decode(Rpc.encode(plain)), plain)
  }

  test("the x402 MCP transport's own 402 example decodes with its PaymentRequired intact") {
    val line = """{"jsonrpc":"2.0","id":1,"error":{"code":402,"message":"Payment required","data":{"x402Version":2,"error":"Payment required to access this resource","resource":{"url":"mcp://tool/financial_analysis"},"accepts":[{"scheme":"exact","network":"eip155:84532","amount":"10000","asset":"0x036CbD53842c5426634e7929541eC2318f3dCF7e","payTo":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","maxTimeoutSeconds":60}]}}}"""
    Rpc.decode(line) match
      case Rpc.Failed(Json.JNum(1), 402, "Payment required", Some(Json.JObj(fs))) =>
        assertEquals(fs.map(_._1), Vector("x402Version", "error", "resource", "accepts"))
      case other => fail(s"$other")
  }
