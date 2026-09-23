package okay.x402

import okay.*
import okay.chain.Network
import okay.codec.Json.*
import okay.http.{Http, Request, Response}

/** the snippet of docs/modules/okay-x402.md, VERBATIM between the markers,
 * run end to end in memory (the facilitator is a stand-in) */
class TestDocExamplesX402 extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val usdcOnBase = "0x833589fCD6eDb6E08f4c7C32D4f71b54bdA02913"
  private val merchant = "0x209693Bc6afc0C5328bA36FaF03C514EF312287C"
  private val routes: Request => Response ! Async = _ => pure(Response(200, Nil, Http.one("the report".getBytes("UTF-8"))))
  private val facilitator = new Facilitator:
    def verify(p: PaymentPayload, r: PaymentRequirements) = pure(VerifyResponse(true, None, Some("0xpayer")))
    def settle(p: PaymentPayload, r: PaymentRequirements) = pure(SettlementResponse(true, "0xtx", r.network, Some("0xpayer")))
    def supported = pure(Vector.empty)
  private val payer = new Payer:
    def pay(r: PaymentRequirements, res: ResourceInfo) = pure(Some(PaymentPayload(r, JObj(Vector("signature" -> JStr("0xsig"))), Some(res))))

  test("docs: a priced route and a paying client") {
    // ---- snippet: x402
    // the SERVER: what a route costs, verified and settled by a facilitator
    val price = PaymentRequirements("exact", Network.base, BigInt(10000), usdcOnBase, merchant, maxTimeoutSeconds = 60)
    val paid = Gate(
      req => Option.when(req.url.endsWith("/report"))(PaymentRequired(ResourceInfo(req.url), Vector(price))),
      facilitator)(routes)
    // Server.serve(8080)(paid)                    — JVM: the route, now priced

    // the CLIENT: pays what the policy allows, with keys behind `payer`
    val http: Http = new Http { def send(r: Request) = paid(r) }   // in production: a real transport
    val client = Paying(http, Policy.upTo(BigInt(50000), Set(Network.base), Set(usdcOnBase)), payer)
    val report = client.send(Request.get("https://api.example.com/report"))
    // Response(200, ..., PAYMENT-RESPONSE: {"success":true,"transaction":"0xtx",...})
    // ---- snippet ends
    Async.runAsync(report).map { r =>
      assertEquals(r.status, 200)
      assert(r.header(X402.Response).isDefined)
    }
  }
