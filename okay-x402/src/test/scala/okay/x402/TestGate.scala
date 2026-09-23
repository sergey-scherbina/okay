package okay.x402

import okay.*
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.http.{Http, Request, Response}
import scala.concurrent.Future

/** the 402 gate and the paying client, end to end in memory: a client
 * `Http` that calls the gated route directly, a facilitator that counts */
class TestGate extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val usdc = "0x036CbD53842c5426634e7929541eC2318f3dCF7e"
  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000), usdc,
    "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60, Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val resource = ResourceInfo("https://api.example.com/premium-data", Some("premium"), Some("text/plain"))

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

  private def text(s: String, status: Int = 200) = Response(status, Nil, Http.one(s.getBytes("UTF-8")))

  private def server(f: Facilitator, status: Int = 200): Http =
    val gated = Gate(req => Option.when(req.url.endsWith("/premium-data"))(PaymentRequired(resource, Vector(price))), f)(
      req => pure(if req.url.endsWith("/premium-data") then text("secret", status) else text("free")))
    new Http { def send(r: Request): Response ! Async = gated(r) }

  private var nonce = 0
  final class Signing(signature: String) extends Payer:
    @volatile var calls = 0
    def pay(r: PaymentRequirements, res: ResourceInfo): Option[PaymentPayload] ! Async =
      calls += 1; nonce += 1
      pure(Some(PaymentPayload(r, JObj(Vector("signature" -> JStr(signature),
        "authorization" -> JObj(Vector("nonce" -> JStr(s"0x$nonce"))))), Some(res))))

  private val anyUsdc = Policy.upTo(BigInt(20000), Set(price.network), Set(usdc))
  private def run[A](p: A ! Async): Future[A] = Async.runAsync(p)
  private def body(r: Response): Future[String] = run(Http.text(r))

  test("unpaid: 402 with PAYMENT-REQUIRED naming the price; a free route is untouched") {
    val http = server(Counting())
    for
      r <- run(http.send(Request.get("https://api.example.com/premium-data")))
      free <- run(http.send(Request.get("https://api.example.com/free")))
      t <- body(free)
    yield
      assertEquals(r.status, 402)
      val q = r.header(X402.Required).map(X402.unheader(_).flatMap(X402.paymentRequired)).get
      assertEquals(q.map(_.accepts), Right(Vector(price)))
      assertEquals(t, "free")
  }

  test("the paying client pays once, gets the resource and a PAYMENT-RESPONSE; the facilitator settled once") {
    val f = Counting()
    val payer = Signing("good")
    val http = Paying(server(f), anyUsdc, payer)
    for
      r <- run(http.send(Request.get("https://api.example.com/premium-data")))
      t <- body(r)
    yield
      assertEquals((r.status, t), (200, "secret"))
      val s = r.header(X402.Response).map(X402.unheader(_).flatMap(X402.settlement)).get
      assertEquals(s.map(x => (x.success, x.transaction)), Right((true, "0xtx")))
      assertEquals((payer.calls, f.settles), (1, 1))
  }

  test("a replayed payment is refused and not settled again") {
    val f = Counting()
    val http = server(f)
    val p = PaymentPayload(price, JObj(Vector("signature" -> JStr("good"), "authorization" -> JObj(Vector("nonce" -> JStr("0xfixed"))))))
    val signed = Request.get("https://api.example.com/premium-data", Seq(X402.Signature -> X402.header(X402.toJson(p))))
    for
      first <- run(http.send(signed))
      again <- run(http.send(signed))
      why <- body(again)
    yield
      assertEquals((first.status, again.status, f.settles), (200, 402, 1))
      assert(why.contains("already used"), why)
  }

  test("the policy decides: over the limit, the client does not pay and the 402 is the answer") {
    val f = Counting()
    val payer = Signing("good")
    val http = Paying(server(f), Policy.upTo(BigInt(5000), Set(price.network), Set(usdc)), payer)
    run(http.send(Request.get("https://api.example.com/premium-data"))).map { r =>
      assertEquals((r.status, payer.calls, f.settles), (402, 0, 0))
    }
  }

  test("an invalid signature: 402 with the reason, the route never runs, nothing settled") {
    val f = Counting()
    run(Paying(server(f), anyUsdc, Signing("bad")).send(Request.get("https://api.example.com/premium-data")))
      .flatMap(r => body(r).map(t => (r.status, t)))
      .map { (s, t) =>
        assertEquals((s, f.settles), (402, 0))
        assert(t.contains("invalid_signature"), t)
      }
  }

  test("a failing route is not paid for: its 500 comes back and nothing is settled") {
    val f = Counting()
    run(Paying(server(f, status = 500), anyUsdc, Signing("good")).send(Request.get("https://api.example.com/premium-data")))
      .map(r => assertEquals((r.status, f.settles), (500, 0)))
  }

  test("a failed settlement withholds the resource: 402 with the failed PAYMENT-RESPONSE") {
    val f = Counting(settleOk = false)
    run(Paying(server(f), anyUsdc, Signing("good")).send(Request.get("https://api.example.com/premium-data"))).map { r =>
      assertEquals(r.status, 402)
      val s = r.header(X402.Response).map(X402.unheader(_).flatMap(X402.settlement)).get
      assertEquals(s.map(x => (x.success, x.errorReason)), Right((false, Some("insufficient_funds"))))
    }
  }

  test("HttpFacilitator speaks /verify, /settle and /supported, and an unreadable answer is a refusal") {
    val inner = Counting()
    // a facilitator SERVICE, in memory: the protocol's own request and response shapes
    def service(req: Request): Response ! Async =
      if req.url.endsWith("/supported") then
        pure(text("""{"kinds":[{"x402Version":2,"scheme":"exact","network":"eip155:84532"}],"extensions":[]}"""))
      else
        val j = Json.parse(String(req.body.bytes, "UTF-8"))
        def part(k: String) = j match { case JObj(fs) => fs.collectFirst { case (`k`, v) => v }.getOrElse(JNull); case _ => JNull }
        val p = X402.paymentPayload(part("paymentPayload")).fold(e => sys.error(s"payload: $e"), identity)
        val r = X402.requirements(part("paymentRequirements")).fold(e => sys.error(s"requirements: $e"), identity)
        if req.url.endsWith("/verify") then inner.verify(p, r).map(v => text(Json.print(X402.toJson(v))))
        else inner.settle(p, r).map(s => text(Json.print(X402.toJson(s))))
    val remote = HttpFacilitator(new Http { def send(r: Request) = service(r) }, "https://facilitator.example")
    val broken = HttpFacilitator(new Http { def send(r: Request) = pure(text("<html>oops</html>", 502)) }, "https://down.example")
    val p = PaymentPayload(price, JObj(Vector("signature" -> JStr("good"))))
    for
      v <- run(remote.verify(p, price))
      s <- run(remote.settle(p, price))
      k <- run(remote.supported)
      b <- run(broken.verify(p, price))
    yield
      assertEquals(v.isValid, true)
      assertEquals(s.transaction, "0xtx")
      assertEquals(k, Vector(SupportedKind("exact", price.network)))
      assertEquals(b.isValid, false)
      assert(b.invalidReason.exists(_.contains("502")), b.toString)
  }

  // ---- consent: the decision before paying, with the price in hand

  private def premium(http: Http): Future[Int] =
    run(http.send(Request.get("https://api.example.com/premium-data"))).map(_.status)

  test("a consent that refuses is asked with the price, and nothing is signed") {
    val payer = Signing("good")
    @volatile var asked: Option[(BigInt, String)] = None
    val no = Consent.ask((c, r) => { asked = Some((c.amount, r.url)); pure(false) })
    premium(Paying(server(Counting()), anyUsdc, payer, no)).map { status =>
      assertEquals(status, 402)
      assertEquals(asked, Some((BigInt(10000), resource.url)))
      assertEquals(payer.calls, 0)
    }
  }

  test("a budget is spent across payments and refuses the one it cannot cover") {
    val payer = Signing("good")
    val budget = Consent.budget(BigInt(25000), price.network, usdc.toLowerCase)
    val http = Paying(server(Counting()), anyUsdc, payer, budget)
    for a <- premium(http); b <- premium(http); c <- premium(http)
    yield
      assertEquals((a, b, c), (200, 200, 402))
      assertEquals((budget.remaining, payer.calls), (BigInt(5000), 2))
  }

  test("a payment that was not taken gives its reservation back") {
    val budget = Consent.budget(BigInt(25000), price.network, usdc)
    val forged = Paying(server(Counting()), anyUsdc, Signing("forged"), budget)
    val unsettled = Paying(server(Counting(settleOk = false)), anyUsdc, Signing("good"), budget)
    for a <- premium(forged); b <- premium(unsettled)
    yield
      assertEquals((a, b), (402, 402))
      assertEquals(budget.remaining, BigInt(25000))
  }

  test("budget AND a person: the person's no returns the budget's reservation; another asset is not the budget's") {
    val budget = Consent.budget(BigInt(25000), price.network, usdc)
    val payer = Signing("good")
    premium(Paying(server(Counting()), anyUsdc, payer, budget and Consent.ask((_, _) => pure(false)))).flatMap { s =>
      assertEquals((s, budget.remaining, payer.calls), (402, BigInt(25000), 0))
      val other = Consent.budget(BigInt(25000), price.network, "0xdai")
      premium(Paying(server(Counting()), anyUsdc, payer, other)).map(s2 => assertEquals(s2, 402))
    }
  }
