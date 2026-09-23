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

  // ---- stage 4: recipients, memory, audit, settings

  private val merchant = price.payTo

  test("Policy.payTo: a recipient not on the list is not paid, whatever the amount") {
    val payer = Signing("good")
    val onlyOthers = anyUsdc and Policy.payTo(Set("0x0000000000000000000000000000000000000001"))
    for s <- premium(Paying(server(Counting()), onlyOthers, payer))
        t <- premium(Paying(server(Counting()), anyUsdc and Policy.payTo(Set(merchant.toLowerCase)), payer))
    yield assertEquals((s, t, payer.calls), (402, 200, 1))
  }

  test("a budget over a durable journal is not refilled by a restart") {
    val topic = okay.persist.MemoryStore().topic("x402", 1, okay.persist.Policy.default)
    val first = Budget("agent", BigInt(25000), price.network, usdc, journal = PaymentJournal.on(topic))
    premium(Paying(server(Counting()), anyUsdc, Signing("good"), first)).map { s =>
      assertEquals((s, first.remaining), (200, BigInt(15000)))
      // the process restarts: a new journal over the same topic, a new budget
      val again = Budget("agent", BigInt(25000), price.network, usdc, journal = PaymentJournal.on(topic))
      assertEquals(again.remaining, BigInt(15000))
      val other = Budget("someone-else", BigInt(25000), price.network, usdc, journal = PaymentJournal.on(topic))
      assertEquals(other.remaining, BigInt(25000))
    }
  }

  test("a windowed budget counts only what was reserved inside the window; a return names its reservation") {
    var now = 1_000_000L
    val journal = PaymentJournal.inMemory()
    val daily = Budget("daily", BigInt(15000), price.network, usdc, Some(86_400_000L), journal, () => now)
    for a <- premium(Paying(server(Counting()), anyUsdc, Signing("good"), daily))
        b <- premium(Paying(server(Counting()), anyUsdc, Signing("good"), daily))
        d <- { now += 86_400_001L; premium(Paying(server(Counting()), anyUsdc, Signing("forged"), daily)) }
        c <- premium(Paying(server(Counting()), anyUsdc, Signing("good"), daily))
    yield
      // b: the window still holds a's 10000; d: a new window, reserved and returned; c: paid
      assertEquals((a, b, d, c), (200, 402, 402, 200))
      assertEquals(daily.remaining, BigInt(5000))
      val reserved = journal.events.filter(_.kind == PaymentEvent.Reserved).flatMap(_.ref)
      val returned = journal.events.filter(_.kind == PaymentEvent.Returned).flatMap(_.ref)
      assertEquals(reserved.size, 3)
      assertEquals(returned, Vector(reserved(1)))
  }

  test("the server's replay record over a journal survives a restart") {
    val journal = PaymentJournal.inMemory()
    val before = Settled.journaled(journal)
    assert(before.claim("k1"))
    val after = Settled.journaled(journal)
    assert(!after.claim("k1"))
    after.release("k1")
    assert(Settled.journaled(journal).claim("k1"))
  }

  test("audit: asked, then paid with the transaction; asked, then returned for a refused payment") {
    val journal = PaymentJournal.inMemory()
    for _ <- premium(Paying(server(Counting()), anyUsdc, Signing("good"), Consent.audit(journal)))
        _ <- premium(Paying(server(Counting()), anyUsdc, Signing("forged"), Consent.audit(journal)))
    yield
      assertEquals(journal.events.map(e => (e.kind, e.transaction)), Vector(
        (PaymentEvent.Asked, None), (PaymentEvent.Paid, Some("0xtx")),
        (PaymentEvent.Asked, None), (PaymentEvent.Returned, None)))
      assertEquals(journal.events.head.resource, Some(resource.url))
  }

  test("Consent.resources: only resources the predicate accepts") {
    val payer = Signing("good")
    for s <- premium(Paying(server(Counting()), anyUsdc, payer, Consent.resources(_.startsWith("https://other.example/"))))
    yield assertEquals((s, payer.calls), (402, 0))
  }

  test("HttpFacilitator asks for its headers on every request") {
    var sent = Vector.empty[Seq[(String, String)]]
    var token = 0
    val service = new Http { def send(r: Request) = { sent :+= r.headers; pure(text("""{"isValid":true}""")) } }
    val f = HttpFacilitator(service, "https://f.example", () => { token += 1; Seq("authorization" -> s"Bearer t$token") })
    val p = PaymentPayload(price, JObj(Vector.empty))
    for _ <- run(f.verify(p, price)); _ <- run(f.verify(p, price))
    yield assertEquals(sent.map(_.collect { case ("authorization", v) => v }), Vector(Seq("Bearer t1"), Seq("Bearer t2")))
  }

  private val confJson = s"""{ "client": { "maxAmount": "20000", "networks": ["eip155:84532"], "assets": ["$usdc"],
    "payTo": ["$merchant"], "budget": { "total": "15000", "network": "eip155:84532", "asset": "$usdc", "windowSeconds": 86400 } },
    "facilitator": { "url": "https://f.example", "apiKey": "env:X402_TEST_KEY" } }"""

  test("X402Conf: the file builds the policy, the budget and the audit; the facilitator's key is a reference") {
    val conf = X402Conf.read(confJson).fold(e => fail(e), identity)
    val journal = PaymentJournal.inMemory()
    val (policy, consent) = X402Conf.client(conf.client.get, journal)
    val payer = Signing("good")
    val secrets = okay.conf.Secrets.memory(Map("env:X402_TEST_KEY" -> "k-123"))
    assert(X402Conf.facilitator(conf.facilitator.get, server(Counting()), secrets).isRight)
    assertEquals(X402Conf.facilitator(conf.facilitator.get, server(Counting()), okay.conf.Secrets.memory(Map.empty))
      .left.map(_.contains("env:X402_TEST_KEY")), Left(true))
    for a <- premium(Paying(server(Counting()), policy, payer, consent))
        b <- premium(Paying(server(Counting()), policy, payer, consent))
    yield
      assertEquals((a, b, payer.calls), (200, 402, 1))
      assertEquals(journal.events.map(_.kind).count(_ == PaymentEvent.Paid), 2) // the audit's and the budget's
  }
