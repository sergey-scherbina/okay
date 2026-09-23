package okay.x402.evm

import java.math.BigInteger
import okay.*
import okay.chain.Network
import okay.codec.Json.*
import okay.x402.*
import scala.concurrent.Future

/** the payer's payload VERIFIES under the verifier — the round trip is
 * the test — and it declines what it cannot sign honestly */
class TestEvmPayer extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val key = BigInteger("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318", 16)
  private val now = 1_800_000_000L
  private val resource = ResourceInfo("https://api.example.com/report")

  test("a payment signed by EvmPayer verifies, with the reference window and the signer as `from`") {
    val signer = Signer.local(key)
    Async.runAsync(EvmPayer(signer, () => now).pay(price, resource)).map {
      case None => fail("declined")
      case Some(p) =>
        assertEquals(ExactEvm.verify(p, price, now), VerifyResponse(true, None, Some(signer.address)))
        ExactEvm.payload(p.payload) match
          case Right(pl) =>
            assertEquals((pl.authorization.validAfter, pl.authorization.validBefore), (BigInt(now - 600), BigInt(now + 60)))
            assertEquals(pl.authorization.from, signer.address)
            assertEquals(pl.authorization.nonce.length, 32)
          case Left(e) => fail(e)
        // and not after its window
        assertEquals(ExactEvm.verify(p, price, now + 60).invalidReason,
          Some("invalid_exact_evm_payload_authorization_valid_before"))
    }
  }

  test("two payments carry two nonces") {
    val payer = EvmPayer(Signer.local(key), () => now)
    for a <- Async.runAsync(payer.pay(price, resource)); b <- Async.runAsync(payer.pay(price, resource))
    yield assertNotEquals(a.map(p => okay.codec.Json.print(p.payload)), b.map(p => okay.codec.Json.print(p.payload)))
  }

  test("it declines what it cannot sign honestly: another scheme, a non-EVM network, no EIP-712 domain") {
    val payer = EvmPayer(Signer.local(key), () => now)
    val asks = Vector(price.copy(scheme = "upto"), price.copy(network = Network("solana", "mainnet")), price.copy(extra = None))
    Future.traverse(asks)(r => Async.runAsync(payer.pay(r, resource))).map(rs => assertEquals(rs, Vector(None, None, None)))
  }

  test("end to end: a gate verifying locally, a client paying with EvmPayer") {
    val remote = new Facilitator:
      def verify(p: PaymentPayload, r: PaymentRequirements) = pure(VerifyResponse(true, None, None))
      def settle(p: PaymentPayload, r: PaymentRequirements) = pure(SettlementResponse(true, "0xtx", r.network))
      def supported = pure(Vector.empty)
    val clock = () => now
    val gate = Gate(_ => Some(PaymentRequired(resource, Vector(price))), LocalFacilitator(remote, clock = clock))(
      _ => pure(okay.http.Response(200, Nil, okay.http.Http.one("report".getBytes("UTF-8")))))
    val http = new okay.http.Http { def send(r: okay.http.Request) = gate(r) }
    val client = Paying(http, Policy.upTo(BigInt(10000), Set(price.network), Set(price.asset)), EvmPayer(Signer.local(key), clock))
    Async.runAsync(client.send(okay.http.Request.get(resource.url))).map(r => assertEquals(r.status, 200))
  }


  test("docs: the settings file, a durable journal and a payer behind a Signer") {
    val file = java.nio.file.Files.createTempFile("x402", ".json")
    java.nio.file.Files.writeString(file, s"""{ "client": { "maxAmount": "10000", "networks": ["eip155:84532"],
      "assets": ["${price.asset}"], "payTo": ["${price.payTo}"],
      "budget": { "total": "15000", "network": "eip155:84532", "asset": "${price.asset}" } } }"""): Unit
    val path = file.toString
    val store = okay.persist.MemoryStore()
    val signer = Signer.local(key)
    val remote = new Facilitator:
      def verify(p: PaymentPayload, r: PaymentRequirements) = pure(VerifyResponse(true, None, None))
      def settle(p: PaymentPayload, r: PaymentRequirements) = pure(SettlementResponse(true, "0xtx", r.network))
      def supported = pure(Vector.empty)
    val gate = Gate(_ => Some(PaymentRequired(resource, Vector(price))), LocalFacilitator(remote))(
      _ => pure(okay.http.Response(200, Nil, okay.http.Http.one("report".getBytes("UTF-8")))))
    val http = new okay.http.Http { def send(r: okay.http.Request) = gate(r) }
    // ---- snippet: x402-evm-paying
    val conf = X402Conf.load(path).fold(e => sys.error(e), identity)
    val journal = PaymentJournal.on(store.topic("x402-payments", 1, okay.persist.Policy.default))
    val (policy, consent) = X402Conf.client(conf.client.get, journal)
    val client = Paying(http, policy, EvmPayer(signer), consent)   // signer: your KMS-backed Signer
    // ---- snippet ends
    for a <- Async.runAsync(client.send(okay.http.Request.get(resource.url)))
        b <- Async.runAsync(client.send(okay.http.Request.get(resource.url)))
    yield
      assertEquals((a.status, b.status), (200, 402))
      assertEquals(journal.events.count(_.kind == PaymentEvent.Paid), 2)
  }
