package okay.x402.cdp

import okay.*
import okay.chain.Network
import okay.codec.Json.*
import okay.x402.*
import okay.x402.evm.{EvmPayer, ExactEvm}

/**
 * The real Coinbase CDP, `Live`: a Server Wallet signs an x402 payment
 * (Base Sepolia USDC, nothing is broadcast — a signature moves no money
 * until a facilitator settles it) and it verifies. Skipped without
 * CDP_ACCOUNT, CDP_API_KEY_ID, CDP_API_KEY_SECRET, CDP_WALLET_SECRET.
 */
class TestLiveCdp extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  given scala.concurrent.ExecutionContext = munitExecutionContext

  test("a CDP Server Wallet signs an x402 payment that verifies") {
    val env = Seq("CDP_ACCOUNT", "CDP_API_KEY_ID", "CDP_API_KEY_SECRET", "CDP_WALLET_SECRET").map(sys.env.get)
    assume(env.forall(_.isDefined), "CDP credentials not in the environment")
    val conf = CdpConf(env(0).get, env(1).get, okay.conf.Secret("env:CDP_API_KEY_SECRET"), okay.conf.Secret("env:CDP_WALLET_SECRET"))
    val signer = CdpSigner.fromConf(conf, okay.http.Transports.http(), okay.conf.Secrets.env).fold(e => fail(e), identity)
    val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
      "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
      Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
    val now = java.lang.System.currentTimeMillis() / 1000
    Async.runAsync(EvmPayer(signer, () => now).pay(price, ResourceInfo("https://api.example.com/report"))).map { p =>
      assertEquals(ExactEvm.verify(p.get, price, now).isValid, true)
    }
  }
