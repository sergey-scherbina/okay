package okay.x402.signers

import okay.*
import okay.chain.Network
import okay.codec.Json.*
import okay.conf.{Secret, Secrets}
import okay.x402.*
import okay.x402.evm.{AuthorizationSigner, EvmPayer, ExactEvm}

/**
 * The real providers, `Live`, each skipped without its credentials in the
 * environment. A signature moves no money until a facilitator settles it.
 *  - Circle: CIRCLE_ADDRESS, CIRCLE_WALLET_ID, CIRCLE_API_KEY, CIRCLE_ENTITY_SECRET
 *  - Turnkey: TURNKEY_ADDRESS, TURNKEY_ORGANIZATION_ID, TURNKEY_API_PUBLIC_KEY, TURNKEY_API_PRIVATE_KEY
 *  - Web3Signer: WEB3SIGNER_ADDRESS, WEB3SIGNER_URL
 */
class TestLiveSigners extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val http = okay.http.Transports.http()
  private def env(names: String*): Seq[String] =
    val vs = names.map(sys.env.get)
    assume(vs.forall(_.isDefined), s"not in the environment: ${names.mkString(", ")}")
    vs.flatten
  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private def verifies(s: AuthorizationSigner) =
    val now = java.lang.System.currentTimeMillis() / 1000
    Async.runAsync(EvmPayer(s, () => now).pay(price, ResourceInfo("https://api.example.com/report")))
      .map(p => assertEquals(ExactEvm.verify(p.get, price, now).isValid, true))

  test("Circle") {
    val Seq(address, wallet, _, _) = env("CIRCLE_ADDRESS", "CIRCLE_WALLET_ID", "CIRCLE_API_KEY", "CIRCLE_ENTITY_SECRET")
    verifies(Signers.circle(CircleConf(address, wallet, Secret("env:CIRCLE_API_KEY"), Secret("env:CIRCLE_ENTITY_SECRET")),
      http, Secrets.env).fold(e => fail(e), identity))
  }

  test("Turnkey") {
    val Seq(address, org, pub, _) = env("TURNKEY_ADDRESS", "TURNKEY_ORGANIZATION_ID", "TURNKEY_API_PUBLIC_KEY", "TURNKEY_API_PRIVATE_KEY")
    verifies(Signers.turnkey(TurnkeyConf(address, org, pub, Secret("env:TURNKEY_API_PRIVATE_KEY")), http, Secrets.env)
      .fold(e => fail(e), identity))
  }

  test("Web3Signer") {
    val Seq(address, url) = env("WEB3SIGNER_ADDRESS", "WEB3SIGNER_URL")
    verifies(Signers.web3signer(Web3SignerConf(address, url), http))
  }
