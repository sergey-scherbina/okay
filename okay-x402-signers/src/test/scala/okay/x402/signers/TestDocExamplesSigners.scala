package okay.x402.signers

import okay.*
import okay.chain.Network
import okay.conf.Secret
import okay.http.{Http, Request, Response}
import okay.x402.*
import okay.x402.evm.EvmPayer

/** the snippet of docs/modules/okay-x402-signers.md, VERBATIM between the
 * markers; nothing is called — the signers are only built */
class TestDocExamplesSigners extends munit.FunSuite:
  test("docs: the three signers from their settings, and a paying client") {
    val address = "0x2c7536E3605D9C16a7a3D7b1898e529396a65c23"
    val (walletId, organizationId) = ("wallet-1", "org-1")
    val apiPublicKey = "02" + "11" * 32
    val http: Http = new Http { def send(r: Request): Response ! Async = pure(Response(200, Nil, Http.one(Array.emptyByteArray))) }
    val policy = Policy.upTo(BigInt(10000), Set(Network("eip155", "84532")), Set("0x036CbD53842c5426634e7929541eC2318f3dCF7e"))
    val consent = Consent.always
    object Secrets { val env: okay.conf.Secrets = okay.conf.Secrets.memory(Map(
      "env:CIRCLE_API_KEY" -> "TEST_API_KEY:id:secret", "env:CIRCLE_ENTITY_SECRET" -> "ab" * 32,
      "env:TURNKEY_API_PRIVATE_KEY" -> ("0" * 63 + "1"))) }
    // ---- snippet: signers
    val circle = Signers.circle(CircleConf(address, walletId, Secret("env:CIRCLE_API_KEY"), Secret("env:CIRCLE_ENTITY_SECRET")), http, Secrets.env)
    val turnkey = Signers.turnkey(TurnkeyConf(address, organizationId, apiPublicKey, Secret("env:TURNKEY_API_PRIVATE_KEY")), http, Secrets.env)
    val local = Signers.web3signer(Web3SignerConf(address, "http://web3signer.internal:9000"), http)
    val client = circle.map(signer => Paying(http, policy, EvmPayer(signer), consent))
    // ---- snippet ends
    assert(circle.isRight && turnkey.isRight && client.isRight)
    assertEquals(local.address, address)
  }
