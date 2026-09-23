package okay.x402.cdp

import okay.*
import okay.chain.Network
import okay.conf.Secret
import okay.http.{Http, Request, Response}
import okay.x402.*
import okay.x402.evm.EvmPayer

/** the snippet of docs/modules/okay-x402-cdp.md, VERBATIM between the
 * markers; the Http is a stand-in that is never called, since nothing
 * here meets a 402 */
class TestDocExamplesCdp extends munit.FunSuite:
  test("docs: a CDP-signed paying client from the settings") {
    val account = "0x2c7536E3605D9C16a7a3D7b1898e529396a65c23"
    val apiKeyId = "organizations/o/apiKeys/k"
    val http: Http = new Http { def send(r: Request): Response ! Async = pure(Response(200, Nil, Http.one(Array.emptyByteArray))) }
    val policy = Policy.upTo(BigInt(10000), Set(Network("eip155", "84532")), Set("0x036CbD53842c5426634e7929541eC2318f3dCF7e"))
    val consent = Consent.always
    val k = java.security.KeyPairGenerator.getInstance("Ed25519").generateKeyPair()
    val w = { val g = java.security.KeyPairGenerator.getInstance("EC"); g.initialize(java.security.spec.ECGenParameterSpec("secp256r1")); g.generateKeyPair() }
    val b64 = java.util.Base64.getEncoder
    val secrets = okay.conf.Secrets.memory(Map(
      "env:CDP_API_KEY_SECRET" -> b64.encodeToString(k.getPrivate.getEncoded.takeRight(32) ++ k.getPublic.getEncoded.takeRight(32)),
      "env:CDP_WALLET_SECRET" -> b64.encodeToString(w.getPrivate.getEncoded)))
    // the environment, stood in for: the snippet reads `Secrets.env`
    object Secrets { val env: okay.conf.Secrets = secrets }
    // ---- snippet: cdp
    val conf = CdpConf(account, apiKeyId, Secret("env:CDP_API_KEY_SECRET"), Secret("env:CDP_WALLET_SECRET"))
    val signer = CdpSigner.fromConf(conf, http, Secrets.env).fold(e => sys.error(e), identity)
    val client = Paying(http, policy, EvmPayer(signer), consent)   // pays 402s from the CDP wallet
    // ---- snippet ends
    assertEquals(signer.address, account)
    assert(client ne http)
  }
