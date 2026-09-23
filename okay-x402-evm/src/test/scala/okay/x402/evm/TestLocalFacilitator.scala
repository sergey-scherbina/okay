package okay.x402.evm

import okay.*
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.x402.*

/** local verification in front of a remote facilitator — and the
 * snippet of docs/modules/okay-x402-evm.md, VERBATIM */
class TestLocalFacilitator extends munit.FunSuite:
  given scala.concurrent.ExecutionContext = munitExecutionContext

  private val price = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val good = Json.parse("""{"signature":"0x2d6a7588d6acca505cbf0d9a4a227e0c52c6c34008c8e8986a1283259764173608a2ce6496642e377d6da8dbbf5836e9bd15092f9ecab05ded3d6293af148b571c","authorization":{"from":"0x857b06519E91e3A54538791bDbb0E22373e36b66","to":"0x209693Bc6afc0C5328bA36FaF03C514EF312287C","value":"10000","validAfter":"1740672089","validBefore":"1740672154","nonce":"0xf3746613c2d920b5fdabc0856f2aeb2d4f88ee6037b8cc5d04a71a4462f13480"}}""")

  final class Remote extends Facilitator:
    @volatile var verifies = 0
    def verify(p: PaymentPayload, r: PaymentRequirements) = { verifies += 1; pure(VerifyResponse(true, None, Some("remote"))) }
    def settle(p: PaymentPayload, r: PaymentRequirements) = pure(SettlementResponse(true, "0xtx", r.network))
    def supported = pure(Vector.empty)

  test("a local refusal never reaches the remote; a locally valid payment is also asked remotely") {
    val remote = Remote()
    val remoteFacilitator: Facilitator = remote
    val routes: okay.http.Request => okay.http.Response ! Async = _ => pure(okay.http.Response(200, Nil, okay.http.Http.one(Array.emptyByteArray)))
    // ---- snippet: local
    val facilitator = LocalFacilitator(remoteFacilitator)   // local checks first, then the remote's balance and simulation
    val paid = Gate(_ => Some(PaymentRequired(ResourceInfo("/report"), Vector(price))), facilitator)(routes)
    // ---- snippet ends
    val _ = paid
    val atWindow = LocalFacilitator(remote, clock = () => 1740672100L)
    val tampered = Json.parse(Json.print(good).replace("\"10000\"", "\"20000\""))
    for
      bad <- Async.runAsync(atWindow.verify(PaymentPayload(price, tampered), price))
      ok <- Async.runAsync(atWindow.verify(PaymentPayload(price, good), price))
      localOnly <- Async.runAsync(LocalFacilitator(remote, alsoRemote = false, clock = () => 1740672100L)
        .verify(PaymentPayload(price, good), price))
    yield
      assertEquals(bad.invalidReason, Some("invalid_exact_evm_payload_signature"))
      assertEquals(ok.payer, Some("remote"))
      assertEquals(localOnly.payer.map(_.toLowerCase), Some("0x857b06519e91e3a54538791bdbb0e22373e36b66"))
      assertEquals(remote.verifies, 1)
  }
