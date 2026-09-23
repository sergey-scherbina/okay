package okay.x402.evm

import java.math.BigInteger
import okay.chain.Network
import okay.codec.Json
import okay.codec.Json.*
import okay.x402.*

/**
 * Offline `exact`/EVM verification, checked against oracles this code did
 * not produce: published keccak vectors, the address of private key 1,
 * EIP-712's own `Ether Mail` domain separator, and the x402 spec's
 * example payment — a real signature that must recover to its `from`.
 */
class TestExactEvm extends munit.FunSuite:

  test("keccak-256: the published vectors (not SHA3-256)") {
    assertEquals(Evm.hex(Evm.keccak(Array.emptyByteArray)), "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470")
    assertEquals(Evm.hex(Evm.keccak("abc".getBytes("UTF-8"))), "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45")
  }

  test("an address from a key: private key 1 is 0x7E5F…5Bdf") {
    assertEquals(Evm.addressOf(BigInteger.ONE), "0x7e5f4552091a69125d5dfcb7b8c2659029395bdf")
  }

  test("EIP-712: the Ether Mail domain separator from the EIP itself") {
    val d = Eip712.domainSeparator("Ether Mail", "1", BigInt(1), "0xCcCCccccCCCCcCCCCCCcCcCccCcCCCcCcccccccC")
    assertEquals(Evm.hex(d), "f2cee375fa42b42143804025fc449deafd50cc031ca257e0b194a650a912090f")
  }

  // the example PaymentPayload of x402's spec (§5.2.1): USDC on Base Sepolia
  private val requirements = PaymentRequirements("exact", Network("eip155", "84532"), BigInt(10000),
    "0x036CbD53842c5426634e7929541eC2318f3dCF7e", "0x209693Bc6afc0C5328bA36FaF03C514EF312287C", 60,
    Some(JObj(Vector("name" -> JStr("USDC"), "version" -> JStr("2")))))
  private val specPayload = Json.parse("""{
    "signature": "0x2d6a7588d6acca505cbf0d9a4a227e0c52c6c34008c8e8986a1283259764173608a2ce6496642e377d6da8dbbf5836e9bd15092f9ecab05ded3d6293af148b571c",
    "authorization": {
      "from": "0x857b06519E91e3A54538791bDbb0E22373e36b66",
      "to": "0x209693Bc6afc0C5328bA36FaF03C514EF312287C",
      "value": "10000", "validAfter": "1740672089", "validBefore": "1740672154",
      "nonce": "0xf3746613c2d920b5fdabc0856f2aeb2d4f88ee6037b8cc5d04a71a4462f13480" } }""")

  test("the x402 spec's example payment: its signature recovers to its `from`") {
    val p = ExactEvm.payload(specPayload).fold(fail(_), identity)
    val digest = Eip712.digest(Eip712.domainSeparator("USDC", "2", BigInt(84532), requirements.asset), p.authorization)
    assertEquals(Evm.recover(digest, p.signature).map(_.toLowerCase), Right("0x857b06519e91e3a54538791bdbb0e22373e36b66"))
    val v = ExactEvm.verify(PaymentPayload(requirements, specPayload), requirements, now = 1740672100L)
    assertEquals(v, VerifyResponse(true, None, Some("0x857b06519e91e3a54538791bdbb0e22373e36b66")))
  }

  test("every refusal has the reference implementation's reason") {
    def at(now: Long, r: PaymentRequirements = requirements, j: Json = specPayload) =
      ExactEvm.verify(PaymentPayload(r, j), r, now).invalidReason
    assertEquals(at(1740672150L), Some("invalid_exact_evm_payload_authorization_valid_before"))  // inside the 6 s margin
    assertEquals(at(1740672000L), Some("invalid_exact_evm_payload_authorization_valid_after"))
    assertEquals(at(1740672100L, requirements.copy(amount = BigInt(10001))), Some("invalid_exact_evm_payload_authorization_value"))
    // payTo is not in the signed message: the signature still recovers, and the RECIPIENT check refuses
    assertEquals(at(1740672100L, requirements.copy(payTo = "0x0000000000000000000000000000000000000001")),
      Some("invalid_exact_evm_payload_recipient_mismatch"))
    assertEquals(at(1740672100L, requirements.copy(extra = None)), Some("missing_eip712_domain"))
    assertEquals(at(1740672100L, requirements.copy(network = Network("solana", "x"))), Some("network_mismatch"))
    // a changed amount inside the signed message: the signature no longer recovers to `from`
    val tampered = Json.parse(Json.print(specPayload).replace("\"10000\"", "\"20000\""))
    assertEquals(at(1740672100L, j = tampered), Some("invalid_exact_evm_payload_signature"))
  }

  test("sign and recover, with our own key; a high-s twin is refused") {
    val key = BigInteger("4c0883a69102937d6231471b5dbb6204fe5129617082792ae468d01a3f362318", 16)
    val digest = Evm.keccak("x402".getBytes("UTF-8"))
    val sig = Evm.sign(digest, key)
    assertEquals(Evm.recover(digest, sig), Right(Evm.addressOf(key)))
    val n = BigInteger("fffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141", 16)
    val s = BigInteger(1, sig.slice(32, 64))
    val high = sig.take(32) ++ Evm.word32(n.subtract(s)) :+ (if sig(64) == 27 then 28 else 27).toByte
    assert(Evm.recover(digest, high).left.exists(_.contains("high-s")))
  }
