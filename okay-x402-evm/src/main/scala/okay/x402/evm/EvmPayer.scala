package okay.x402.evm

import java.math.BigInteger
import okay.*
import okay.codec.Json.*
import okay.x402.*

/**
 * What `EvmPayer` needs from whoever holds the key (specs/x402.md stage
 * 4b): an address, and a signature (`r ‖ s ‖ v`, low-s, v 27/28) over an
 * EIP-3009 AUTHORIZATION in its EIP-712 domain. Two kinds implement it:
 * a `Signer`, which signs the DIGEST and so sees 32 opaque bytes, and a
 * service that signs the TYPED DATA itself (`CdpSigner`, okay-x402-cdp)
 * and so sees what it signs — the kind whose own policies can refuse a
 * payment (a limit, a contract), which a digest does not allow.
 */
trait AuthorizationSigner:
  def address: String
  def signAuthorization(domain: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async

/**
 * The key, behind ONE operation (specs/x402.md stage 4): sign a 32-byte
 * digest as `r ‖ s ‖ v` (low-s, v 27/28 — what USDC's FiatToken
 * accepts), and say which address that is. A KMS, an HSM or a remote
 * wallet implements this; nothing else in the payment path ever holds
 * key material.
 */
trait Signer extends AuthorizationSigner:
  def sign(digest: Array[Byte]): Array[Byte] ! Async
  def signAuthorization(d: Eip712.Domain, a: Eip712.Authorization): Array[Byte] ! Async =
    sign(Eip712.digest(Eip712.domainSeparator(d.name, d.version, d.chainId, d.verifyingContract), a))

object Signer:
  /**
   * A private key held in this process's memory. For DEVELOPMENT and
   * tests: the key is as safe as the heap, the process and whatever
   * config it was read from. Production keys belong behind a `Signer`
   * whose key never enters the JVM.
   */
  def local(privateKey: BigInteger): Signer = new Signer:
    val address: String = Evm.addressOf(privateKey)
    def sign(digest: Array[Byte]): Array[Byte] ! Async = okay.async(Evm.sign(digest, privateKey))

/**
 * A `Payer` for x402's `exact` scheme on EVM: an EIP-3009
 * `TransferWithAuthorization`, EIP-712-signed by `signer`, built as the
 * reference client builds it (coinbase/x402 typescript
 * mechanisms/evm/src/exact/client/scheme.ts): `validAfter = now − 600`,
 * `validBefore = now + maxTimeoutSeconds`, a random 32-byte nonce, `to` =
 * `payTo`, `value` = the amount asked.
 *
 * It DECLINES what it cannot sign honestly — another scheme, a network
 * that is not `eip155:<chainId>`, requirements with no EIP-712 domain in
 * `extra` — rather than signing something the verifier will refuse. It
 * checks nothing about WHETHER to pay: that is `Policy` and `Consent`,
 * decided before a payer is asked.
 */
final class EvmPayer(signer: AuthorizationSigner,
                     clock: () => Long = () => java.lang.System.currentTimeMillis() / 1000,
                     nonce: () => Array[Byte] = EvmPayer.randomNonce) extends Payer:
  def pay(r: PaymentRequirements, resource: ResourceInfo): Option[PaymentPayload] ! Async =
    EvmPayer.domain(r) match
      case None => pure(None)
      case Some((name, version, chainId)) =>
        val now = clock()
        val a = Eip712.Authorization(signer.address, r.payTo, r.amount, BigInt(now - 600),
          BigInt(now + r.maxTimeoutSeconds), nonce())
        signer.signAuthorization(Eip712.Domain(name, version, chainId, r.asset), a).map { sig =>
          Some(PaymentPayload(r, JObj(Vector(
            "signature" -> JStr("0x" + Evm.hex(sig)),
            "authorization" -> JObj(Vector(
              "from" -> JStr(a.from), "to" -> JStr(a.to), "value" -> JStr(a.value.toString),
              "validAfter" -> JStr(a.validAfter.toString), "validBefore" -> JStr(a.validBefore.toString),
              "nonce" -> JStr("0x" + Evm.hex(a.nonce)))))), Some(resource)))
        }

object EvmPayer:
  private val random = java.security.SecureRandom()
  val randomNonce: () => Array[Byte] = () =>
    val b = new Array[Byte](32)
    random.nextBytes(b)
    b

  /** the EIP-712 domain the requirements name, or why not */
  def domain(r: PaymentRequirements): Option[(String, String, BigInt)] =
    if r.scheme != "exact" || r.network.namespace != "eip155" ||
       r.network.reference.isEmpty || !r.network.reference.forall(_.isDigit) then None
    else r.extra.flatMap {
      case JObj(fs) =>
        for case JStr(n) <- fs.collectFirst { case ("name", v) => v }; case JStr(v) <- fs.collectFirst { case ("version", x) => x }
        yield (n, v, BigInt(r.network.reference))
      case _ => None
    }
