package okay.x402.evm

import java.nio.charset.StandardCharsets.UTF_8
import okay.*
import okay.codec.Json
import okay.codec.Json.*
import okay.x402.*

/**
 * EIP-712 typed-data hashing for EIP-3009's `TransferWithAuthorization`
 * — the one message x402's `exact` scheme signs on EVM.
 */
object Eip712:
  private def utf8Keccak(s: String) = Evm.keccak(s.getBytes(UTF_8))
  private def uint(x: BigInt): Array[Byte] = Evm.word32(x.bigInteger)
  private def addr(a: String): Array[Byte] =
    val b = Evm.unhex(a)
    require(b.length == 20, s"not a 20-byte address: $a")
    new Array[Byte](12) ++ b

  private val domainType = utf8Keccak("EIP712Domain(string name,string version,uint256 chainId,address verifyingContract)")
  private val transferType = utf8Keccak(
    "TransferWithAuthorization(address from,address to,uint256 value,uint256 validAfter,uint256 validBefore,bytes32 nonce)")

  def domainSeparator(name: String, version: String, chainId: BigInt, verifyingContract: String): Array[Byte] =
    Evm.keccak(domainType ++ utf8Keccak(name) ++ utf8Keccak(version) ++ uint(chainId) ++ addr(verifyingContract))

  /** the EIP-712 domain of a token that implements EIP-3009 */
  final case class Domain(name: String, version: String, chainId: BigInt, verifyingContract: String)

  final case class Authorization(from: String, to: String, value: BigInt, validAfter: BigInt,
                                 validBefore: BigInt, nonce: Array[Byte])

  def structHash(a: Authorization): Array[Byte] =
    require(a.nonce.length == 32, "the nonce is 32 bytes")
    Evm.keccak(transferType ++ addr(a.from) ++ addr(a.to) ++ uint(a.value) ++ uint(a.validAfter) ++
      uint(a.validBefore) ++ a.nonce)

  /** what is signed: 0x19 0x01 ‖ domain separator ‖ struct hash, hashed */
  def digest(domain: Array[Byte], a: Authorization): Array[Byte] =
    Evm.keccak(Array[Byte](0x19, 0x01) ++ domain ++ structHash(a))

  def digest(d: Domain, a: Authorization): Array[Byte] =
    digest(domainSeparator(d.name, d.version, d.chainId, d.verifyingContract), a)

  /** the typed data of an EIP-3009 transfer as JSON — what a signing
   * service that signs TYPED DATA is sent (`eth_signTypedData_v4`'s
   * object; CDP, Circle and Turnkey take this shape) */
  def typedData(d: Domain, a: Authorization): Json =
    def field(n: String, t: String) = JObj(Vector("name" -> JStr(n), "type" -> JStr(t)))
    JObj(Vector(
      "domain" -> JObj(Vector("name" -> JStr(d.name), "version" -> JStr(d.version),
        "chainId" -> JNum(d.chainId.toDouble), "verifyingContract" -> JStr(d.verifyingContract))),
      "types" -> JObj(Vector(
        "EIP712Domain" -> JArr(Vector(field("name", "string"), field("version", "string"),
          field("chainId", "uint256"), field("verifyingContract", "address"))),
        "TransferWithAuthorization" -> JArr(Vector(field("from", "address"), field("to", "address"),
          field("value", "uint256"), field("validAfter", "uint256"), field("validBefore", "uint256"),
          field("nonce", "bytes32"))))),
      "primaryType" -> JStr("TransferWithAuthorization"),
      "message" -> JObj(Vector("from" -> JStr(a.from), "to" -> JStr(a.to), "value" -> JStr(a.value.toString),
        "validAfter" -> JStr(a.validAfter.toString), "validBefore" -> JStr(a.validBefore.toString),
        "nonce" -> JStr("0x" + Evm.hex(a.nonce))))))

  /**
   * A signature a SERVICE returned, checked before it is used: `v` as
   * 0/1 or 27/28, whichever the service writes (FiatToken wants 27/28),
   * then RECOVERED against the digest this process computes — it must be
   * the account's. A service answering for another key does not get to
   * sign our payments; the refusal names both addresses.
   */
  def checked(d: Domain, a: Authorization, raw: Array[Byte], account: String): Either[String, Array[Byte]] =
    val sig = if raw.length == 65 && (raw(64) & 0xFF) < 27 then raw.updated(64, (raw(64) + 27).toByte) else raw
    Evm.recover(digest(d, a), sig) match
      case Right(who) if who.equalsIgnoreCase(account) => Right(sig)
      case Right(who) => Left(s"the signature recovers to $who, not the account $account")
      case Left(e) => Left(s"the signature does not recover: $e")

/**
 * x402's `exact` scheme on EVM (EIP-3009), verified OFFLINE — the checks
 * the reference facilitator makes that need no chain, with its own
 * `invalidReason` codes (coinbase/x402 typescript/packages/mechanisms/
 * evm/src/exact/facilitator/scheme.ts), so a refusal reads the same:
 *
 * - `unsupported_scheme`, `network_mismatch` (not eip155),
 *   `missing_eip712_domain` (no `extra.name`/`extra.version`);
 * - `invalid_exact_evm_payload_signature` — the signature must RECOVER
 *   to `authorization.from` (ECDSA; an EIP-1271/6492 smart wallet needs
 *   the chain and is refused here);
 * - `invalid_exact_evm_payload_recipient_mismatch` — `to` is `payTo`;
 * - `invalid_exact_evm_payload_authorization_valid_before` /
 *   `_valid_after` — the window, with the reference's 6-second margin;
 * - `invalid_exact_evm_payload_authorization_value` — `value >= amount`.
 *
 * NOT checked, because they need an RPC: the payer's balance and a
 * simulated `transferWithAuthorization` (the scheme's steps 2 and 5).
 */
object ExactEvm:
  final case class Payload(signature: Array[Byte], authorization: Eip712.Authorization)

  def payload(j: Json): Either[String, Payload] =
    def obj(x: Json, k: String): Either[String, Json] = x match
      case JObj(fs) => fs.collectFirst { case (`k`, v) => v }.toRight(s"missing '$k'")
      case _ => Left(s"not an object where '$k' was expected")
    def str(x: Json, k: String): Either[String, String] = obj(x, k).flatMap {
      case JStr(s) => Right(s); case other => Left(s"'$k' is not a string: $other") }
    def num(x: Json, k: String): Either[String, BigInt] = str(x, k).flatMap(s =>
      scala.util.Try(BigInt(s)).toOption.toRight(s"'$k' is not an integer: '$s'"))
    def bytes(x: Json, k: String): Either[String, Array[Byte]] = str(x, k).flatMap(s =>
      scala.util.Try(Evm.unhex(s)).toEither.left.map(_ => s"'$k' is not hex"))
    for
      sig <- bytes(j, "signature")
      a <- obj(j, "authorization")
      from <- str(a, "from"); to <- str(a, "to"); value <- num(a, "value")
      after <- num(a, "validAfter"); before <- num(a, "validBefore"); nonce <- bytes(a, "nonce")
    yield Payload(sig, Eip712.Authorization(from, to, value, after, before, nonce))

  private def refuse(why: String) = VerifyResponse(false, Some(why))

  /** verify at time `now` (unix seconds) */
  def verify(p: PaymentPayload, r: PaymentRequirements, now: Long): VerifyResponse =
    if r.scheme != "exact" || p.accepted.scheme != "exact" then refuse("unsupported_scheme")
    else if r.network.namespace != "eip155" || !r.network.reference.forall(_.isDigit) then refuse("network_mismatch")
    else
      val domain = r.extra.flatMap {
        case JObj(fs) =>
          for case JStr(n) <- fs.collectFirst { case ("name", v) => v }; case JStr(v) <- fs.collectFirst { case ("version", x) => x }
          yield (n, v)
        case _ => None
      }
      domain match
        case None => refuse("missing_eip712_domain")
        case Some((name, version)) => payload(p.payload) match
          case Left(e) => refuse(s"invalid_exact_evm_payload: $e")
          case Right(pl) =>
            val a = pl.authorization
            val digest = scala.util.Try(Eip712.digest(
              Eip712.domainSeparator(name, version, BigInt(r.network.reference), r.asset), a)).toEither
            digest.left.map(_ => "invalid_exact_evm_payload_signature").flatMap(d => Evm.recover(d, pl.signature)) match
              case Left(_) => refuse("invalid_exact_evm_payload_signature")
              case Right(signer) if !signer.equalsIgnoreCase(a.from) => refuse("invalid_exact_evm_payload_signature")
              case Right(signer) =>
                if !a.to.equalsIgnoreCase(r.payTo) then refuse("invalid_exact_evm_payload_recipient_mismatch")
                else if a.validBefore < BigInt(now + 6) then refuse("invalid_exact_evm_payload_authorization_valid_before")
                else if a.validAfter > BigInt(now) then refuse("invalid_exact_evm_payload_authorization_valid_after")
                else if a.value < r.amount then refuse("invalid_exact_evm_payload_authorization_value")
                else VerifyResponse(true, None, Some(signer))

/**
 * Verify LOCALLY, then — when given — ask the remote facilitator too
 * (for the balance and the simulation this cannot do); settle through
 * the remote one, since broadcasting needs a node. A local refusal never
 * reaches the remote.
 */
final class LocalFacilitator(remote: Facilitator, alsoRemote: Boolean = true,
                             clock: () => Long = () => java.lang.System.currentTimeMillis() / 1000) extends Facilitator:
  def verify(p: PaymentPayload, r: PaymentRequirements): VerifyResponse ! Async =
    val local = ExactEvm.verify(p, r, clock())
    if !local.isValid || !alsoRemote then pure(local) else remote.verify(p, r)
  def settle(p: PaymentPayload, r: PaymentRequirements): SettlementResponse ! Async = remote.settle(p, r)
  def supported: Vector[SupportedKind] ! Async = remote.supported
