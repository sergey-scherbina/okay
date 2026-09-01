package okay.security

import okay.codec.Json

/**
 * The model (specs/security.md): who was authenticated, what a token
 * asserts, whether a proof held, and what a principal may do — all
 * VALUES. Nothing here knows a platform API; crypto is the seam next
 * door, and refusal is a value with a reason, never a throw — the
 * totality rule every parser in this stack already follows, applied
 * to the one input that is hostile by definition.
 */
final case class Principal(id: String, name: String, claims: Claims)

final case class Claims(issuer: Option[String] = None,
                        subject: Option[String] = None,
                        audience: Vector[String] = Vector.empty,
                        expires: Option[Long] = None,
                        notBefore: Option[Long] = None,
                        issuedAt: Option[Long] = None,
                        scopes: Set[String] = Set.empty,
                        json: Json = Json.JObj(Vector.empty))

object Claims {

  import Json.*

  /** the registered names, read off a payload; everything stays in
   * `json` too, so nothing a token carried is lost */
  def of(j: Json): Claims = Claims(
    issuer = str(j, "iss"),
    subject = str(j, "sub"),
    audience = field(j, "aud") match
      case Some(JStr(a)) => Vector(a)
      case Some(JArr(vs)) => vs.collect { case JStr(a) => a }
      case _ => Vector.empty,
    expires = num(j, "exp"),
    notBefore = num(j, "nbf"),
    issuedAt = num(j, "iat"),
    scopes = str(j, "scope").map(_.split(' ').filter(_.nonEmpty).toSet)
      .orElse(field(j, "scp").collect { case JArr(vs) =>
        vs.collect { case JStr(s) => s }.toSet })
      .getOrElse(Set.empty),
    json = j)

  /** the payload of these claims — the registered names plus whatever
   * `json` already carried (registered names win) */
  def json(c: Claims): Json =
    val registered = Vector(
      c.issuer.map("iss" -> JStr(_)),
      c.subject.map("sub" -> JStr(_)),
      (c.audience.length match
        case 0 => None
        case 1 => Some("aud" -> JStr(c.audience.head))
        case _ => Some("aud" -> JArr(c.audience.map(JStr(_))))),
      c.expires.map(n => "exp" -> JNum(n.toDouble)),
      c.notBefore.map(n => "nbf" -> JNum(n.toDouble)),
      c.issuedAt.map(n => "iat" -> JNum(n.toDouble)),
      (if c.scopes.isEmpty then None
       else Some("scope" -> JStr(c.scopes.toVector.sorted.mkString(" "))))).flatten
    val extra = c.json match
      case JObj(fs) => fs.filterNot((k, _) => registered.exists(_._1 == k))
      case _ => Vector.empty
    JObj(registered ++ extra)

  private[security] def field(j: Json, n: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (k, v) if k == n => v }
    case _ => None
  private[security] def str(j: Json, n: String): Option[String] =
    field(j, n).collect { case JStr(s) => s }
  private[security] def num(j: Json, n: String): Option[Long] =
    field(j, n).collect { case JNum(x) => x.toLong }
}

/** did the proof hold? A refusal names its reason — callers log it,
 * attackers read a uniform 401 that does not */
enum Verified:
  case Ok(principal: Principal)
  case No(reason: String)

/**
 * The platform seam: every primitive stage 0 needs, and nothing
 * more. Given on the JVM over JCA; a node:crypto given is the
 * security-node stage. The model above never touches a platform API.
 */
trait Crypto:
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte]
  def sha256(data: Array[Byte]): Array[Byte]
  def signRsaSha256(key: Crypto.Handle, data: Array[Byte]): Array[Byte]
  def verifyRsaSha256(key: Crypto.Handle, data: Array[Byte], sig: Array[Byte]): Boolean
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte]
  def randomBytes(n: Int): Array[Byte]
  /** an RSA public key from its material (a JWKS entry's n and e) —
   * None where the platform cannot build one, which is what makes
   * JWKS parseable on every platform and verifying where keys exist */
  def rsaPublicKey(modulus: BigInt, exponent: BigInt): Option[Crypto.Handle]
  /** ECDSA P-256/SHA-256, DER-shaped at the seam (each platform's
   * native form); the JOSE raw R||S dance is Es256's, pure and shared */
  def signEcdsaSha256(key: Crypto.Handle, data: Array[Byte]): Array[Byte]
  def verifyEcdsaSha256(key: Crypto.Handle, data: Array[Byte], derSig: Array[Byte]): Boolean
  /** a P-256 public key from its point (a JWKS entry's x and y) —
   * None where the platform cannot build one, same rule as RSA */
  def ecPublicKey(x: BigInt, y: BigInt): Option[Crypto.Handle]

object Crypto:
  /**
   * A PLATFORM key, opaquely: `java.security` types do not exist on
   * JS even as signatures the linker will accept, so the shared
   * surface carries a handle and each platform knows what it wrapped.
   * The JVM's typed constructors live in `Keys` (scala-jvm); the cast
   * back happens at the seam and nowhere else.
   */
  opaque type Handle = AnyRef
  object Handle:
    def apply(a: AnyRef): Handle = a
    extension (h: Handle) def value: AnyRef = h

  /** constant-time equality — the compare that does not leak how far
   * it got; every verifier here goes through it */
  def constantTimeEquals(a: Array[Byte], b: Array[Byte]): Boolean =
    if a.length != b.length then false
    else
      var diff = 0
      var i = 0
      while i < a.length do
        diff |= (a(i) ^ b(i))
        i += 1
      diff == 0

// ---------------------------------------------------------------- policy

/** may this principal do this action to this resource? Total. */
type Policy = (Principal, String, String) => Decision

enum Decision:
  case Permit
  case Deny(why: String)

object Policy {

  val allowAll: Policy = (_, _, _) => Decision.Permit

  /** the principal must carry this scope */
  def scoped(scope: String): Policy = (p, _, _) =>
    if p.claims.scopes(scope) then Decision.Permit
    else Decision.Deny(s"missing scope '$scope'")

  /** the principal must carry this role (a `roles` claim, array or
   * space-separated string — both occur in the wild) */
  def role(r: String): Policy = (p, _, _) =>
    val roles = Claims.field(p.claims.json, "roles") match
      case Some(Json.JArr(vs)) => vs.collect { case Json.JStr(s) => s }.toSet
      case Some(Json.JStr(s)) => s.split(' ').toSet
      case _ => Set.empty[String]
    if roles(r) then Decision.Permit else Decision.Deny(s"missing role '$r'")

  /** every one must permit; the first refusal names the whole */
  def allOf(ps: Policy*): Policy = (p, a, r) =>
    ps.iterator.map(_(p, a, r)).collectFirst { case d: Decision.Deny => d }
      .getOrElse(Decision.Permit)

  /** any one suffices; refused only when all refuse */
  def anyOf(ps: Policy*): Policy = (p, a, r) =>
    if ps.exists(_(p, a, r) == Decision.Permit) then Decision.Permit
    else Decision.Deny(ps.map(_(p, a, r)).collect {
      case Decision.Deny(w) => w }.mkString("; "))
}
