# okay-security — authorization for services, once

## Overview
Every service in this stack keeps arriving at the same questions —
who is this, what may they do, how do they prove it — and the answers
keep being deferred: MCP's spec parked OAuth, the HTTP servers serve
everything to everyone, the server-driven UI has sessions with no
user behind them. This module answers them ONCE, as a self-contained,
reusable layer, laid out to its full extent here and built in stages
— the persist doctrine: each stage a working system, none of it a
dead end.

The shape follows the house rules rather than the industry's
frameworks: identities, claims and decisions are VALUES; crypto is a
SEAM (a trait the platform fills — JCA on the JVM, node:crypto later
on JS); protection is a ROUTE WRAPPER, not a filter chain; and the
whole of stage 0 costs zero dependencies, because the JDK already
carries HMAC, RSA, SHA-2, PBKDF2 and SecureRandom.

## The model

- **Principal** — who was authenticated: an id, a display name, the
  claims that came with it. A value; nothing here knows HOW it was
  authenticated.
- **Claims** — what a token asserts: issuer, subject, audience,
  expiry, scopes, arbitrary fields (a `Json`). Validation of TIME and
  AUDIENCE is the library's job (with skew); meaning is the caller's.
- **Credential** — how the caller proved it: a bearer token, an API
  key, a password. Each verifies into a Principal or a stated refusal.
- **Policy** — what they may do: `(Principal, action, resource) =>
  Decision`, a total function; Decision is Permit | Deny(why). Combinators:
  `allOf`, `anyOf`, `scoped("read")`, `role("admin")`. Deliberately
  small — an algebra, not a rules engine.
- **Crypto** — the seam: HMAC, signatures, secure random, PBKDF2.
  Given on the JVM (JCA); the JS given is a stage; the model above
  never touches a platform API directly.

## Interface (stage 0)

```scala
final case class Principal(id: String, name: String, claims: Claims)
final case class Claims(issuer: Option[String], subject: Option[String],
                        audience: Vector[String], expires: Option[Long],
                        notBefore: Option[Long], issuedAt: Option[Long],
                        scopes: Set[String], json: Json)

enum Verified:
  case Ok(principal: Principal)
  case No(reason: String)              // a refusal is a VALUE with a why

trait Crypto:                           // the platform seam
  def hmacSha256(key: Array[Byte], data: Array[Byte]): Array[Byte]
  def signRsaSha256(key: java.security.PrivateKey, data: Array[Byte]): Array[Byte]
  def verifyRsaSha256(key: java.security.PublicKey, data: Array[Byte], sig: Array[Byte]): Boolean
  def pbkdf2(password: Array[Char], salt: Array[Byte], iterations: Int, bits: Int): Array[Byte]
  def randomBytes(n: Int): Array[Byte]

object Jwt:
  enum Key:                             // what verifies/signs
    case Hmac(secret: Array[Byte])
    case RsaPublic(key: java.security.PublicKey)
    case RsaPair(pub: java.security.PublicKey, priv: java.security.PrivateKey)
  def sign(claims: Claims, key: Key, kid: Option[String] = None): String
  def verify(token: String, keys: String => Option[Key],   // by kid ("" when absent)
             audience: Option[String], now: Long, skew: Long = 60): Verified

object Jwks:
  def parse(j: Json): Map[String, Jwt.Key]                  // RSA n/e, by kid
  def fetch(http: Http, url: String): Map[String, Jwt.Key] ! Async

object Password:
  def hash(password: Array[Char]): String                   // pbkdf2$iters$salt$hash
  def verify(password: Array[Char], stored: String): Boolean  // constant-time

object ApiKey:
  def issue(): (String, String)          // (the key to hand out, its stored digest)
  def verify(presented: String, digest: String): Boolean     // constant-time

type Policy = (Principal, String, String) => Decision
enum Decision { case Permit; case Deny(why: String) }

object Secure:                           // okay-http integration
  /** wrap a route: extract the bearer, verify, apply policy; a
   * refusal is 401/403 with WWW-Authenticate, never an exception */
  def bearer(verify: String => Verified, policy: Policy = allowAll)
            (route: Principal => PartialFunction[Request, Response ! Async])
  : PartialFunction[Request, Response ! Async]

object OAuth2:                           // the client flows, over trait Http
  final case class Client(id: String, secret: Option[String],
                          authEndpoint: String, tokenEndpoint: String,
                          redirectUri: String, scopes: Seq[String])
  final case class Tokens(access: String, refresh: Option[String],
                          expiresIn: Option[Long], idToken: Option[String])
  def pkce(): (verifier: String, challenge: String)          // S256
  def authorizationUrl(c: Client, state: String, challenge: String): String
  def exchange(http: Http, c: Client, code: String, verifier: String): Either[String, Tokens] ! Async
  def refresh(http: Http, c: Client, token: String): Either[String, Tokens] ! Async
  def clientCredentials(http: Http, c: Client): Either[String, Tokens] ! Async
```

## Behavior (stage 0)
- [ ] a JWT round-trips: sign with HS256 and with RS256, verify with
      the right key; a TAMPERED payload, a wrong key, a wrong
      audience, an expired token and a not-yet-valid token are each a
      `No` with its own reason — never a throw
- [ ] clock skew: a token expiring "just now" verifies within the
      skew window and not beyond it
- [ ] `alg: none` and an alg/key MISMATCH (an HS256 token against an
      RSA key — the classic confusion attack) are refused
- [ ] JWKS: a real JWKS JSON (RSA n/e, kid) parses into verifying
      keys; verify picks the key by the token's kid
- [ ] passwords: hash-then-verify; a wrong password refuses; two
      hashes of one password differ (salt); the stored form carries
      its own parameters so iterations can rise without migration
- [ ] API keys: issue/verify round-trips; the digest alone (what a
      database would leak) does not verify
- [ ] the policy algebra: scoped/role/allOf/anyOf compose; a Deny
      names its reason
- [ ] `Secure.bearer` on a real okay-http server: no token is 401
      with WWW-Authenticate, a bad token is 401, a good token without
      the scope is 403, a good token with it reaches the route and
      the route sees the Principal
- [ ] OAuth2 code+PKCE against a STUB authorization server (an
      okay-http route): the url carries the S256 challenge, exchange
      posts the verifier and yields tokens, refresh rotates, client
      credentials works; a token-endpoint error is a Left, not a throw
- [ ] nothing in the module throws on hostile input: fuzzed garbage
      into Jwt.verify/Jwks.parse/Password.verify answers refusals

## Stages
- **0 — the core (this claim)**: everything above, JVM crypto.
- **1 — security-mcp**: MCP authorization per its spec — RFC 9728
  protected-resource metadata served beside `McpHttp.route`, 401
  challenges, client-side discovery → PKCE → bearer retry on the
  McpLink.
- **2 — security-node**: the Crypto seam over node:crypto; the JS leg
  verifies.
- **3 — security-oidc**: id_token validation, discovery, nonce.
- Satellites when needed: argon2 (a real KDF, with a dependency),
  ES256 (the JOSE raw-vs-DER signature dance, its own tested task).

## Out of scope (module-wide, until a stage names them)
- being an authorization SERVER (issuing codes/tokens to third
  parties) — stage 0 issues only its own JWTs and API keys; the stub
  AS in tests is a test
- dynamic client registration (RFC 7591), token introspection,
  revocation endpoints
- SAML, WebAuthn/passkeys, mTLS
- cookies and CSRF (arrives with a browser-session story, likely
  beside okay-ui's wire)

## Decisions
- **Zero dependencies, so PBKDF2** — the JDK's own KDF. Argon2 is
  better and is a dependency; it becomes a satellite for NEW password
  stores, and the stored-form-carries-parameters rule means adopting
  it later migrates hash by hash, not by flag day.
- **A refusal is a value with a reason** — `Verified.No(why)` /
  `Deny(why)`, logged by callers who care; exceptions are for broken
  invariants, not for wrong passwords. The same totality rule as
  every parser here.
- **The route wrapper, not middleware chains** — `Secure.bearer`
  takes the route as `Principal => PartialFunction`, so the protected
  route CANNOT be reached without a principal in scope: the type
  system holds the door, not the call order.
- **HS256+RS256 first, ES256 staged** — ES256's JOSE signature format
  (raw R||S) differs from JCA's (DER) and that conversion deserves
  its own tested task rather than a footnote in this one.
