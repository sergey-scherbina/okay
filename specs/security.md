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
- [x] a JWT round-trips: sign with HS256 and with RS256, verify with
      the right key; a TAMPERED payload, a wrong key, a wrong
      audience, an expired token and a not-yet-valid token are each a
      `No` with its own reason — never a throw
- [x] clock skew: a token expiring "just now" verifies within the
      skew window and not beyond it
- [x] `alg: none` and an alg/key MISMATCH (an HS256 token against an
      RSA key — the classic confusion attack) are refused
- [x] JWKS: a real JWKS JSON (RSA n/e, kid) parses into verifying
      keys; verify picks the key by the token's kid
- [x] passwords: hash-then-verify; a wrong password refuses; two
      hashes of one password differ (salt); the stored form carries
      its own parameters so iterations can rise without migration
- [x] API keys: issue/verify round-trips; the digest alone (what a
      database would leak) does not verify
- [x] the policy algebra: scoped/role/allOf/anyOf compose; a Deny
      names its reason
- [x] `Secure.bearer` on a real okay-http server: no token is 401
      with WWW-Authenticate, a bad token is 401, a good token without
      the scope is 403, a good token with it reaches the route and
      the route sees the Principal
- [x] OAuth2 code+PKCE against a STUB authorization server (an
      okay-http route): the url carries the S256 challenge, exchange
      posts the verifier and yields tokens, refresh rotates, client
      credentials works; a token-endpoint error is a Left, not a throw
- [x] nothing in the module throws on hostile input: fuzzed garbage
      into Jwt.verify/Jwks.parse/Password.verify answers refusals

## Stages
- **0 — the core**: everything above, JVM crypto. SHIPPED.
- **1 — security-mcp (this claim)**: MCP authorization per its spec.

  ```scala
  object McpAuth:   // okay-security, jvm — it sees McpHttp through okay-http
    /** the RFC 9728 document, served at /.well-known/oauth-protected-resource */
    def metadata(resource: String, authorizationServers: Seq[String])
    : PartialFunction[Request, Response ! Async]

    /** the MCP route, protected: no/bad bearer answers 401 with
     * WWW-Authenticate carrying resource_metadata=<url>; a good one
     * reaches the route (the principal is on the request's watch,
     * policy applied) */
    def protect(verify: String => Verified, metadataUrl: String,
                policy: Policy = Policy.allowAll)
               (route: Request => Response ! Async): Request => Response ! Async

    /** client side: from an MCP url to its authorization server's
     * endpoints — probe (expect 401), read resource_metadata, fetch
     * RFC 9728 doc, fetch RFC 8414 AS metadata */
    final case class Discovered(resource: String, authServer: String,
                                authEndpoint: String, tokenEndpoint: String)
    def discover(http: Http, mcpUrl: String): Either[String, Discovered] ! Async

    /** the whole client dance for a non-interactive grant: discover,
     * client credentials, a bearer-carrying link */
    def connect(http: Http, mcpUrl: String, clientId: String,
                secret: Option[String], scopes: Seq[String])
    : Either[String, McpHttp.McpLink] ! Async
  ```

  `McpHttp.link` grows `bearer: Option[() => Option[String]]` — the
  supplier is asked per request, so a refreshed token is picked up
  without rebuilding the link. The interactive code+PKCE path uses
  stage 0's pieces (`authorizationUrl`/`exchange`) with the browser
  the caller owns; `connect` covers the machine-to-machine grant.

  - [x] an unauthenticated MCP request answers 401 whose
        WWW-Authenticate names resource_metadata, and that url serves
        the RFC 9728 document naming the authorization server
  - [x] discover walks the chain: 401 -> resource metadata -> AS
        metadata -> endpoints; each missing link is a named Left
  - [x] the whole loop against a stub AS: connect obtains a token by
        client credentials and the SAME agent tool-call that works
        unauthenticated on an open server works on the protected one
        — with nothing above the link changed
  - [x] a wrong-scope token is 403 by policy; the metadata documents
        stay servable without any token (they are how you LEARN to
        authenticate)
  - [x] hostile: a metadata document pointing at a hostile AS is the
        caller's to distrust — discover surfaces the AS url it found,
        and connect only talks to it if the caller proceeds (recorded
        as the trust boundary, not silently followed)... reformulated:
        discover ANSWERS what it found; connect takes the Discovered
        VALUE, so the caller sees the AS before any secret goes to it
- **2 — security-node**: SHIPPED. The Crypto seam over node:crypto:
  HMAC-SHA256, SHA-256, PBKDF2 and secure random are real on JS, and
  the JS suite runs the SAME shared code under Node — HS256 JWTs,
  passwords, API keys, and PKCE pinned to the RFC 7636 appendix-B
  vector (checked against the standard, not against ourselves).

  What the linker taught: `java.security` key types do not exist on
  JS even as signatures, so the shared surface now carries an OPAQUE
  `Crypto.Handle` — each platform knows what it wrapped, the JVM's
  typed doors are `Keys.rsaPublic/rsaPair` (scala-jvm), the cast back
  happens at the seam and nowhere else, and JWKS PARSES everywhere
  while verifying where keys exist (`rsaPublicKey` answers None on
  JS). RS256 stays JVM with that reason; a JWK-native verify is the
  follow-up if JS ever needs it.
- **3 — security-oidc**: SHIPPED. User login assembled from the
  pieces already here: discovery is one GET, the login URL is
  OAuth2's with `openid` and a nonce, the tokens come from
  OAuth2.exchange, and id_token validation adds exactly four checks
  to Jwt.verify's — issuer, audience-is-the-client, nonce, at_hash
  (the left half of sha256(access_token), so a spliced access token
  cannot ride a genuine id_token). The stub-IdP test walks the whole
  login and then forges everything forgeable: wrong issuer, wrong
  nonce, stolen access token, wrong audience, expired, and a
  stranger's signature — each refused by ITS name, because an
  id_token is exactly the input an attacker crafts.
- Satellites when needed: argon2 (a real KDF, with a dependency),
  ES256 (the JOSE raw-vs-DER signature dance, its own tested task).
- **4 — security-es256**: SHIPPED. ES256 (ECDSA over P-256 with
  SHA-256) joins HS256/RS256. The task IS the signature format: JOSE
  carries `R||S` — two fixed 32-byte big-endian integers, 64 bytes
  total — while JCA and node speak DER `SEQUENCE(INTEGER r, INTEGER
  s)`, where integers shed leading zeros and grow a 0x00 pad when the
  high bit is set. The conversion is pure bytes, platform-free, and
  hostile input hits it first (the signature segment of a JWT is
  attacker-supplied), so it lives in its own object with its own
  battery.

  ```scala
  object Es256:   // shared, pure — no crypto, only bytes
    /** DER SEQUENCE(INTEGER r, INTEGER s) -> raw R||S (64 bytes);
     * None for anything that is not exactly that shape */
    def derToJose(der: Array[Byte]): Option[Array[Byte]]
    /** raw R||S (exactly 64 bytes) -> DER; None otherwise */
    def joseToDer(raw: Array[Byte]): Option[Array[Byte]]
  ```

  - `Jwt.Key` gains `EcPublic(key)` / `EcPair(pub, priv)`; the key
    still decides the algorithm (`ES256`), the token only agrees —
    the same confusion defusal as stage 0.
  - `Crypto` gains `signEcdsaSha256`/`verifyEcdsaSha256` (DER at the
    seam — each platform's native shape) and `ecPublicKey(x, y)`
    (P-256). JS answers None/false like RSA, same reason, same
    follow-up.
  - `Jwks.parse` accepts `kty: EC, crv: P-256` entries; damaged or
    non-P-256 entries are skipped, not thrown.
  - `Keys` (jvm) gains `ecPublic`/`ecPair` doors.

  Behavior:
  - [x] round-trip: sign with EcPair -> compact JWT whose signature
        segment is EXACTLY 64 bytes (proof it is JOSE, not DER);
        verify with EcPublic -> Ok with the principal
  - [x] the dance is total both ways: high-bit r/s gain and shed the
        0x00 pad, short r left-pads to 32, r=0 encodes as one zero
        byte; derToJose(joseToDer(raw)) == raw for every valid raw
  - [x] hostile bytes refuse as None: raw of length 0/63/65, DER
        truncated mid-integer, wrong outer tag, integer longer than
        33 bytes, trailing garbage after the sequence
  - [x] a 63- or 65-byte signature segment on the wire is a refusal,
        not a throw
  - [x] confusion refused both ways: an ES256 token against an Hmac
        or RSA key, and an HS256/RS256 token against an EC key
  - [x] tampered payload and a stranger's EC key both refuse by name
  - [x] JWKS: an EC entry parses and its key verifies; an entry
        missing y, and one with crv P-384, are skipped while the
        good keys around them survive
  - [x] the pure battery runs on JS too (shared test) — the dance
        exists everywhere even while EC keys are JVM-only

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

## Results (stage 0)
Shipped 2026-09-01: the model, Jwt (HS256/RS256, kid lookup, skew),
Jwks (RSA, damaged entries skipped), Password (pbkdf2 with parameters
in the stored form), ApiKey, the policy algebra, Secure.bearer as a
route wrapper, and the OAuth2 client flows with S256 PKCE — the stub
AS in the tests CHECKS the S256 relation, so the challenge/verifier
pair is proven, not asserted. 11 tests, hostile side throughout:
tampered payloads, the HS256-against-RSA confusion (defused by the
key deciding the algorithm), fuzzed garbage into every entry point.
Zero dependencies; the JS leg compiles, verifies nothing until
security-node.

Stage 1 (security-mcp) shipped the same day: McpAuth — the RFC 9728
document servable without a token (it is how a stranger learns to
stop being one), the protected MCP route whose 401 carries
resource_metadata, discovery walking 401 -> resource metadata -> AS
metadata with every missing link a named Left, and connect for the
machine grant handing back a bearer-carrying link. The proof is the
loop test: the SAME agent tool call that works on an open server
works on the protected one, with nothing above the link changed.
Discovered is a VALUE the caller sees before any secret travels —
the trust boundary held by making them look. 4 tests.
