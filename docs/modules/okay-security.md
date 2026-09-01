# okay-security

Authorization for services, once (specs/security.md): identities,
claims and decisions as VALUES; crypto as a platform seam (JCA on the
JVM); protection as a route wrapper; refusal as a value with a
reason. Zero dependencies — the JDK carries the primitives.

| | |
|---|---|
| `Principal` / `Claims` / `Verified` | who, what was asserted, whether the proof held — `No(reason)` never throws |
| `Jwt` | HS256/RS256 over the `Crypto` seam; keys by kid; skew; the alg/key confusion defused by the KEY deciding the algorithm |
| `Jwks` | an issuer's key set into verifying keys (RSA); damaged entries skipped |
| `Password` | PBKDF2 with the stored form carrying its parameters — iterations rise without a flag day; constant-time |
| `ApiKey` | hand out the key, store the digest: a leaked table cannot be presented |
| `Policy` | `(principal, action, resource) => Permit / Deny(why)`; `scoped`, `role`, `allOf`, `anyOf` — an algebra, not a rules engine |
| `Secure.bearer` | the route wrapper: the protected route is `Principal => PartialFunction`, so it CANNOT be reached without a principal — the type system holds the door; 401/403 with WWW-Authenticate |
| `OAuth2` | the client flows over `trait Http`: code+PKCE (S256), refresh, client credentials; a token-endpoint error is a Left |

Staged next (BACKLOG): security-mcp (MCP authorization: RFC 9728
metadata, 401 challenges, discovery -> PKCE -> bearer retry on the
link), security-node (the seam over node:crypto), security-oidc,
ES256 and an argon2 satellite.

Not an authorization SERVER: this module verifies and obtains tokens;
it does not issue codes to third parties. The stub AS in its tests is
a test.
