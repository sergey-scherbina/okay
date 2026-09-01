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

| `McpAuth` | stage 1: the RFC 9728 document, the protected MCP route (401 with resource_metadata), discovery (401 -> resource metadata -> AS metadata, missing links as named Lefts), and `connect` — the machine grant onto a bearer-carrying `McpLink`; the loop test proves the same agent call works protected and open |

The JS leg verifies (security-node): the same shared code under
Node via node:crypto — HS256 JWTs, passwords, API keys, PKCE pinned
to the RFC 7636 vector. RS256 stays JVM: platform key types are an
opaque `Crypto.Handle`, built only where material exists.

Staged next (BACKLOG): security-oidc, ES256 and an argon2 satellite.

Not an authorization SERVER: this module verifies and obtains tokens;
it does not issue codes to third parties. The stub AS in its tests is
a test.
