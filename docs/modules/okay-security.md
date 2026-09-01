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

| `Oidc` | user login: discovery, the code+PKCE url with nonce, callback validating the id_token (signature via JWKS, issuer, audience, nonce, at_hash) into a Principal — every forgery refused by name |

Staged next (BACKLOG): ES256 and an argon2 satellite.

Not an authorization SERVER: this module verifies and obtains tokens;
it does not issue codes to third parties. The stub AS in its tests is
a test.

## Since stage 1

| | |
|---|---|
| `Es256` | ES256 joins HS256/RS256: the JOSE raw R||S ↔ DER dance as its own PURE object, total both ways, its battery on JS too; the key still decides the algorithm with three kinds in the ring |
| `Oidc` | user login from parts already on the shelf: discovery, the login url (nonce), callback validating the id_token into a Principal — issuer/audience/nonce/at_hash on top of Jwt.verify |
| `Secure.granted` | the capability form of the route wrapper: the principal AMBIENT in the handler (`Principal ?=> route`), the 401/403 ladder byte-identical to bearer's; composes with `Traced.route` — one stored `(Principal, Tracer) ?=> Route` is protected AND traced |
| argon2 | the satellite with a real KDF: [okay-security-argon2](okay-security-argon2.md) |
