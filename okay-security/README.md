# okay-security — authorization for services, once

Identities, claims and decisions are VALUES; crypto is a platform
seam; protection is a route wrapper rather than a filter chain; and a
refusal is a value carrying its reason, never an exception. Zero
dependencies — the JDK already has HMAC, RSA, SHA-2, PBKDF2 and
SecureRandom, and the JS leg answers the same shared code through
`node:crypto`.

## The pieces

| | |
|---|---|
| `Principal` / `Claims` / `Verified` | who, what was asserted, and whether the proof held — `No(reason)` rather than a throw |
| `Jwt` / `Jwks` / `Es256` | HS256, RS256 and ES256 over the `Crypto` seam; keys by kid, clock skew, and the alg/key confusion defused by the KEY deciding the algorithm |
| `Password` / `ApiKey` | PBKDF2 whose stored form carries its parameters (iterations rise with no flag day); a key handed out, a digest stored |
| `Policy` | `(principal, action, resource) => Permit \| Deny(why)`, with `scoped`, `role`, `allOf`, `anyOf` — an algebra, not a rules engine |
| `Secure.bearer` / `Secure.granted` | the route wrapper: the protected route takes the `Principal`, so it cannot be reached without one — the type holds the door |
| `OAuth2` / `Oidc` | the client flows: code+PKCE (S256), refresh, client credentials, and a login whose id_token is checked by signature, issuer, audience, nonce and at_hash |
| `Capability` / `Caveat` | macaroon-shaped authority: anyone can narrow it, nobody can widen it, with no issuer and no registry |
| `Revocations` | somebody else's list of what is switched off, as a local snapshot you refresh |
| `McpAuth` | the MCP door: the RFC 9728 metadata, the 401 that teaches, discovery, and per-TOOL authorization — in [okay-mcp-http](../okay-mcp-http) since http-mcp-agent-edge, package unchanged |

## Protecting a route

```scala
import okay.security.*

val verify: String => Verified =
  (t: String) => Jwt.verify(t, _ => Some(Jwt.Key.Hmac(secret)), Some("api"), now)

val route = Secure.bearer(verify, Policy.scoped("read")) { principal =>
  { case r if Server.path(r) == "/api/data" =>
      Server.text(200, s"seen by ${principal.id}") }
}
```

No token is a 401 with `WWW-Authenticate`; a good token without the
scope is a 403 naming what it needed; only a principal that passed
both reaches the handler.

## Narrowing authority without asking anybody

```scala
val grant = Capability.issue(rootKey, "alice")          // the user's own
val agent = grant.attenuate(Caveat.Scope("tool:search")) // one permission
                 .attenuate(Caveat.Until(deadline))      // and not forever

agent.verify(rootKey, Capability.checking(now, Set("tool:search")))  // true
agent.verify(rootKey, Capability.checking(now, Set("tool:delete")))  // false
```

Each caveat is signed with the previous signature as the key, so
adding one needs only the token — which the holder has — while
removing one would need a value that was consumed and never travels.
Narrowing is free and offline; widening is not possible. It is
attenuation and not revocation: see `Revocations` for the other half,
and the type's own comment for which handle a deny-list can catch.

## Further

| | |
|---|---|
| [`docs/modules/okay-security.md`](../docs/modules/okay-security.md) | the pieces, stage by stage |
| [`specs/security.md`](../specs/security.md) | the design, its decisions, and every behaviour with a test behind it |
| [`okay-mcp/`](../okay-mcp) | the MCP server this module's tool gate protects |
