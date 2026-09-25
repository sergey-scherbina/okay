# okay-crypto

The primitive crypto seam (security-crypto-split): the four
operations SCRAM and password hashing need, as a per-platform given
that drags NO dependency. It rests on the platform's own crypto —
JCA on the JVM, `node:crypto` on JS — and on nothing else, so a
module that must not cycle back through the security stack
(okay-pg's SCRAM authentication) stands on a shared seam instead of
a private copy of the same four functions.

| | |
|---|---|
| `Crypto` | the whole trait: `hmacSha256`, `sha256`, `pbkdf2`, `randomBytes` — a keyed MAC, a hash, a KDF, randomness |
| `given Crypto` | one per platform, from `src/main/scala-jvm` and `src/main/scala-js`; a caller summons it and never names an implementation |
| `Keccak256.hash` | Ethereum's Keccak-256 — NOT SHA3-256 (one padding byte apart): pure Scala, JVM and JS, the one implementation okay-x402-evm and okay-watch use (keccak-pure) |

Deliberately the SMALL surface. The fuller crypto — RSA/ECDSA
signing, JWT key handles, JWKS — stays in
[`okay-security`](okay-security.md), which owns those concerns and
their heavier dependencies: a caller that needs only primitives
depends here, a caller that needs signing depends there. Platform
primitives, never our own (the specs/tls.md rule).

Cross-built JVM + JS (no Native: it has no platform crypto to rest
on). Consumers: [`okay-pg`](okay-pg.md) (SCRAM) and
[`okay-security`](okay-security.md), whose wider `Crypto` EXTENDS
this one and delegates the four primitives to its given — named
`okay.crypto.platform` for that — so there is one implementation per
platform (security-crypto-dedup, 2026-09-23; until then okay-security
kept a second copy of all four). A security `Crypto` therefore serves
wherever this one is asked.
