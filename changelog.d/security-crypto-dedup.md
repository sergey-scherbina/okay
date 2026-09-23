## security-crypto-dedup - okay-security's Crypto extends okay-crypto's

The four crypto primitives (hmacSha256, sha256, pbkdf2, randomBytes)
were implemented twice per platform: okay-crypto's givens (for okay-pg)
and okay-security's own, left behind when security-crypto-split made
okay-crypto on 2026-09-01. Now `okay.security.Crypto extends
okay.crypto.Crypto`, okay-security depends on okay-crypto, and the
security givens (JCA and node:crypto) delegate the four to okay-crypto's
— named `okay.crypto.platform` so the delegation is explicit. A security
`Crypto` serves wherever the primitive seam is asked.

The copies had drifted: only okay-security's cleared the PBKDF2 password
spec after use. The surviving implementation does. New shared
`TestCryptoSeam` pins the NIST/RFC vectors on the JVM and on Node — the
node:crypto leg had no vector test before, only the live SCRAM battery.
Docs: okay-crypto and okay-security module pages, specs/security.md.
