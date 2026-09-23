- [ ] security-crypto-dedup — the four crypto primitives (hmacSha256,
      sha256, pbkdf2, randomBytes) are implemented TWICE per platform:
      okay-crypto's `CryptoJvm`/`CryptoJs` (used by okay-pg) and
      okay-security's own `CryptoJvm`/`CryptoJs` behind the wider
      `okay.security.Crypto` trait (RSA/ECDSA, JWKS handles).
      security-crypto-split (2026-09-01) created okay-crypto to move
      okay-pg OFF a private copy, and left okay-security's copy in
      place — found 2026-09-23 while correcting okay-crypto's module
      page, which listed okay-security as a consumer (it is not; its
      build.sbt deps are okayHttp + okayData only). Shape of the fix:
      `okay.security.Crypto extends okay.crypto.Crypto`, okay-security
      dependsOn okay-crypto (no cycle: okay-crypto rests on nothing),
      and the security givens delegate the four primitives to
      okay-crypto's — one implementation, pinned by TestCrypto's
      NIST/RFC vectors, and a security `Crypto` then serves wherever
      an `okay.crypto.Crypto` is asked (okay-pg). JVM + JS both.
