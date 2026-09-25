- [ ] compress-crypto-facades — the existing implementations beside ours,
      transparently on choice, the okay-arrow shape (operator, 2026-09-25:
      "кроме своего сжатия еще использовать существующее — то с которым
      мы сравнивали — по тому же принципу как и с арров — прозрачно на
      выбор. Тоже самое касается и алгоритмов шифрования"). Two facades:
      `okay.compress.Compression` (lz4, zstd) — ours the default given,
      `Aircompressor.given` on the JVM behind an OPTIONAL dependency,
      refused by name without it; `okay.crypto.Keccak` — ours the default,
      `BouncyCastleKeccak.given` optional. `byName` on both, as
      `WireChoice`. Consumers made transparent: okay-arrow's compressed
      bodies (read side picks by the given), okay-cluster's
      `RemoteCompression` givens, okay-x402-evm's keccak. The rest of the
      crypto (SHA-256, HMAC, PBKDF2, Argon2, TLS) is ALREADY the platform's
      or BouncyCastle's by specs/tls.md's rule — nothing of ours to choose
      against; said in the spec so nobody looks.
