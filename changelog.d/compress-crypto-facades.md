## compress-crypto-facades - ours or the standard one, transparently: Compression, Keccak

The operator, 2026-09-25: the existing implementations beside ours, "по
тому же принципу как и с арров — прозрачно на выбор", and the RULE for
every implementation of ours, now and later: specs/own-or-standard.md
and a line in AGENTS.md. The shape is okay-arrow's.

- `okay.compress.Compression` (lz4, zstd): ours the default given on
  every platform; `Aircompressor.given` on the JVM over an OPTIONAL
  io.airlift:aircompressor — ZSTD as its frame codec, LZ4 as our frame
  envelope over its block codec (`Lz4Blocks`, `Lz4FrameCodec`), refused
  by name without the jar, `Compressions.byName`. `TestAircompressor`:
  each reads the other's frames on every sample and pyarrow's fixtures.
- `okay.crypto.Keccak` (hash256): ours the default; `BouncyCastleKeccak.given`
  over an optional bcprov, `Keccaks.byName`. `TestBouncyCastleKeccak`:
  byte for byte on 0–300 and random lengths.
- Transparent consumers: okay-arrow reads a compressed body by the
  `Compression` in scope (`read`/`readFile`/`readFileBatch` take it;
  `TestArrowCompressionChoice` reads ours under theirs and theirs under
  ours, ZSTD and LZ4, streams and files); okay-cluster's
  `RemoteCompression.Lz4/Zstd` givens and `Remote.listen`;
  okay-x402-evm's `Evm.keccak`.
- Not on the list, said in the spec: SHA-256, HMAC, PBKDF2, randomness,
  Argon2, TLS, DEFLATE/zlib — already the platform's or BouncyCastle's by
  specs/tls.md's rule; nothing of ours to choose against. Roads: a JNI
  zstd as a second JVM implementation, Node/Native ones when asked for.
