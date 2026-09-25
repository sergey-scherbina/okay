## keccak-pure - one Keccak-256: okay.crypto, pure Scala

Ethereum's hash was here twice: okay-x402-evm over BouncyCastle, and
okay-watch's pure-Scala port (its EIP-55 check). One implementation now.

- `okay.crypto.Keccak256.hash` — pure Scala, JVM and JS, no dependency; the
  original Keccak, not SHA3-256 (one padding byte apart). TestKeccak256: the
  canonical vectors, every length across the 136-byte rate, not SHA3.
- okay-x402-evm's `Evm.keccak` is it (BouncyCastle stays there for secp256k1);
  TestKeccakAgreesWithBouncyCastle: byte for byte on lengths 0–600 and 500
  random inputs.
- Gate: `affected master` 7512, GREEN after the native rerun (okayKernelNative
  lost its process; recorded in native-runner-error).

Landed as 600553374.
