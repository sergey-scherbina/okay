- [ ] keccak-pure — Ethereum's Keccak-256 twice: okay-x402-evm's over
      BouncyCastle, and okay-watch's own pure-Scala port (its
      `chain/Keccak256`, cross-checked against EIP-55's vectors). One pure
      implementation in okay-crypto (no dependency, JVM and JS),
      canonical vectors in its test; okay-x402-evm's `keccak` on it
      (BouncyCastle stays there for secp256k1); okay-watch drops its copy.
