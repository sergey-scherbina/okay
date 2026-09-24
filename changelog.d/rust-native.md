## rust-native - the Rust kernel on Scala Native

polyglot-rust stage 2.

- okay-rust is a JVM and Scala Native cross project. The `Kdf` effect is
  shared, and each platform supplies its handlers: `Kdf.rust` (FFM) and
  `Kdf.wasm` (Chicory) on the JVM, and `Kdf.native` on Native, the
  argon2 `staticlib` linked in and called through `@extern`.
- The law crosses platforms through pinned vectors. BouncyCastle holds
  them on the JVM, in the default gate, and the Native suite holds the
  linked kernel to them.
- `scripts/rust-native-check.sh` builds the staticlib offline, links it
  by full path, and runs the Native suite. A mutant is caught.

Docs: "The same kernel on Scala Native" in docs/rust.md.
