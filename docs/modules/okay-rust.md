# okay-rust

Rust kernels for okay (specs/polyglot-rust.md). A Rust crate over the C
ABI, bound through FFM (JDK 22+) and offered as an okay EFFECT whose
operations are the kernel's calls.

| | |
|---|---|
| `kernels/argon2` | the Cargo crate: `okay_argon2id` over the C ABI, `cdylib` + `staticlib`, `Cargo.lock` checked in (builds offline) |
| `NativeLib.load(path)` / `.function(name, descriptor)` | a native library through FFM; a missing symbol refused by name |
| `PasswordHash` / `PasswordHash.argon2id(...)` | the effect: Argon2id, answering `Either[String, Array[Byte]]` |
| `PasswordHash.rust(lib)` | the handler over the Rust kernel |
| `PasswordHash.using(f)` | the handler over any function (BouncyCastle, a test) |
| `PasswordHash.native` (Scala Native) | the argon2 `staticlib` linked in, through `@extern`; checked by `scripts/rust-native-check.sh` against vectors BouncyCastle pins on the JVM |
| `Digest` / `Digest.jdk` / `Digest.wasm(lib)` | SHA-256 as an effect: the JDK's, or a Go plugin (`kernels/sha256-go`) compiled to `wasip1` and run by Chicory |
| `WasmLib.load(bytes)` / `PasswordHash.wasm(lib)` | the same kernel compiled to `wasm32-wasip1`, run by Chicory (pure Java) under a WASI that grants nothing; the third handler |

Byte-equal to okay-security-argon2's BouncyCastle Argon2id over 48 cases.
The guide: [okay with Rust](../rust.md).
