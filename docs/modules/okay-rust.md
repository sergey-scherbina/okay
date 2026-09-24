# okay-rust

Rust kernels for okay (specs/polyglot-rust.md). A Rust crate over the C
ABI, bound through FFM (JDK 22+) and offered as an okay EFFECT whose
operations are the kernel's calls.

| | |
|---|---|
| `kernels/argon2` | the Cargo crate: `okay_argon2id` over the C ABI, `cdylib` + `staticlib`, `Cargo.lock` checked in (builds offline) |
| `NativeLib.load(path)` / `.function(name, descriptor)` | a native library through FFM; a missing symbol refused by name |
| `Kdf` / `Kdf.argon2id(...)` | the effect: Argon2id, answering `Either[String, Array[Byte]]` |
| `Kdf.rust(lib)` | the handler over the Rust kernel |
| `Kdf.using(f)` | the handler over any function (BouncyCastle, a test) |
| `WasmLib.load(bytes)` / `Kdf.wasm(lib)` | the same kernel compiled to `wasm32-wasip1`, run by Chicory (pure Java) under a WASI that grants nothing; the third handler |

Byte-equal to okay-security-argon2's BouncyCastle Argon2id over 48 cases.
The guide: [okay with Rust](../rust.md).
