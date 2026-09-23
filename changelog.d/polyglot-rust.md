## polyglot-rust - Rust kernels as okay effects (stage 1: Argon2id through FFM)

- New module okay-rust.
  - `kernels/argon2` is a Cargo crate exporting `okay_argon2id` over the
    C ABI, with `Cargo.lock` checked in so it builds offline.
  - `NativeLib` binds a library through FFM (JDK 22+, no JNI).
  - `Kdf` is the effect, with two handlers: `Kdf.rust(lib)` over the
    kernel, and `Kdf.using(f)` over any function.
- THE LAW: the Rust kernel's bytes equal BouncyCastle's (okay-security's
  Argon2id) over 48 cases. A mutant kernel on Argon2 version 0x10 is
  caught.
- Refused parameters are a `Left` naming the kernel's code, and a
  missing symbol is refused by name.

Docs: docs/rust.md, with Go's roads and why not in-process, and
docs/modules/okay-rust.md.
