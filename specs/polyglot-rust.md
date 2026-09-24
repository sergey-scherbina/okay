# polyglot-rust — Rust kernels for okay

## Overview

Rust's value to okay is COMPUTE: kernels such as parsing, crypto, SIMD
and compression, written once and fast. It is not a host for okay
programs, because a Rust `async` future is not a continuation okay could
resume from outside. So a Rust kernel enters okay as an EFFECT whose
operations are the kernel's calls. It can be mocked in a test,
substituted by a JVM implementation, and later journalled, like okay-py's
`PyEval`.

A kernel crosses the C ABI: one `extern "C"` function over plain pointers
and lengths, with an integer answer, and every buffer owned by the
caller. Three roads call it, cheapest first:

1. **The JVM, through FFM** (JEP 454, final in JDK 22): a `cdylib` loaded
   by `SymbolLookup` and called by a downcall handle. There is no JNI
   glue and no generated code. The module's floor is 22 (`jdkFloor(22)`,
   specs/jdk-compatibility.md).
2. **Scala Native, through `@extern`**: the `staticlib` linked in. This
   is an ordinary C call with no runtime between.
3. **WebAssembly under Chicory**: no native code in the process at all,
   and memory sandboxed. This is the road for UNTRUSTED plugins (and
   for Go through TinyGo).

The first kernel is a real one, as the backlog asked: Argon2id, which
okay-security-argon2 already does on the JVM with BouncyCastle. That
gives a LAW to hold the binding to: the same inputs give the same bytes.

## Stage 1 — Argon2id on the JVM through FFM

- [x] `okay-rust/kernels/argon2` is a Cargo crate, `cdylib` and
      `staticlib`, with the RustCrypto `argon2` 0.5.3 crate as its only
      dependency. `Cargo.lock` is checked in, so a build is reproducible
      and works offline.
- [x] `okay.rust.NativeLib.load(path)` binds a library through FFM. A
      symbol it does not export is refused by name, not a crash.
- [x] `enum PasswordHash[+A] derives okay.Effect` has one operation,
      `Argon2id(password, salt, memoryKb, iterations, parallelism, length)`,
      answering `Either[String, Array[Byte]]`.
      - `PasswordHash.rust(lib)` is the handler over the kernel.
      - `PasswordHash.using(f)` is the handler over any function (a test's, or a
        JVM implementation).
- [x] THE LAW (Live: needs cargo): for the same inputs, the Rust kernel's
      bytes are BouncyCastle's bytes, over several parameter sets and
      salts. Parameters Argon2 refuses are a `Left` naming the kernel's
      code, not an exception.
- [x] A program written against `PasswordHash` runs under either handler
      unchanged.

## Later stages

- Stage 2 — Scala Native: DONE (rust-native, below).
- Stage 3 — Chicory: see below.

## Stage 3 — the same kernel as WebAssembly, under Chicory

No native code in the process at all: the crate compiled to
`wasm32-wasip1` and run by Chicory, a WebAssembly runtime written in
Java. The kernel's memory is its own linear memory, so a bug in it
cannot touch the JVM's heap. This is the road for UNTRUSTED plugins,
and for Go (`GOOS=wasip1`) as well as Rust.

- [x] The crate exports `okay_alloc(n)` and `okay_free(p, n)`. A host
      cannot hand a module its own pointers, so buffers are the module's
      memory, filled and read by the host. The native road ignores both.
- [x] `okay.rust.WasmLib.load(bytes)` instantiates a module under Chicory
      with a WASI that grants NOTHING: no files, no environment, no
      arguments. It gives:
      - `call(name, args*)` on an export;
      - `bytesIn(a)` and `bytesOut(p, n)` over the module's memory, both
        freed in a `finally`.
- [x] `PasswordHash.wasm(lib)` is the third handler of the same effect. The
      program is unchanged.
- [x] (Live: cargo + the wasm32-wasip1 target) THE LAW again: the wasm
      kernel's bytes are BouncyCastle's, over the same 48 cases. A
      refused parameter set is the same `Left` as the native road's.

## Decisions

- **A kernel is an effect, not a function.** Called directly, a native
  kernel is a dependency every caller carries. As an effect it can be
  mocked, swapped for the JVM one on a platform without the library, and
  measured. This is the same reasoning as okay-py's `PyEval`.
- **The caller owns every buffer.** Nothing is allocated in Rust and
  freed in Java, so there is no pair of allocators to keep matched and no
  leak on an early return.

## Results

- Stage 1 (polyglot-rust, 2026-09-23).
  - okay-rust: the crate builds offline in about 1.3 s (argon2 0.5.3 and
    14 locked packages, all in the local cargo cache).
  - `NativeLib` and `PasswordHash` compile on JDK 25 with a floor of 22. The tests
    run on 26 with native access enabled, and no restricted-method
    warning appears.
  - TestPasswordHash (default gate) runs a program under a stand-in handler and
    under BouncyCastle. TestPasswordHashRust (Live, cargo) covers:
    - THE LAW, byte-equal to BouncyCastle over 48 cases;
    - one program with either handler giving the same answer;
    - a refused parameter set coming back as a `Left` naming code -2;
    - a missing symbol refused by name.
  - Mutant: the kernel on Argon2 version 0x10 fails the law and the
    either-handler test.
  - A trap on the way: a Scala 3 enum case's `apply` answers the ENUM
    type (`PasswordHash[...]`), so a vector of `PasswordHash.Argon2id(...)` could not read
    `.password`. `new PasswordHash.Argon2id(...)` keeps the case's own type.

- Stage 3 (rust-wasm, 2026-09-24).
  - The crate builds offline for `wasm32-wasip1` (a 70 KB module). It
    imports four WASI functions (`fd_write`, `environ_sizes_get`,
    `environ_get`, `proc_exit`), which Chicory 1.7.5's WASI answers with
    an empty world.
  - TestPasswordHashWasm (Live): THE LAW over the 48 cases (4.5 s, against 1.25 s
    for the native road with its build), one program under the third
    handler, the same `Left` for refused parameters, and a missing
    export refused by name.
  - Mutant: reading the output one byte off fails the law and the
    either-handler test.
  - A trap: `export` is a Scala 3 keyword, so Chicory's
    `Instance.export` is called as ``instance.`export`(name)``.

- Stage 2 (rust-native, 2026-09-24).
  - okay-rust is `crossProject(JVMPlatform, NativePlatform)`. The effect,
    `PasswordHash.using` and `argon2id` are shared, and `object PasswordHash extends
    PasswordHashPlatform`, whose trait is each platform's: FFM and Chicory on the
    JVM, `@extern` on Native.
  - The Native build links the staticlib given by `OKAY_RUST_ARGON2_LIB`,
    as a full path, because on macOS `-l` picks the dylib. rustc's
    `native-static-libs` are `-lSystem -lc -lm`, which clang links
    anyway. okayRust.native is not in the root aggregate, since the
    default gate has no cargo.
  - `PasswordHashGoldenSuite` pins four vectors. TestPasswordHashGoldenJvm (default gate)
    holds them to BouncyCastle, and TestPasswordHashGoldenNative
    (`scripts/rust-native-check.sh`) holds the linked kernel to them.
    GREEN on the first run.
  - Mutant: one extra iteration on Native turns the check RED.

- Renamed (kdf-to-password-hash, operator, 2026-09-24): the effect `Kdf`
  is `PasswordHash`. It is named for what a program asks, not for how it
  is computed, and "KDF" read as opaque to someone who does not know the
  term. `PasswordHash.rust`, `.wasm`, `.native` and `.using` are the
  handlers, as before.
  The same lane found that rust-native had broken the JVM Live tests' path
  to the crates: a forked test starts in `okay-rust/.jvm` now. The gate it
  ran did not include them. `Kernels.dir` finds `kernels/` from either
  place.
