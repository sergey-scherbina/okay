- [ ] polyglot-rust — feasible and USEFUL, but as COMPUTE, not as a
      host for okay programs. Rust's value is kernels (parsing, crypto,
      SIMD, compression); its `async` futures are not continuations
      okay could resume from outside. Three roads, cheapest first:
      (1) Scala Native links a Rust `staticlib` through `@extern`
      directly — okay is already cross-built for Native, so a Rust
      kernel there is an ordinary C call with no runtime between;
      (2) on the JVM, the FFM API (JEP 454, final in JDK 22) calls a
      `cdylib` over the C ABI without JNI glue — a module with
      `jdkFloor(22)`, the floor rule of specs/jdk-compatibility.md;
      (3) Rust compiled to WebAssembly and run by Chicory (a pure-JVM
      Wasm runtime): no native code in the process at all, memory
      sandboxed — the road for UNTRUSTED plugins. Each exposed as a
      handler whose operations are the kernel's calls, so it is
      mockable and journalable like okay-py's. Pick a real kernel first
      (okay-crypto's Argon2 is the obvious candidate); no speculative
      binding generator.
