## rust-wasm - the Rust kernel as WebAssembly, under Chicory

polyglot-rust stage 3.

- The argon2 crate builds offline for `wasm32-wasip1`, a 70 KB module, and
  now exports `okay_alloc`/`okay_free`.
- `okay.rust.WasmLib` runs it under Chicory 1.7.5, a WebAssembly runtime
  in pure Java, with a WASI that grants nothing: no files, no
  environment, no arguments. There is no native code in the process.
- `Kdf.wasm(lib)` is the effect's third handler, and the program is
  unchanged.

THE LAW holds under Chicory as well: the same bytes as BouncyCastle over
48 cases. A mutant that reads the output one byte off is caught.

Docs: "The same kernel as WebAssembly, under Chicory" in docs/rust.md.
