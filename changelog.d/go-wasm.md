## go-wasm - a Go plugin as WebAssembly inside the JVM

polyglot-go stage 2.

- `okay-rust/kernels/sha256-go` is SHA-256 from Go's standard library,
  compiled with `GOOS=wasip1 GOARCH=wasm -buildmode=c-shared` (no TinyGo)
  and run by Chicory. There is no Go runtime beside the JVM's.
- `Digest` is an effect with two handlers: `Digest.jdk` and
  `Digest.wasm(lib)`.
- THE LAW: the same digests as the JDK's over 134 input sizes.
- `WasmLib` now calls a reactor module's `_initialize` once at load.
  Chicory does not, and Go's runtime trapped without it.
- `WasmLib` captures the module's stderr, so a trap's `Left` carries the
  plugin's own reason, such as a Go panic's message, instead of a bare
  "unreachable".

Docs: "A Go plugin as WebAssembly, inside the JVM" in docs/go.md;
docs/rust.md; docs/modules/okay-rust.md.
