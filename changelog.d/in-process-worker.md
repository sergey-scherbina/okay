## in-process-worker - Rust and Go workers inside the JVM (FFM, WebAssembly)

Stage 3 of specs/polyglot-one-wire.md.

- The same Rust and Go workers run with no second process.
  - Rust: `okay::export_worker!(make)` builds a `cdylib`, loaded
    through FFM, or a `wasm32-wasip1` module.
  - Go: `okay.Export` in `init()`, compiled with `GOOS=wasip1` (no
    TinyGo) and run by Chicory.
- `InProcessLinks.ffm` and `.wasm` are wire links of one call each
  (`okay_exchange`), and the engine is unchanged.
- The conformance suite passes over (Rust, FFM) and (Go, wasm) in full,
  and over (Rust, wasm) for multi-shot and callbacks. wasip1 has no
  threads, and a panic there traps with its message reported.
- Found and fixed:
  - wire lines were read with the repairing JSON parser, so a reply cut
    short passed. They are now checked for balance and parsed strictly,
    on every transport.
  - `WasmLib` freed empty buffers with the wrong size.
  - The Rust crate's resource path shadowed the `okay` package for
    scalac.
- Refuted: `okay_call` as an FFM upcall, because a callback must run
  under the caller's handlers.

Docs: docs/rust.md ("The same worker, in this process"), docs/go.md ("In
this process, as WebAssembly").
