## in-process-oneliner - ForeignWorker.inProcess(library)

The operator: "A one-line in-process call: ForeignWorker.inProcess(dylib) -
вот это хочу да".

- okay-rust: `ForeignWorker.inProcess(library)` (a Rust `cdylib` through
  FFM) and `ForeignWorker.inProcessWasm(module)` (Rust or Go as
  WebAssembly, under Chicory), extensions in `okay.rust`. They take the
  wire's givens like every other constructor. A library without
  `okay_exchange` is refused by name and closed.
- The in-process suites (`TestRustFfm*`, `TestRustWasm*`, `TestGoWasm*`)
  open their engines this way; `TestRustFfm` refuses the Argon2 kernel,
  a cdylib that is not a worker.
- Docs: one-language.md (the ways to reach a worker), rust.md, go.md.
