## foreign-one-bulk — the table call in Go, Rust and Haskell; held values and C Data deferred (2026-09-26)

Stage 6 of specs/foreign-one.md, narrowed by Decision 18. The Go, Rust and
Haskell libraries read the wire's own columnar frame and claim it in their
hello (`"frames": ["columnar"]`): Go `okay.Frame`, Rust `Value::Table`,
Haskell `VTable`, each with `col(name)`, and a function answering one
answers a table. Haskell gains `call` without a second registry: a call is a
named program that answers without performing, and one that performs is
refused by name. The ONE conformance body gained its table case (`scale`,
a frame in, a frame out), answered by every wire row — Python, TypeScript,
R, Go (pipes, TCP, auth, TLS, wasm), Rust (pipes, TCP, auth, TLS, FFM) and
Haskell (pipes, gateway); Rust on wasm has no direct functions and skips it.
Docs: "A table, in every language" and the Table call column in
docs/one-language.md; the "not yet" notes in the facade and cluster pages
now point at foreign-more-languages. foreign-frame-op-rust-hs-go closed.
Filed: foreign-held-values, foreign-arrow-ffm (the `arrow` crate is not in
the offline registry, and no caller moves tables big enough to show the
copy).
