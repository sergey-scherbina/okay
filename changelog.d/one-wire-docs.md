## one-wire-docs - "Rust and Go as okay", one page

Stage 4 of specs/polyglot-one-wire.md. `docs/one-language.md` covers:
- the table of links (pipes, TCP, FFM, WebAssembly) for Rust and Go;
- the same direct-style `quote` in both languages, with its operations
  generated from the Scala callbacks;
- the same Scala over every link;
- what the one conformance suite checks, why in-process uses the
  dialogue and not an upcall, and the limits.

The spec records the next stage, which the operator asked for: the
encoding (JSON or CBOR), compression, encryption and authorization,
chosen by `given`s, each backed by its own mechanism, for every language on
the wire, not only Rust and Go.
