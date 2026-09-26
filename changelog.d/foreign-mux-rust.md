## foreign-mux-rust — several requests at once on one Rust worker (2026-09-26)

Part 2 of foreign-mux-duplex. A Rust worker over pipes or TCP now claims
`mux`: a reader thread takes requests off the wire, each running function's
events are forwarded to the worker's one loop, and every request is
answered when its call finishes — the worker itself stays on one thread,
because its programs hold `Rc` continuations. TLS and in-process libraries
are served one exchange at a time as before. WireConformance's MUX case
passes on the five Rust rows, and now fails in seconds rather than at the
suite's timeout when a wire is not multiplexed. docs/one-language.md.
