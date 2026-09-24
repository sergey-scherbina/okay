## wire-support-table - which language supports which layer

The last open box of specs/polyglot-one-wire.md. docs/one-language.md
gains one table: six far sides (Python, TypeScript, Go, Rust, Haskell,
R) against every layer of the wire:
- links, `okay_call`, programs as data;
- CBOR and compression;
- TCP, `WireAuth` and TLS (the worker's own, or the gateway's);
- recovery (supervision and replay).

The suites that hold each row are listed, and the two cells that are
generic rather than tested per language (the supervisor's replay, the
gateway's TLS) say so. The polyglot-one-wire spec has no open box left.
