## wire-format-givens - the wire's encoding and compression, chosen by an import

Stage 5a of specs/polyglot-one-wire.md, asked by the operator: "транспорт
по трубе в формате cbor вместо json ... через имплисииты", "для всех языков
а не только для раста или гоу".

- okay-py: `WireFormat` (json; `import WireFormat.Cbor.given`) and
  `WireCompression` (none; `import WireCompression.Deflate.given`) are
  `using` parameters of `ForeignWorker.over/speaking/connect/start`,
  `PyWorkers`, `PyEnv.start` and `TsWorker.start`. The defaults come from
  the companions, so a program without the imports keeps its JSON lines.
  `WireCbor` is the CBOR subset (RFC 8949). DEFLATE is raw (RFC 1951),
  from java.util.zip. `WireLink.exchange` carries a frame (a 4-byte
  big-endian length) on streams, and the raw bytes in-process
  (`InProcessLinks.ffm/.wasm`).
- The protocol: the far side's hello ANNOUNCES `speaks`. A non-default
  choice sends one JSON `configure`, which the far side answers in the
  old mode before switching. A choice that was not announced is refused
  by name before any request is sent.
- Far sides: Go, Rust, Python's shim, the TypeScript worker (CBOR and
  DEFLATE) and Haskell (CBOR; no DEFLATE, because GHC ships no zlib, and
  its hello says so). Each has its own CBOR codec and no new package,
  except Rust's flate2 on the pure-Rust backend.
- Tests: TestWireGivens (default gate: round trips, cut inputs refused,
  unannounced format refused). `WireConformance` also runs under CBOR
  over Go (pipes, TCP, wasm), Rust (pipes, TCP, FFM, wasm), Python,
  TypeScript and Haskell (pipes), plus the Deflate refusal from Haskell.
- Docs: docs/one-language.md, "The wire's encoding, chosen by a given",
  with a table of which language supports which layer.
- Not done, and filed: R (`wire-givens-r`; okay-r has its own engine) and
  a read deadline (`wire-read-deadline`: the Haskell mutant that
  confirms `configure` without switching was caught only by the gate's
  stall watchdog).
