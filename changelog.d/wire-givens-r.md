## wire-givens-r - R on the wire givens, and the codecs in okay-codec

The operator asked for format, compression and the rest "для всех языков";
R was the language left out of stage 5a, because okay-r has its own engine.

- okay-codec (JVM): `WireFormat`, `WireCompression`, `WireCbor`,
  `WireJson`, `WireFrames` and `WireNegotiation` moved out of okay-py, so
  `ForeignWorker` and okay-r's `RSubprocess` share one handshake. `okay.py`
  re-exports the names.
- A third compression, zlib (RFC 1950). R can check it natively
  (`memCompress`/`memDecompress`). Raw DEFLATE has no safe road in base R,
  as measured in r-base 4.4.1: `gzcon` accepts a cut stream, and
  `memDecompress` on a hand-wrapped member was OOM-killed. The default
  compression is now an order: deflate, zlib, none. `Zlib.given` is
  strict.
- okay-r: `RSubprocess.start(...)(using WireFormat, WireCompression)`
  reads bytes, negotiates, and speaks frames after a configure. A
  timeout's respawn negotiates again. `RSubprocess.wire` names the
  result.
- shim.R speaks CBOR (base R, encoded from jsonlite's own tree so both
  formats carry the same values, numbers in one `writeBin`) and zlib,
  on a binary stdin and `/dev/stdout`.
- FIXED: doubles left R rounded to 15 significant digits
  (`digits = NA`); `sqrt(2)` arrived as 1.4142135623731. They now leave
  with `digits = I(17)`. The new suite found it on the old JSON wire.
- Tests: `RWireConformance` over four wires (6 cases each) plus the
  Deflate refusal; all 75 of okay-r's live tests pass. `TestWireGivens`
  adds zlib and the order. Mutant: little-endian doubles failed only the
  CBOR suites.
- Docs: one-language.md (R's row in the table, zlib, and the three ways
  to reach a worker, with `connect` quoted as the one line a user
  writes rather than the test's tuple), python-and-r.md. Spec:
  polyglot-one-wire.md.
