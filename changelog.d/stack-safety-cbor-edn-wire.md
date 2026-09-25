## stack-safety-cbor-edn-wire - the wire's CBOR and strict JSON line take any depth; Cbor and Edn confirmed bounded

Stack-safety stage 2c (specs/stack-safety.md). Three real defects, all in
okay-codec's `Wire.scala`, each red first on a 256 KB stack at 20 000
levels (`TestWireDepth`):

- `WireCbor.decode` recursed once per level of the bytes a worker sent,
  and its `catch` takes only `IllegalStateException` — so a deep message
  was a `StackOverflowError` that escaped the decoder and took the thread
  reading the wire. Now one loop over an explicit stack of open
  containers (remaining count, items read so far); a declared count
  larger than the bytes left is refused as damage before any builder
  grows.
- `WireCbor.encode` recursed per level of the tree: a preorder on an
  explicit stack now, byte for byte the same output.
- `WireJson.whole`'s `damaged` walked the REPAIRED tree of a damaged line
  recursively, after a parse that already takes any depth — a worklist
  now, so a deep damaged line is refused as "not whole JSON" instead of
  crashing.

`Cbor` and `Edn` were already a direct call per container below
`Codecs.NativeThreshold` and the `Cont` trampoline past it: their ten
inventory rows are marked BOUNDED, and `TestCborEdnDepth` runs encode and
decode of both, 20 000 deep, on the same small stack. The three Wire rows
are gone from specs/stack-safety-okay.tsv. okay-py's and okay-r's wire
suites (TestWireGivens, TestRWire) are unchanged and green.
