## wire-choice-by-name - the wire codec picked by name, not only by given import

`WireFormat`, `WireCompression` and `FrameFormat` (okay-codec's Wire.scala)
stay given-based for the compile-time default, but each now has a
`byName(String): Either[String, _]` beside its givens, and a new
`WireChoice` case class (`format`, `compression`, `frames`, `deadline`)
combines all three into one explicit, runtime-constructible value —
`WireChoice.named(format = "cbor", compression = "deflate", frames =
"arrow")`, `Left` naming an unknown name rather than guessing.

- `ForeignWorker.startWithWire(wire, python, env, modules)` (okay-py) and
  `RSubprocess.startWithWire(wire, rscript, env, timeoutMillis, require,
  modules)` (okay-r) take a `WireChoice` explicitly, next to the existing
  `start` that reads the wire from givens in scope. Same handshake, same
  refusals; only how the choice is made differs. R has no Arrow side yet
  (`r-arrow`), so its `startWithWire` reads only `wire.format` and
  `wire.compression`.
- Asked for so a caller can pick the wire from a string — a flag or a
  config file — instead of writing a Scala `import ...given` at the call
  site, after `arrow-vs-cbor` and `okay-arrow` made the choice of format
  (json/cbor/arrow) something worth exposing as data, not only as code.
- Tests: `TestWireGivens` (pure, default gate) covers `byName` for all
  three types and `WireChoice.named`/`default`; `TestPyPipesWireChoice`
  and `TestRWireChoice` (Live) start a real worker with `startWithWire`
  and check `.wire` settles where `WireChoice.named` said it would.
