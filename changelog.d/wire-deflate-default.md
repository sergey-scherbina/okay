## wire-deflate-default - DEFLATE on by default, as a preference

The operator: "DEFLATE пусть будет не обязательным но желательным (по
умолчанию) - его при желании можно только отключить".

- okay-py `WireCompression`: the default given is now `preferred`, which
  is raw DEFLATE with a `fallback` to no compression. The fallback is
  taken where the far side's hello does not announce deflate (Haskell,
  R, older workers: no refusal) and on in-process links
  (`WireLink.inProcess`, set by okay-rust's FFM and wasm links), where
  a message is a memory copy. `import WireCompression.Off.given` turns
  compression off. `import WireCompression.Deflate.given` stays the
  STRICT choice: refused by name where it is not spoken, and applied
  in-process too.
- `ForeignWorker.wire` names what the handshake settled on
  (`json/deflate`, `json/none`, `cbor/deflate`, ...).
- Tests: six fake-link cases in `TestWireGivens` (default gate), plus
  live checks in `TestPyPipes` (json/deflate), `TestPyPipesOff`,
  `TestHsPipes` (json/none, unrefused) and `TestRustFfm` (in-process,
  uncompressed). The default conformance suites of Python, TypeScript,
  Go and Rust now all run compressed. The mutant (no in-process branch)
  failed exactly its case.
- Docs: docs/one-language.md, "The wire's encoding, chosen by a given",
  rewritten around the new default. Spec: polyglot-one-wire.md.
