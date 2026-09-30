## okay2-refine - okay-refine ported to the Scala 2.13 core

`okay2/okay2-refine`: `Refine[A, B]` (step / andThen / `<|>` / map /
widen / search), `Verdict` with `Path` and `Refusal`, `Format.detect`
over okay2-codec's JSON and XML trees (strict XML; YAML and CBOR when the
codec has them), `Format.value`, `Refine.schema`, `Refine.json.*`. The
three suites ported (21 tests, JVM / JS / Native), a §31 in
docs/okay2.md, the spec's Results. Dispatch is per-case methods rather
than a match over the tree — Scala 2's existential does not connect
across `case AndThen(f, s)`.
