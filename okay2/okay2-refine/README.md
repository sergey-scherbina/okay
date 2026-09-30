# okay2-refine

okay-refine for the Scala 2.13 core: a typed hierarchy of patterns
(prisms whose read may decline) that recognise a document level by level
and say why. `Refine[A, B]`, `Verdict` (`Took` / `Unclear` / `Declined`
with every `Refusal`), `Path`; `Format.detect` over okay2-codec's own
lossless trees — JSON and XML today, YAML and CBOR the day the codec has
them (one more `<|>` each, nothing here edited); `Format.value` into the
one `Json` a Schema pattern reads; `Refine.schema`, `Refine.json.*`; the algebra — `>>>`, `or`, `orElse`,
`Refine.id` + `Category`, `Refine.empty`, `***` / `+++` / `and` with
`Refine.Merge`, `orRaise` (Throws), `verdicts` / `taken` (Stages); routing — `Router`
(`route[X]`, `route { case … }`, `byName`, `tap`, `otherwise`, `run`),
`decide`, `Refine.routed`.

**Depends on:** `okay2` (core), `okay2-codec`, `okay2-optics`, `okay2-stream`. Pure
Scala — cross-built for JVM, JS and Native.

The one difference from okay-refine: dispatch is the trait's own
methods (`runAt`, `writeBack`) rather than a match over the tree —
Scala 2 does not refine a generic case class's existential across a
match the way Scala 3's GADT check does. Same shape, same verdicts, the
same tests (`TestRefine`, `TestFormat`, `TestSchemaPattern`).

| | |
|---|---|
| [`docs/okay2.md`](../../docs/okay2.md) §31 | the guide |
| [`docs/modules/okay-refine.md`](../../docs/modules/okay-refine.md) | what it is, and the reasoning (the Scala 3 module's page) |
| [`specs/refine.md`](../../specs/refine.md) | the spec: stages, decisions, open questions |
