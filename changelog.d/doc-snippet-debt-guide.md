## doc-snippet-debt-guide - docs/guide.md pinned line for line; two of its examples did not compile

docs/guide.md carried 36 lines of example debt across fourteen blocks.
Every one of them now sits verbatim in a test that asserts what the
page claims (debt 1 409 -> 1 359, the rest of it from lanes that
landed meanwhile). Each line went to the suite whose module sees what
the example needs:

- core, `TestDocExamplesGuide` (new): the `Users` signature, both
  `tracked` interpreters (`!.interpret`, and `!.tracing` over `stored`,
  shown to log `Save(1,ada)`, `Find(1)`), `Reader.local`, and `Tag`.
- okay-stream, `TestDocExamplesFoldUntil`: the answer lines of
  Collatz, `runFoldUntil`, `Chunks.foldUntil` and the header parser,
  plus `Staged` (2 997 000).
- okay-direct, `TestDocExamplesGen`: squares and countdown.
- okay-x402, `TestDocExamplesGuideProvide` (new; the one module that
  sees both okay-http's `Http` and okay-conf's `Secrets`): `provide`
  and `providing`, asserted by identity so they also run on JS.
- okay-obs, `TestShowcase`: aligned to the page's lines (`.getBytes`,
  a named `tracer`, `bob`).
- okay-ui, `TestDocExamplesGuideOptics` (new): the `city` optic, the
  Json zipper walk, `State.zoom(Zipper.focus)` and `Zipper.at[Ui]`.

What pinning found:
- `Tag.tag["big", State % Int](bump(10)).at[Small + Big]` did NOT
  compile. A program of `State` alone infers `State` as its rest
  rather than `Pure`, and TestTag had known this for a week. The page
  now spells `[Int, okay.Pure]` and says why.
- The zipper example needs `Plate[Json]`, which is not in implicit
  scope. The page said "`Json` and `Ui` have one"; it now names the
  import, `okay.codec.JsonOptic.plate`.
