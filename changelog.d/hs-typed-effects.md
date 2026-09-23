## hs-typed-effects - Haskell programs typed by their effects

- The jar ships `OkayEff` beside `Okay.hs`. A Haskell program is
  `Eff '[Shop] a`, and `send (PriceOf sku)` performs a typed operation.
- An effect is a GADT of its operations, as in freer-simple and
  polysemy. `Hs.ops(name, callbacks)` writes that GADT and its wire
  instance from the Scala callbacks' Schemas, so the row is written once,
  in Scala.
- GHC refuses an undeclared effect with "this program does not declare
  the effect Shop", and refuses a wrong argument type.
- Underneath it is the same `Prog`: the wire, multi-shot continuations
  and `Durable` are unchanged, and untyped programs still work.

Checked live with GHC 9.14, and a mutant is caught.

Docs: "Haskell programs typed by their effects" in docs/python-and-r.md;
the Haskell row of "Where each language names okay's effects" in
docs/jvm-languages.md.
