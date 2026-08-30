# P6 — Staged pipelines, Catalyst-style

## Overview
Continue specs/staged-tagless.md and staged-effects.md into the stream
layer, taking the ideas from Spark's Catalyst (the user's direction):
represent a pipeline as an optimizable operator tree, rewrite it by
rules, then compile the whole thing into one tight loop (whole-stage
codegen). Program-as-value is our native ground — the reification the
optimizer needs is what the library already is.

## Design
- **Reify**: a Chunks/Stage pipeline as an operator ALGEBRA (an
  initial encoding: Map, Filter, Take, Drop, Zip, Rechunk, FoldOp,
  Source nodes) — built either directly or captured from the existing
  combinator calls.
- **Rewrite rules** (each a small, testable tree transform):
  map/map and filter/filter fusion, filter-before-map pushdown where
  the predicate allows, take/drop pushdown into sources (range knows
  its length), rechunk elimination, aggregator zip-fusion (two folds
  in one pass — P1's zip at the operator level).
- **Whole-stage codegen**: compile a rewritten operator tree into a
  single while-loop over arrays via inline/summonInline first (the
  proven CKS half from staged-tagless), Expr/quotes second where
  inline cannot reach. The target the chunked transformers measured
  16.9us by hand becomes the GENERATED code — at or below the
  Iterator floor.
- **Selective** earns its keep: statically-known branches (the
  operator tree is data at compile time) select code paths without
  runtime dispatch.

## Behavior
- [x] every rewrite rule preserves semantics on generated pipelines
      (ScalaCheck over random operator trees against the naive run)
- [x] fused map/filter compiles TO the hand-written chunked
      transformers (agreement tested; fewer passes by fusion — depth
      5 -> 3 on the sample tree)
- [x] take-pushdown makes take(n) over range structural
      (NumRange(0, 5) out of a million-row range)
- [ ] the staged whole-stage loop measures at or under the Iterator
      floor on the standard pipeline lane

## Out of scope
- cost-based optimization (rule-based only, like early Catalyst);
  adaptive execution
