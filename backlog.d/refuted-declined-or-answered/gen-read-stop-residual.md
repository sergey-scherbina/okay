- gen-read-stop-residual — DONE 2026-09-23, REFUTED the recorded
  cause: generators-jmh's "Gen.unfold.toList is +40%/+24 B/elem over
  Writer.foldUntil because of the Stop arm in every split" was wrong,
  and had gone unquestioned through gen-filter-as-walk, gen-chain-
  fusion, gen-flatmap-fusion and typeablek-instanceof — all of which
  touch the exact machinery the claim blamed. `Gen.of(prog)` — the
  SAME `prog`, widened by `Stop` and nothing else, read through the
  same `Gen.foldUntil`/`Chain`/`Xf.Id` — allocates 1 917 025 B, LESS
  than the Stop-free `Writer.foldUntil` floor (2 153 945): widening a
  row by `Stop` costs nothing, and post-typeablek-instanceof the Gen
  road is cheaper, not more expensive. The whole +24 B/elem is
  `Gen.unfold`'s OWN signature, `S => Option[(W, S)]` — an `Option`,
  a `Tuple2` and two boxed `Long`s per step for a scalar state — the
  same shape isolated on the Writer side, no Stop anywhere, pays
  +71 B/elem for the identical reason. NOT WORTH FIXING: `Option[(W,
  S)]` is Scala's own `unfold` convention (`LazyList.unfold`,
  `Iterator.unfold`) and the tax only bites a scalar element/state —
  production `unfold` calls over a case-class state do not pay it,
  and changing the signature to dodge a benchmark's own shape would
  break the idiom users expect. Rows `grsr-*`; docs/benchmarks.md §21
  and specs/generators.md corrected.
