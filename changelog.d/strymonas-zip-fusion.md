## strymonas-zip-fusion - `Gen.zip`, without materializing either side

Kiselyov, Biboudis, Palladinos & Smaragdakis, "Stream fusion, to
completeness" (POPL 2017) name `zip` as fold-based fusion's hard
case — both sources must advance in lockstep, which a fold (one
source to completion) cannot express. `Gen` had `zipWithIndex` but
no `zip`.

- `Gen.zip`/`Gen.zipWith` (src/main/scala/Gen.scala): a NEW
  program-level walk (`pull` — one step of a program, the value and
  the rest; `zipping` — pull both sides, pair, stop at the first end),
  not another `Xf` stage, since `Xf`/`FoldUntil` fundamentally folds
  one source. Composes with the ordinary `Xf` chain afterward
  (`.map`, `.filter`, another `.zip`).
- THE HARD CASE: zipping a `flatMap`-fused generator (several
  elements per outer step) needed no special case at all — `pull`
  calls `.resume` once, which already normalizes through however many
  `Delay`/`Bind` nodes `flatMap`'s own `splice` walk built, exactly as
  `readState`'s loop and `Stepper.advance` already rely on. Proven
  against an INFINITE `flatMap`-fused source zipped down to 5
  elements — full materialization was not merely undesired, it was
  impossible, so the test terminating IS the proof.
- TestGenZip (10 tests): correctness (equal/unequal lengths, empty
  sides, composing after zip, `++` on one side), the hard case both
  orders, and its laziness — a finite witness (2 outer steps for 3
  zipped pairs, VERIFIED by running rather than assumed) and the
  infinite one. docs/guide.md's pinned example; specs/strymonas-zip-
  fusion.md carries the rest, including what stays out of scope
  (`zip3`, a non-lockstep `merge`).
