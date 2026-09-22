## producer-fold-until - `Producer.foldUntil`, and `.foldUntil` on an effectful writer program

The consumer stage 1 of specs/fold-until.md named in its first design
answer and did not ship; the operator asked for it, and then removed
the triggers on stages 3–4 ("все нужно"). `Producer.foldUntil[W, S, R,
A, G](p: A ! Produce + G)(using FoldUntil[W, S, R]): R ! G` in
Generate.scala is `Producer.fold`'s split walk with an early `pure(end
(s))` — the `Bind` arm does not call `k` once the state is done, so a
`G` operation after the satisfying production is never performed;
answers `R` alone, as `Writer.foldUntil` does. `TestFoldUntil` +3:
agreement with the pure road on eight instances, the op-after-the-stop
law counted through a `Writer % String` row, 100 000 productions on
the default stack.

The effectful writer program gained `.foldUntil(using fo)` (Stream.
scala) beside the pure one's, so a `Source` under a `Handler[Async]`
folds with one `using`. It lives in a SEPARATE extension block, for a
reason worth knowing: an explicit `(using fo)` at a call site is
matched against the EXTENSION's using clause when the extension has
one, so on the block carrying `(using TypeableK[G], Handler[G])` the
call did not type — and the new method did not use `TypeableK[G]`,
which the gate refused as E198. A block with no extension-level clause
and `Handler[G]` in the method's own clause after the fold is the
shape. `TestFoldUntilStreams` +1. Typepedia and guide §3 name both.
Landed as 5c36f5b5.
