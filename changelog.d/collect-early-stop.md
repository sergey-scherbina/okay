## collect-early-stop - `Delim.collectUntil`: the same push producer, stopped by a `FoldUntil`

The trigger the operator lifted ("сделай без триггера. Это полезная
штука"). `collect` builds its list on the way BACK, in the
continuation frames, which is why `exit` inside it could never answer
with the prefix. `Delim.collectUntil(using fo: FoldUntil[A, S, R])
(body): R ! F` runs the SAME producer — a body written against
`Emitting[A]`, unchanged — and passes the fold's state on the way
DOWN: the prompt's answer is a function of the state (Filinski's trick
for state over shift/reset, `PState`'s over `Cont`), each `emit`
answers `s => …`, adds, and either ends with `fo.end` — the
continuation never called, the rest of the producer never run — or
resumes into the next such function. No mutable cell, so a multi-shot
capture inside the producer sees its own state; `done(init)` runs no
body; a fold never done answers what `collect` answers.
`collectingUntil` nests as `collecting` does. `Emitting[A]` is now
sealed over two evidences (`Listing`, `Stopping`) and `emit` is one
inline door over both, so every existing producer runs under either;
the one cast — the block's row meeting the evidence's, one row the
type system cannot join — is isolated in `Stopping.atRow` with its
reason. `TestCollectUntil` (okay-direct, 8: take(3) over a tree walk
visits 3 leaves, find(_ == 5) visits 5, take(0) visits 0, never-done
equals collect, the operator's running-sum `until`, 10 000 emits on
the default stack, collectingUntil under delimited, the docs snippet
verbatim); every existing collect suite unchanged, 48 green. Docs:
continuations-in-practice §2 and the table, typepedia, theory ch. 2;
specs/delimited-control.md box closed. Landed as 5093ce53.
