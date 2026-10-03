## shift-prompt-key — Shift.Stacked: the prompt stack in the row

specs/shift-merge.md stage 3, the last: `Shift.Stacked` keys a delimiter by
its own type in the ROW (`Shift % d.type`) instead of a tuple-indexed stack
(`Stack`/`Has`/`Under`/`rebase`, gone). `reset(d => body)` hands its body a
`Reset[R, F]` — its prompt and the row OUTSIDE it, fixed when installed — and
runs it, or pushes it on the machine already running (`Shift.Machine`);
`shift(d)[A]`, `shift0(d)[A]`, `abort(d)[A]` and `dollar` type their bodies at
the delimiter's own row, so no key of a delimiter installed inside can reach a
body (the first cut let the caller name that row, and a row is a set — it
typed a body that threw `NoPrompt`). A capture to a sibling's delimiter, an
escaped one, a consumed one, or a bare prompt does not compile where it is run
or embedded. A delimiter's key is told apart by value (`TypeableK.ByValue`),
so two share a row. Lexical.Stacked and Layered.Stacked moved onto it;
continuations-in-practice's "The stack in the row". The one effect `Shift % K`
now has all three keys: an answer type, `?`, and a delimiter's own type.
