## foreign-one-value — R on the one value tree, effect, API and handler (2026-09-26)

Stage 2a of specs/foreign-one.md. R keeps no tree, effect, API, journal
instance, codec or engine of its own: an R value is the one tree
(`PyValue`, which gains R's typed `NA`), an R call is a `ForeignEval`,
`R` is the one API at `R.shape` (R's value and frame rules, `RCodec`),
and `RSubprocess` is a `ForeignWorker` speaking R — supervised when it has
a deadline, so R's own replay is gone and R now recovers from a DEATH as
well as a timeout. `RValue`, `REval`, `RStep`, `RNode`, `RFrame`, `ToR`
and `okay.r.Condition` stay as R's names over the shared types; R code
and every okay-r test read unchanged. okay-r main 1 869 → 1 220 lines.

The wire: shim.R v9 speaks the shared value tags; frames cross in the
columnar shape wherever a hello claims it (Python, TypeScript and R now
do); the shared decoder still reads R's old tags. The shared API takes
the CALLER's `Shape` (it had fixed Python's inside `object Py`), and a
frame carries the rules it is read by, tagged by the worker that answered
it — found because R's conformance row failed, and because a mutant
removing the tag survived every suite until `TestRFrameRules` (new).

R is a row of the ONE conformance body and the ONE crash suite
(`TestRConformance`, `TestRCrash`). `TestRFacade` ran live for the first
time and found the facade's R instances addressing R as `module:fn`
(fixed to `module::fn`). Live: okay-r 98, okay-py 226, okay-foreign-cluster
26, okay-foreign-workflow 12. Durable R journals written before v9 are
refused by the drift check rather than replayed.
