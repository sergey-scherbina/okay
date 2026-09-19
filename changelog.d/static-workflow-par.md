## static-workflow-par - two waits at once, and the honest limit of one journal

Stage 5 of specs/static-workflow.md listed three things the closure
had forbidden and the term makes possible, each gated on a trigger.
The operator ungated the first (2026-09-18, "сделай gated потом
пригодится") — so this is built for the shape rather than for a
consumer, which is worth saying out loud: no workflow here yet
serialises two independent waits.

`Proc.Par(f, g)` is two branches over the SAME input — that is what
independent means, and a branch that needed the other's answer would
be a `Then`. It is a node and not `&&&` because an arrow's fanout is
derivable from `first` and `compose`, and what it derives is plumbing
that walks as ONE position. The node exists so the position can be a
pair of paths.

WHAT IS PARALLEL IS THE WAITING, NOT THE JOURNAL, and the constraint
that decides it was there before the node: a record is
`Either[SysA, A]` and says nothing about which question it answers —
the driver matches records POSITIONALLY — and a tagged record is the
second journal format this spec puts out of scope. So a `Par` records
its two answers in term order, left then right. What it buys is that
both pending questions are known before either is answered, so a front
end puts them to finance and to legal on the same morning instead of
one after the other. That is the half that costs calendar days.

The consequence is stated rather than left to be discovered: an answer
cannot be COMMITTED out of order. If legal replies first the front end
holds it until finance's arrives.

`walk` can see both because of an accident of the fold worth naming:
`Walked.Asking` is produced in exactly one place, the `Op` case's
`Nil` arm, so when the left branch is waiting the journal is EMPTY —
and the right branch therefore starts from an empty journal too and
its own first question is knowable with no lookahead and no second
pass.

ADDITIVE, and deliberately: `Walked.Asking` now carries a VECTOR of
(path, question), so every arm of the fold that forwards a stop
(`case stop: Walked.Asking => stop`) stayed exactly as it was; and
`walk` still answers `Standing.Asking` when there is one question, so
`accepts`, `strands` and every consumer of a term without a `Par` are
untouched. `Standing.Waiting` is the new case, `Standing.pending`
reads all three alike, and the five walk-vs-replay tests now compare
the FIRST pending question — which is the keystone property restated
exactly: `program` maps `Par(f, g)` to `f` then `g`, so the head of
`Waiting.on` is what the engine will consume.

`Step.Side(n)` is its own step rather than `Fst`/`Snd`, so a path says
which KIND of node it went into; `render` indents on it and `mermaid`
draws a fork (`{{"both"}}`) and a join. TestProcPar is 12 tests: the
pair, the order, the lopsided case that is an ordinary wait, the
keystone over a term with a `Par` in it, the leaves, the picture, the
position inside a branch, and a counter behind both branches that says
which ran first.
