## static-workflow-undo - compensation as structure, and no ArrowApply anywhere in it

Stage 5's second gated item, ungated by the operator. The trigger — "a
saga written by hand on the cancelled branch twice" — had NOT fired,
and checking it found something more useful than a missing trigger:
`okay.persist.Saga` already compensates a LINEAR sequence, journaled
intent-first, with forward and backward recovery and a status. So the
question was never "build a saga"; it was "what does a TERM add", and
the answer is three things.

It compensates a SHAPE. A saga is a `Vector[Step]` and has to know its
steps before it starts; a term's walk goes through branches and loops,
so "one room per night, where the number of nights is an answer" has
three cancellations after three rounds and two after two. A branch not
taken leaves nothing to undo.

It uses the WORKFLOW'S OWN JOURNAL, not a second one.

And what comes back IS A TERM. `Wf.Proc.compensating(p)(x, journal)`
answers a `Wf.Proc[Q, A, Unit, Unit]` — an ordinary workflow, which
runs on the landed engine, writes to the same journal, draws itself,
and resumes from its own position if the compensation is interrupted
halfway. A saga that is a workflow rather than a mechanism beside one.

THE UNDOS ARE FOUND, NOT CARRIED, and that is the only reason any of
this is possible in an arrow. The obvious design puts a stack of
compensations on the edge to be run later — a computation carried as a
value and then run is `app`, which is `ArrowApply`, which is a monad,
which is the one thing `Proc` refuses. A term is walkable, so the
compensation for a step is simply there at the path where the step
ran; the fold builds each piece as
`arr(_ => (x, y)) >>> undo` at the point where `x` and `y` still have
their types, so there is not one cast in it.

`Proc.Undo(step, undo)` is the node and `Step.Back(b)` its path step
(`do` / `undo`), so `leaves` names a compensation beside its step and
`render` indents it. The picture draws it OFF the forward path on a
dotted edge to an "on failure" diamond, because that is the truth
about it.

Failure is not a new node: a term that can fail threads `Either[E, ·]`
and `OnRight` already passes a `Left` through untouched, so the
short-circuit is the ordinary choice. `compensating` decides nothing
about when — the author does.

ONE COLLECTOR, NOT A SECOND FOLD. `go` gained an `undos` buffer rather
than a mirror function that only collects: a second copy of a fold
with the non-consuming `Patch` rule in it is exactly the drift this
file warns about elsewhere. The one subtlety is that `walk` looks
SPECULATIVELY into a `Par`'s right branch to report both pending
questions, and that walk gets a throwaway buffer — otherwise it
collects a compensation for a step nobody took. THE FIRST VERSION OF
THAT TEST PROVED NOTHING: with the guard removed it still passed,
because an `Op` on an empty journal stops at `Asking` and collects
nothing whatever the buffer is. The shape that catches it is a
compensable step that needs no answer — a PURE one — which completes
on an empty journal; with the guard removed that test now reports
`free slot-1`, a refund for a reservation that was never made.

NOT FIXED HERE, THOUGH THIS LANE FOUND IT: `static-workflow-par` left
`TestProcDirect.scala:74` non-exhaustive when `Standing.Waiting` was
added, and that lane's gate never saw it because the worktree was WARM
— zinc does not recompile a file your change did not touch, and a
warning is only emitted when its file is compiled. This lane's fresh
worktree reported it on its first compile; the room was told, and
`standing-exhaustive` (8ef67726) landed the fix on master with a
better arm than the one drafted here — a `Waiting` case that FAILS
with what it saw, rather than quietly taking the first question. This
lane's gate ran COLD (targets removed first, 203 module compiles
against 4 warm) for exactly that reason. TestProcUndo is 10 tests.
