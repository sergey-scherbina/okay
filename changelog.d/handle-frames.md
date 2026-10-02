## handle-frames — a handler nested in a handler takes no host stack (stage 1: State and Effects.handle)

Every handler in okay was an EAGER loop run at the call, so a handler called
from code another handler's loop forces ran inside it: 100 000 nested
`State.handle` — no `reset` needed — overflowed a 128 KB stack (the
operator's question "why not the same machine?"). Now the state form
(`Handler.stateOf`, so `State`) and the control form (`Effects[Free].handle`,
`Handler.control`, `Reader.local`) answer a VALUE (`HandleFrames.Run`, a
`Frames.Pending` in a `Delay`): forced by anything else it is the fast fold
as before; met by a running machine it is a FRAME on that machine's stack —
a `Dollar` whose delimiter is a `Cont0.Handling`, an operation it takes made a
`shift0` to it with the clause for its body. A fold that meets a nested run
UPGRADES: it hands itself, its state and the rest of its program to the
machine as a frame (`Freer.resumeRun` stops at a run instead of forcing it).
100 000 nested `State.handle`, reset/State/reset and nested `Effects.handle`
run on 128 KB and on Scala.js and Native; the frame agrees with the fold
(differential, cross; a mutant reds it). Semantics: `Reader.local` under the
machine now reaches a `Shift.push` body's asks (scoped-effects-laws' first
limit lifted, 5 -> 23). Measured: handlePrebuilt 0.97x, stateEffect 1.00x,
100 small `State.run`s 1.18x. Stages 2-3 (relay, translate, the bespoke
loops) queued as handle-frames-forms. specs/handle-frames.md.
