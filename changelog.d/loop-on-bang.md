## loop-on-bang - `!.loop(s)(f)`: tailRecM for programs, and Toolkit's dialogs over it

Stage 2 of specs/fold-until.md. `!.loop[S, A, F](s)(f: S => Either[S,
A] ! F): A ! F` in `object !` beside `tailcall`: run `f` from `s`,
continue from a `Left`, answer a `Right` — the operator's
state-decides-when-to-stop form over a program whose every iteration
may perform `F`. Stack-safe with no trampoline of its own: the
recursive call sits inside the `flatMap`'s continuation and is made
when the interpreter resumes that Bind. Not top-level, because
Generate.scala's Cont fixpoint is called as `loop(f)(a)` — the same
two-list shape — and the overload would be ambiguous at every one of
its calls.

The proof the trigger asked for, read honestly: okay-ui `Toolkit`'s
four dialogs (`confirm`, `alert`, `prompt`, `choice`) are `!.loop`
programs now and are NOT shorter in lines (41 non-comment lines
before, 40 after); what went away is the hand-written recursion and
seed in each, and every case answers a value under `map` rather than
a program under `flatMap`. Actor's receive loop and `Dialog.run` were
written out over `!.loop` and left alone: each already folds an
`Either` per step, and under the loop's own `Either` the two `Right`s
mean opposite things (Decisions). `TestBangLoop` (core, 3): a
1 000 000-iteration counter on the default stack, `f` once per
iteration, a loop over `State`. `TestToolkit` unchanged, green.

Coordinated in the room with okay-direct's generator lane
(direct-staged-v2): a generator stays `Unit ! Writer % W`, its
`take/first/exists` go through `Writer.foldUntil`, the two laws it
relies on are the ones `TestFoldUntilStreams` pins, and stage 3 is
named `Stage.transduceUntil` so a stateful `Gen.takeWhile` can route
to it. Landed as 20c38955.
