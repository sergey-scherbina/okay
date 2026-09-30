# The machine's continuation stack as a Freer: `Frames`, `Reset`, `shift0`

## Overview

The operator's ask (2026-09-30): "Стек продолжений машины в виде
Freer" — find the one foundation for continuations, minimal and fast,
on which everything else (Delim's family, Cont, generators, handlers)
is expressed rather than re-implemented. The backlog item
`freer-kont-frames-probe` had proposed a `Kont` of segments cut by
handler MARKS with state; the design conversation stripped that to
what is absolutely necessary, and this spec records the result and why
each thing that was dropped was dropped.

**The design, in one sentence: the machine's stack is a type-aligned
list of frames that is ITSELF a continuation (`Frames <: A => Freer`),
the delimiter `$` is a FRAME on it (`Frames.Reset`: the prompt and its
`ret`), and `Cont0 = Shift0 | Reset0` are the two operations — `Reset0`
asks for that frame, `Shift0` cuts the stack at it and hands the cut,
as a function, to its body.** The tree does not change: `Freer`'s five
cases stay, `Freer` keeps its own `resume`, and it knows nothing of the
stack — the machine recognises a `Frames` in a `Bind`'s continuation by
class, and a `Frames` applied as a plain function by any other
interpreter applies ONE frame per step, so it is a continuation for
`Freer.resume` too. `Freer` is self-sufficient; `Cont` is orthogonal
(operator, 2026-09-30). What
changes is `resume`: the rotation `Bind(Bind(a, f), g) ⇒ Bind(a,
f(_).flatMap(g))` (a closure per left-nesting, re-pushed down every
step, the JIT-mode lead of `freer-rotation-closure-jit-modes`) becomes
a PUSH onto the stack, and the stack is what a handler receives as `k`.

The smart-object idiom is the one `Freer.Mapped` already uses — a
function that knows what it is: `Mapped` is a frame that only maps and
a builder can read its function; `Frames` is a frame that is a whole
stack and the machine can splice it, and its `Reset` case is a frame
that is a delimiter and a cut can stop at it. They sit in a `Bind` as
an ordinary `A => Freer`, are callable as one from outside, and are
taken apart by the one loop.

## Types

```
enum Frames[G[_, _, +_], A, S, T, Z] extends (A => Freer[G, S, T, Z]):
  case End[G, A, S]()                                    extends Frames[G, A, S, S, A]
  case Frame[G, A, S, S2, T, Y, Z](f: A => Freer[G, S2, T, Y],
                                   rest: Frames[G, Y, S, S2, Z]) extends Frames[G, A, S, T, Z]
  case Reset[G, A, S, S2, T, Y, Z](p: Prompt[S2, Y], ret: A => Freer[G, S2, T, Y],
                                   rest: Frames[G, Y, S, S2, Z]) extends Frames[G, A, S, T, Z]
  apply(a) = End: Return(a) | Frame(f, rest): Bind(f(a), rest) | Reset(_, ret, rest): Bind(ret(a), rest)
                                                          -- ONE FRAME PER STEP: valid under any interpreter of the tree;
                                                          -- the machine never calls it, it splices `rest`

enum Cont0[F, T, R, +X]:                                          -- an enum for the `+X` (Delim.Op's shape)
  case Reset0[F, S, Y, A, T, R](p: Prompt[S, Y], ret: A => Freer[Row[F], S, T, Y], body: Freer[Row[F], T, R, A])
  case Shift0[F, S, Y, T, R, X](p: Prompt[S, Y], f: Frames[Row[F], X, S, T, Y] => Freer[Row[F], S, R, Y], at)
type Row[F] = [T, R, X] =>> Cont0[F, T, R, X] | F[T, R, X]

dollar(p)(ret)(body) = Inject(Reset0(p, ret, body))       -- an OPERATION; the machine turns it into the frame
reset(p)(body)       = dollar(p)(Return(_))(body)
shift(p)(f)          = shift0(p)(k => reset(p)(f(k)))
```

**Answer-type modification is `Bind`'s own index composition.** A
`Freer[G, S, R, A]` reads `(A => S) => R`; `Bind(a: Freer[T, R, A],
f: A => Freer[S, T, B]): Freer[S, R, B]` joins the indexes end to end.
`Frames` is typed by the SAME join — `Frame`'s `f` consumes `T` and
produces `S2`, `rest` goes on from `S2` to `S` — because the stack IS
the `Bind` spine with a hole at its leftmost leaf, read bottom-up.
`End` is `Return`'s diagonal. So `$` is where the answer type changes:

```
body: Freer[G, T, R, A]     ret: A => Freer[G, S2, T, Y]     dollar: Freer[G, S2, R, Y]
```

which is today's `Cont.run(c: Rep[A, S, R])(k: A => S): R` — `$` with a
strict `ret` — and `reset(m: M[A, A, R])` is `$` with `ret = identity`,
whence its `S = A`. A `shift0` leaf `Inject(g): Freer[G, T, R, X]` gets
`k: Frames[G, X, S, T, Y]` (the segment up to AND INCLUDING the
`Reset`, so `k` carries `ret` — the `$/S0` rule) and answers
`Freer[G, S, R, Y]`, the type of what stands in the delimiter's place.
`(T, R)` are the leaf's own indexes; `(S, Y)` are the delimiter's and
come from the PROMPT: `Prompt[S, Y]` is a unique object, `p.same(q)`
answers `Option[(S =:= S2, Y =:= Y2)]` by identity, exactly as Delim's
`(m.p === p) match Some(ev)`. What this pair can and cannot say for a
LAZY `k` is a finding of the probe — Results, below.

**Pure `Cont` is the special case with a strict `k`**: `Shift[S, R, X]
= (X => S) => R` is this leaf with `k` run through `ret` to a VALUE,
so its `S` is the delimiter's `Y` and the `S2` below vanishes. Same
leaf, same machine; only how the clause applies `k` differs.

## The machine

The invariant: the program is `Bind(focus, fs)`; `S0`, `R`, `Z` are
fixed for the whole run (the outermost delimiter's), every step is
checked by GADT refinement, nothing casts:

```
loop[X, T](focus: Freer[G, T, R, X], fs: Frames[G, X, S0, T, Z]): Freer[G, S0, R, Z]
  Bind(a, ks: Frames) → loop(a, ks ++ fs)          -- a resumed k, or re-entry from outside; fs = End: ks itself
  Bind(a, f)          → loop(a, Frame(f, fs))      -- push
  Return(x)           → End: focus  |  Frame(f, rest): loop(f(x), rest)  |  Reset(p, ret, rest): loop(ret(x), rest)   -- ($v) is a pop
  Delay(t)            → loop(t(), fs)
  Inject(Reset0)      → loop(body, Reset(p, ret, fs))                                 -- the operation becomes the frame
  Inject(Shift0)      → cut fs at the first Reset(p): k, below; loop(f(k), below)   -- ($/S0)
  Inject(e)           → Bind(focus, Reenter(fs))   -- the head form: k RE-ENTERS the machine with its stack
```

`++` and the cut are two `@tailrec` walks over a reversed list `Rev`
(the same type-aligned discipline, outermost first): a cut reverses the
prefix up to the `Reset` and links it onto `End`; a splice reverses
`ks` and links it onto `fs`. Both are O(|segment|) and amortised free:
every frame copied is a frame about to be run. A splice onto `End` is
the segment itself — the re-entry from an outer handler costs nothing.

**The head form's `k` re-enters the machine.** An operation nobody on
the stack answers goes out to whoever runs the program — a handler
loop over `Freer.resume`, which knows nothing of frames. If its `k`
were the raw `Frames`, the loop's rotation would apply the frames
itself and the machine would be gone: a `Cont0` operation met later
would have no stack to cut. So `k` is `Reenter(fs)`, a plain function
`x => run(Bind(Return(x), fs))` — `Delim`'s `Out(inject(g).flatMap(x =>
loop(..)))`, the same shape — one JVM call per re-entry, each returning
the next head form before the next. The ordering this fixes is the
library's rule already ("one machine, innermost": `State.handle(
delimited(..))`, never a loop between a program and its machine).

## What is NOT in the foundation, and why

- **No `Kont` wrapper**: the stack is the continuation.
- **No `Mark`/`Seam`/segments, no `Handler` (`Tail`/`Control`), no
  `Step`**: those existed only for HANDLERS WITH STATE as marks (a
  `State.handle` without its loop). They are an extension — one more
  smart frame plus a tail-resumption rule — measured on their own if
  ever wanted. The 112 `case Bind(Inject(e), k)` handler loops of the
  library consume the same head form as today and do not move.
- **No handler for `Cont0`**: the delimiter is found ON THE STACK by
  the machine; `Reset0` is the operation that asks for it (an operation
  is forwarded by a loop that does not own it, a frame is not — the
  same reason `Delim.Push` is one), `Shift0` the one that cuts.
- **No separate Delim machine**: Delim's family is this plus prompt
  tags and two bits over the same cut — take the `Reset` frame into
  `k` or stop one frame short (`shift0`/`control0`), run the body under
  a fresh `reset(p)` or bare (`shift`/`shift0`). `push(p) =
  dollar(p)(pure) = reset(p)`.
- **Nothing in `Freer`, and `Freer` keeps its `resume`** (operator:
  "Freer самодостаточный, Cont ортогональный"). The stack, the
  delimiter, the operations and the machine live in one file of their
  own (`Cont.scala` after the migration, `kont/Kont.scala` in the
  probe); `Freer` is usable without them exactly as today, the 112
  handler loops keep calling `Freer.resume`, and the machine is a
  second interpreter, `Cont.run`, that the head form's `Reenter`
  brings back. `Cont` (`Shift0` with a strict `k`) is defined beside
  it. The rotation-free descent is therefore the machine's, not every
  loop's — making `Freer.resume` the frame machine is a separate,
  later decision, not this one.
## Behavior

Stage 1, the probe (`src/main/scala/kont/Kont.scala`, package
`okay.kont`, ADDITIVE — nothing in `Freer`, `Delim`, `Cont` moves, and
`Freer.resume` is not touched;
`src/test/scala/kont/TestKont.scala` is the oracle):

- [x] `Frames` as an enum over the indexed `Freer` — `End | Frame | Reset`,
      `apply` typed by the GADT, no cast in the machine's loop
- [x] `($v)`: a body that returns runs `ret` once
- [x] `($/S0)`: `k` dropped — `ret` never runs; `k` twice — `ret` per call;
      `k` once with a suffix
- [x] `shift` under a `$`: body under a plain `reset`, `k` carries `ret`
- [x] `k` re-installs the `$`: a second `shift0` in the continuation is
      caught by it
- [x] `ret` runs OUTSIDE the re-installed `$`: a `shift0` in `ret` escapes
      to the next delimiter
- [x] the delimiter changes the VALUE type: a `Prompt[Int, String]`
      delimiter whose body answers `Int` and whose `ret` and `$` answer
      `String`, typed without a cast, `k` resumed twice (what the
      `(S, R)` pair can and cannot say here: Results)
- [x] depth: 100 000 `$` nested (via `Delay`), constant stack, each `ret`
      once on the way out
- [x] a generator: 100 000 `shift0`s whose clauses each resume `k` —
      the resumptions are lazy nodes, one loop, constant stack
- [x] left-nested binds: a 100 000-step `foldLeft` of `flatMap`s runs
      in constant stack with no rotation
- [x] the head form: a foreign operation inside a `$` comes out as
      `Bind(Inject(e), k)`; `k` applied to an answer completes; applied
      to two different answers gives two different results (the frames
      are shared immutably, multi-shot re-entry)
- [x] orthogonal: a handler loop of today's shape, over the real
      `Freer.resume`, OUTSIDE the machine — it answers its operation,
      its `k` re-enters the machine, and a `shift0` after it still finds
      the delimiter and resumes twice
- [x] a `shift0` with no `$` for its prompt fails by name with the
      installed delimiters listed
- [ ] the four lanes of the backlog item measured through `jmh-lane.sh`
      against the Delim machine (DelimBenchmark) and the rotation
      (`Fib`, `BuildShapeBenchmark.rowFoldM`) — RESULTS below

Stage 2 (a decision, not this lane): migrate — `Freer.resume` becomes
this loop, the Delim machine and `Cont.step`'s own runner are expressed
over it, `Prompt` gains its `S` index — or stay. Stage 3, if wanted and
measured: handlers with state as marks.

## Decisions

- **`Reset` is a frame; `Reset0` is the operation that asks for it.**
  A `Return` at the frame is the ordinary pop — `ret(x)` — so `($v)`
  needs no rule; the operation becomes the frame in one arm. A first
  cut built the frame directly (`Bind(body, Reset(.., End()))`, a
  one-frame stack spliced in); the operation form is uniform with
  `Delim.Push` and forwardable by a loop that does not own `Cont0`
  (`relay`), at one `Inject` per delimiter — what `push` pays today.
  Named `Reset`/`Reset0`, not `Dollar` (operator): `$` is reset with a
  `ret`, `reset` is `$` with `pure`; `shift0`/`reset0` is the pair of
  the literature.
- **Three enum cases, the delimiter a case of its own** (operator,
  after a first cut kept it inside `Frame.f` as a smart function
  `Dollar`). As a case the cut and the pop are typed by the GADT and
  the second class test (`Dollar.as`) is gone; the only class test left
  is `Frames.as` on a `Bind`'s continuation. And installing a delimiter
  is the SAME splice a resumed `k` takes: `Reset(p, ret, End())` is a
  one-frame stack.
- **`Cont0` is an enum with one case for the variance alone**: a
  `Freer` signature is `G[_, _, +_]`, `X` sits invariantly inside
  `Frames[…, X, …] =>`, so the case cannot be `+X` but its parent can —
  `Delim.Op`'s shape. Nothing else is implied by the enum.
- **Immutable, copy-on-cut, splice-on-resume**, not a shared chain with
  a stop marker: O(1) capture is possible with underflow records, but a
  capture that crosses a splice boundary then spans two chains and the
  design grows the very structure it saved. Delim's machine copies too.
- **The probe is self-contained.** Its head form `Bind(Inject(e), fs)`
  is for a caller running the SAME loop: under today's `Freer.resume`,
  `fs(x) = Bind(Return(x), fs)` would be rotated back into itself. The
  migration replaces `resume`; the probe does not try to coexist.

## Results

**Stage 1 landed as the probe: 16/16 oracles green through the gate,
no warnings, `recscan` holds** (`okayJVM/testOnly okay.kont.TestKont`; green again with `Reset` as a
case of the enum and the delimiter installed by splice).
Every rule of TestDollar gives the same answer on the frame machine
that it gives on the Delim machine; the four depth tests run in
constant stack; the head form re-enters twice with two answers.

**Answer-type modification in the LAZY machine is not Cont's `(S, R)`.**
Found by the typed test, before any code was written for it: Cont's
`Rep[A, S, R]` lets a shift body ESCAPE with a value of type `R` while
`k` answers `S`, because `k` is strict and nothing sits below the run.
In the lazy machine a `shift0` body STANDS IN THE DELIMITER'S PLACE
and the frames below take the delimiter's value `Y` — so the body
produces `Y`, `k` produces `Y`, and there is no escape type. The
invariant makes the `R` index redundant: every focus has the run's `R`,
`End` forces `S0 = T` at the top, `Return` forces `T = R`, so a run that
completes has `S0 = R` and the pair carries nothing a value type does
not. What the index CAN carry for `shift0`/`$` is Materzok &
Biernacki's own type system: a STACK of answer types, one per enclosing
delimiter — `$` pushes one, `shift0` pops one — which is exactly
`Delim.Stacked`'s `p.type *: b` typestate. The `Frames` join carries
that unchanged (a `Frame` goes from one stack to another); `Prompt`
then needs no `S`, only the tuple index. That is stage 2's typing, and
the reason `Cont` keeps `(S, R)`: it is the strict, one-delimiter case
where the escape type is real.

## Literature

- Materzok & Biernacki, "A Dynamic Interpretation of the CPS
  Hierarchy" (APLAS 2012): `shift0`/`$`, the `($v)` and `($/S0)` rules,
  and that `shift0`/`$` macro-express shift, control, reset and prompt.
- Dybvig, Peyton Jones & Sabry, "A Monadic Framework for Delimited
  Continuations" (JFP 2007): the continuation as a sequence of frames
  with prompts; the cut at a prompt.
- Danvy & Filinski, "Abstracting Control" (1990): answer-type
  modification; Asai & Kameyama's polymorphic typing of it, which the
  `Bind` index join is.
- Kiselyov & Ishii, "Freer Monads, More Extensible Effects" (2015): the
  tree; van der Ploeg & Kiselyov, "Reflection without Remorse" (2014):
  the continuation as a type-aligned sequence — here the stack, not
  the node.
