# Delimited with answer-type modification: a typed CK + meta machine

Operator ask, 2026-10-03 ("Делимитер с ATM", after cont-run-prompt): the one
cast left in Cont.scala is the run's frame claiming the type of the `k` it
hands a leaf. A PROBE, in the test tree; the core is untouched until it
answers.

## Why the cast exists today

Cont is Danvy–Filinski `shift`/`reset` WITH answer-type modification:
`Cont[A, S, R]` is `(A => S) => R`, and inside one `reset` every `shift` may
change the answer type (`shift[Int, Int, String](k => k(1).toString)` then
`shift[Boolean, Boolean, Int](...)`). Filinski's `A / F` and our `A ! F` have
no such indexes: the answer type is fixed per delimiter (`Shift % R`).

Cont runs on the λ$ frame machine (Delimited.scala). There a capture COPIES
the delimiter into `k`, at the type the delimiter was installed with, and the
machine's loop holds one `R` for a whole run (`Freer`'s `R` is constant along
the left spine). Under D-F every installation of the delimiter has its own
answer type: the root's is the run's `R`; the copy a call of `k_i` installs
answers `S_i`. The λ$ typing cannot say that, whatever the delimiter type —
so the frame claims it.

## Hypothesis

D-F's own abstract machine types it with no claim: a CK machine with a
META-continuation (Biernacka, Biernacki & Danvy, "An operational foundation
for delimited continuations in the CPS hierarchy", LMCS 2005). State
`(C[A, S, T], K[A, S], M[T, R])`:

- `C[A, S, R]`, `(A => S) => R` as data: `Pure`, `Bind`, a leaf in its forms;
- `K[A, S]`: the continuation up to the `reset`, a typed stack (`Done[A]:
  K[A, A]`, `Push(f: A => C[B, S, T], k: K[B, S]): K[A, T]`);
- `M[T, R]`: the reset boundaries, each frame typed by ITS answer —
  the delimiter with ATM, per installation. A lazy `k` called from a body
  pushes `Then(rest, m)` and runs `k` to `Done`: the boundary D-F's
  `k = λx. reset(K[x])` needs, which cont-shift-op (REFUTED 2026-10-01) lost.

Every transition is a GADT match whose equations make the next state typed.

## Behavior (the probe)

- [x] no `asInstanceOf`, no `@unchecked`, no `Any` in the machine
- [x] D-F's ATM example: two leaves changing the answer type Int → String
      and Boolean → Int in one `reset`, strict and lazy, the right answer
- [x] `k(x + 1) + k(x + 1)` chained d = 0..6, strict and lazy, against the
      closure instance (cont-shift-op's refuting case)
- [x] multi-shot: a lazy `k` resumed twice
- [x] stack safety: 1M left-nested binds, and 1M lazy-`k` shifts
      (contAnswer's shape) on a 256 KB thread
- [ ] a verdict: what moving Cont onto it costs (ContMacro, `programLeaf`,
      Effects' `onAnswer`, the frame machine's sharing) and its speed on
      contAnswer / statePara / fib100 against master

## Stage 2 — the operator's direction: Delimited itself gets ATM (2026-10-03)

"Меняем Delimited для поддержки ATM"; "правильность дизайна доказывается в
первую очередь типами". What is wrong in Delimited for ATM, found by stage 1:

1. the answer type lives on the PROMPT (`Delimiter[Y, I]`, DPJS's typed
   prompts): every installation of a prompt — the root and each copy in a
   `k` — has one type, held by the `identical` axiom;
2. `Dollar` fuses the `ret` frame with the BOUNDARY: a capture copies both
   into `k`, the boundary's type frozen at the capture;
3. one `R` for a whole run, `$` transparent to it (`Dollar0`'s body shares
   the outside `R`): no level has an answer of its own.

The design to prove by types, in a probe before the core (stage 2a), then
in Delimited (2b):

- `Freer[G, S, R, A]` unchanged as a tree; its `(S, R)` now read as the
  answer pair of the NEAREST delimiter (it is that already for Cont);
- a delimiter is two things: `ret`, an ordinary frame at the bottom of its
  level's `K`, and the boundary, `Level(p, kOuter, m)` in the meta-
  continuation `M`, typed per installation;
- a capture to the NEAREST delimiter is ATM-typed with no claim (stage 1:
  the body is polymorphic in the outer level's answer);
- a capture to a NAMED prompt through levels (Shift, handler frames) takes
  `k` as a typed chain of segments `Seg[A, Z]`; the one claim is the
  generative-prompt axiom, DPJS's `unsafeCoerce`, that a prompt's
  installation is at the prompt's declared type — unavoidable while prompts
  are named at run time rather than by singleton types;
- THE RULE that makes it typeable: answer types may change at the nearest
  delimiter only; an operation that crosses other delimiters is diagonal
  there `(X, X)`. Free's operations are (`Unit, Unit`); Cont's leaves
  target their own `reset`.

- [ ] 2a: the probe — `Level`/`Seg`, nearest capture with ATM, named capture
      through levels with the one axiom, resumption of a multi-level `k`,
      a deep handler frame; stack-safe; tests
- [ ] 2b: Delimited on it; Shift, HandleFrames, Layered, Lexical, Cont moved;
      the full gate; A/B on the core lanes

## Decisions

- A probe in `src/test/scala/ProbeContAtm.scala`, self-contained: it shares
  nothing with Cont.scala, so a refutation costs the core nothing.
- The strict `k` is a nested run of the machine (host recursion), as Cont's
  is today; its room and `StackSwitch` are out of the probe's scope.

## Results

- 2026-10-03, first cut: THE HYPOTHESIS HOLDS FOR THE TYPES. `ProbeContAtm.scala`
  compiles with zero `asInstanceOf`, `@unchecked` or `Any`, no warning, and
  TestProbeContAtm's four tests are green (ATM Int → String / Boolean → Int,
  the d = 0..6 refuting case, multi-shot, 1M binds and 1M lazy shifts on a
  256 KB thread). Every transition is a method whose GADT match supplies
  the equation the next state needs: `Pure` gives `S = T` (so `Apply` may
  take `m: M[T, R]` as `M[S, R]`), `Done` gives `A = S`, `Top` gives
  `T = R`, `Push`/`Then` compose their indexes.
- Open: the verdict on moving Cont onto it, and its speed.
