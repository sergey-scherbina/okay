# Closing the runtime hole: a capture that cannot find its delimiter

## Overview

`NoPrompt` is the one way a well-typed program using delimited control
still fails at run time. `Prompted[R]` (delim-prompted, 2026-09-16)
closed the easy half — a capture with NO delimiter in scope is a
compile error, because the evidence cannot be forged. Two halves are
open, and delim-nesting (2026-09-17) showed that the one everybody
said was theoretical is the one people hit first:

1. **The wrong machine.** The evidence proves a delimiter was
   installed; it does not prove that the machine executing this
   capture is the one holding it. `delimited`/`collect`/`resumable`
   each run their own machine, so an outer evidence used inside an
   inner one throws — and the row type is happy, because
   `collect[A, Delim + F]` merely puts a second `Delim` in the row
   and rows misroute a duplicate rather than rejecting it.
2. **The escaping evidence.** A `Prompted[R]` stored and used after
   its `delimited` has returned. Named in Delim.scala's header from
   the start; still open.

Nesting now has a right spelling (`scope`, `collecting`, `pausing`),
so the question this spec answers is narrower and better: **what stops
someone writing the wrong one?**

## Stage 0 — the duplicate row is a compile error

The combinators that RUN a machine take a witness that the row does
not already have one. The witness is `Delim.OneMachine[F]`, and the
membership test inside it is `NotGiven[Delim[Any] <:< F[Any]]` —
membership by APPLICATION rather than `RowLift.In`, for a reason that
was measured and is recorded in the Results: the `In` formulation
crashes the compiler.

```scala
def delimited[R, F[+_]](body: Prompted[R] ?=> R ! (Delim + F))
                       (using OneMachine[F]): R ! F
```

with the message the error should have had all along:

```
this row already contains Delim, so `delimited` would start a SECOND
machine — and a capture cannot cross from one machine's prompt stack
to another's. Use `Delim.scope`, which installs a delimiter on the
machine already running.
```

`scope`, `collecting` and `pausing` take no such guard: installing a
delimiter in a row that has `Delim` is precisely what they are for.

**What it catches and what it does not, stated plainly.** The guard
fires on a CONCRETE row, which is the trap as people write it
(`Delim.collect[Int, Delim + Pure]` inside a `resumable`). It does
NOT fire on an abstract `F`: `NotGiven` reads "unknown" as "absent",
the same caveat `Failing` already documents for its `In[Async, F]`
probe. A generic helper that takes `F[+_]` and calls `delimited` will
still compile and still throw if instantiated at a Delim row. That is
a real limit, it is why stages 1 and 2 exist, and it must not be
written up as "NoPrompt is now impossible".

A library author who wants the guard to reach THEIR callers takes the
witness themselves — `def mine[F[+_]](…)(using Delim.OneMachine[F])`
— and the obligation propagates to the call site, where the row is
usually concrete. `Delim`'s own `answer`/`replay`/`drive` do exactly
that, and they have a second reason to: an obligation carried as a
parameter is never searched for at an abstract row, which is what
kept the crash out of the core.

### Behavior — stage 0

- [x] `Delim.collect[Int, Delim + Pure]` does not compile, and the
      message names `collecting`
- [x] the same for `delimited`/`resumable`/`run`/`reset`
- [x] `scope`/`collecting`/`pausing` still compile in a Delim row —
      the tests of delim-nesting are untouched
- [x] a generic `F[+_]` helper still compiles AND still throws (the
      limit, pinned, so that nobody mistakes the guard for a proof)
- [x] a helper that DOES take `using Delim.OneMachine[F]` propagates
      the obligation, and its call site at a Delim row is refused —
      which is the fix available to a library author today

## Stage 1 — forward instead of throw — SPIKE DONE, VERDICT POSITIVE

When a machine meets a capture naming a prompt it does not hold, it
throws. It has another option, and the spike's first finding is that
the machinery for it was already there rather than being new: the
foreign-operation path is

```scala
(g => Left(Inject(g).flatMap(x => loop(Next(okay.pure(x), kont)))))
```

— re-emit the operation into the residual program, and when the answer
arrives, resume THIS machine with the same stack. A capture belonging
to an outer machine wants exactly that, so the change is letting the
`None` branch of `split(kont, cap.prompt)` fall into that path instead
of throwing. `Delim.runNested` is the door, and it asks for
`RowLift.In[Delim, F]`: forwarding puts a `Delim` operation into `F`,
so `F` must have one — which is the case it exists for, and asking
keeps `run`'s meaning untouched.

**All three predicted properties hold** (`TestDelimForward`, 6 tests):

1. **the inner machine's frames are INSIDE the outer capture** — the
   residual program's head is the `flatMap` closure, so an outer
   `k(5)` runs the inner tail and then the outer one (measured: 106,
   from `+1` inside and `+100` outside); dropping the continuation
   skips both;
2. **multi-shot survives** — `Segs` is immutable and `loop` closes
   over nothing mutable, so an outer capture invoked twice re-enters
   the inner machine twice, independently (measured: 30 = 10 + 20);
3. **the inner delimiter is re-installed on resume** — its `Mark` is
   still in the forwarded stack, so a second capture naming the inner
   prompt finds it after the round trip.

A capture no machine can place still throws, from the OUTERMOST one,
whose installed stack is the one the user recognises.

**Cost: none.** Paired `-f 3 -prof gc` runs, master against the lane,
same box minutes apart: `delimGenerator` 89.547 ± 0.175 → 88.716 ±
0.455 µs, `delimPushOnly` 23.914 ± 0.099 → 23.833 ± 0.511 µs, and the
BYTES are identical to the digit (942 328.043 → 942 328.397;
358 040.165 → 358 040.164). Worth the run rather than the assumption:
the lane moves `run`'s whole body into a private `machine`, and a
callee crossing the inlining line re-decides every caller in this
codebase's recorded experience. It did not.

### What the verdict CHANGES, and what it does not

- **The default stays refusal.** `run` still throws and `OneMachine`
  still refuses a second machine in a concrete row, because the
  nested forms (`scope`/`collecting`/`pausing`) are strictly cheaper:
  they install a delimiter on the machine already running, where
  forwarding pays a round trip through the residual program per
  capture. A guard that teaches the cheaper spelling is worth more
  than one that silently makes the dearer one work.
- **`runNested` is for a machine you genuinely have**: a library
  function that runs its own `Delim.run` and is called inside
  somebody else's. Before this, such a function could not be called
  from a program that captures across it at all.
- **Region types are no longer needed for the nesting case.** Stage 2
  stays open only for the case it was named for: evidence that
  ESCAPES its `delimited` and is used afterwards.

## Stage 2 — region types (the horizon)

`runST`'s trick: give the evidence a scope tag that cannot escape.

```scala
def delimited[R, F[+_]](body: [S] => Prompted[R, S] ?=> R ! (Delim + F)): R ! F
```

A `Prompted[R, S]` can then only be used where `S` is in scope, which
closes BOTH open cases: the evidence cannot escape its `delimited`,
and it cannot be carried into a different machine, because a different
machine has a different `S`.

The cost is a type parameter on the evidence and therefore on every
signature that carries one — including the `direct`-block inline
doors (`shift[A]`, `exit`, `emit`, `pause`), whose whole design is
that the call site writes as few type arguments as possible. The
freer-base stage-2 probe (4af08745) already proved a prompt's identity
can reach the type level and that `NoPrompt` can be a compile error;
what it did not answer is whether the inline doors survive it.

Order as it turned out: stage 0 landed and catches the real trap;
stage 1's spike came back POSITIVE and made nested machines usable
rather than merely refused; stage 2 is now needed only for escaping
evidence, and nothing has asked for it.

### Behavior — stage 2

- [x] a prompt stored in a `var` and used after its delimiter
      returned is a compile error — ANSWERED BY ANOTHER ROAD
      (freer-base-stage2, 2026-09-23): not a region tag on `Prompted`
      but `Delim.Stacked`, where the installed prompts are a lexical
      GIVEN stack and `shift` needs `Has[stack, p.type]`; after the
      reset returns the stack in force is the outer one (TestProg 8).
      `Prompted` itself is unchanged and its escape stays a run-time
      `NoPrompt` — the typed door is beside it, not under it.
- [x] the four inline doors still take the same type arguments at a
      call site as they do today — trivially, because the stacked
      door is a separate object and touches none of them; a stacked
      `direct`-block door is not built and not asked for

## Decisions

- **The guard goes on the RUNNING combinators, not on `push`.**
  Installing a delimiter is always fine; starting a second machine is
  what is not.
- **The message names the fix.** An error that says "duplicate row"
  teaches nothing; one that says "use `collecting`" ends the
  incident.
- **No stage removes `NoPrompt`.** A multi-prompt implementation
  without a region system has a runtime error for an uninstalled
  prompt; that is true of every one in the literature. The goal is
  that no ORDINARY program can reach it.

## Out of scope

- Static prompt scoping in general (that is stage 2, and it is a
  research-grade change to the surface API).
- Making `Delim` rows nest as a row member. Rows are unions; two
  `Delim`s are one `Delim` by class, which is the root cause here and
  is a property of the row design, not of `Delim`.

## Results

**Stage 0 landed 2026-09-17** (`TestDelimSafety`, 3 tests, plus the
two pinned tests that had to change from runtime-intercept to
compile-error because the shape they demonstrated is now refused).

**The obvious formulation is refuted, and the refutation is a
compiler crash.** `NotGiven[RowLift.In[Delim, F]]` — membership as the
library already spells it — asks implicit search to prove membership
in an ABSTRACT row; `In.deeper` unfolds it into `G + H`, and dotty
3.9 dies with `java.lang.AssertionError: Failure to join alternatives
F and G` in `TypeOps.orDominator`. Not at a user's call site: at
`Delim`'s own internal ones, so the core did not compile at all.

What works is membership by APPLICATION:
`NotGiven[Delim[Any] <:< F[Any]]`. A union on the RIGHT of a `<:<`
needs no join — subtyping INTO a union is the easy direction — and
`<:<` is covariant in its second parameter, so `refl` conforms for a
concrete row and search fails quietly for an abstract one. That is
exactly the behaviour the guard wants, and it is why the witness is
also THREADED through `answer`/`replay`/`drive` rather than summoned
inside them: an obligation carried as a parameter is never searched
for at an abstract row.
