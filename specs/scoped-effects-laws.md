# scoped-effects-laws — `recover`×`State` order, and `Reader.local`

## Overview

Scoped (higher-order) operations — an operation whose ARGUMENT is
itself a program, not a value — raise a question algebraic operations
never do: when a scoped operation's body performs OTHER effects, does
the scope apply to those too? Wu, Schrijvers & Hinze, "Effect
handlers in scope" (Haskell 2014), ask it about `catch`/`local`
exactly: does a `State` write made inside a `catch`ed block survive
the catch, or roll back with it? The answer is not a law of nature —
it is a design choice, and this library had made it implicitly
(`Throws.recover` existed, `Reader.local` did not) without ever
stating it. This spec states it, tests it, and fills the gap.

Three findings, from reading `Effects.handle`'s actual loop and from
tests that were run and watched fail before being believed:

1. **`recover`×`State` is decided already, by `recover`'s own
   definition — GLOBAL by default, scoped on request.** `recover`
   peels off only `Throws % E`'s own operations, forwarding
   everything else (`State`'s included) UNCHANGED via
   `Effects.handle`'s forwarding arm. A `State` mutation made before
   a caught `raise` therefore already happened — `recover` cannot
   roll it back, because nothing buffered it to roll back. Where the
   caller wants GLOBAL, ambient state (a retry counter that survives
   the retry) that is `recover(State.run(s0)(guarded))` — where they
   want a SCOPED, transactional attempt (any writes inside undone on
   failure), that is `State.run` applied INSIDE the guarded scope
   instead of outside it. Both are one line away from each other and
   answer different questions; neither is more "correct". TestScopedEffects
   pins both, plus the explicit save/restore idiom for a
   transactional retry over GLOBAL state (one line each side).
2. **Nesting `local` composes INSIDE-OUT, not outside-in — a genuinely
   surprising result, first written down wrong and corrected against
   a failing test.** The naive guess, matching mtl's `local`, is that
   `local(f2)(local(f1)(p))`'s asks see `f2(f1(r))` — the OUTER
   function wrapped around the inner's result. Measured: they see
   `f1(f2(r))` instead. The mechanism is that `local`'s OWN "find the
   ambient r" step is implemented as an ORDINARY `Reader.ask` — and
   this row's own class-keyed typing makes that ask INDISTINGUISHABLE
   from any ask a user writes. So an outer `local`'s handler, walking
   the inner `local`'s output, intercepts the inner's OWN bookkeeping
   ask too, not only whatever the user's program asks — the inner
   local ends up computing `f1(f2(ambient))`, and `f1` is applied
   LAST. mtl's `local` never meets this, because there the "current
   environment" is the INTERPRETER's own hidden state, never a
   program-visible operation a handler could intercept; here `local`
   is built FROM the very vocabulary it overrides, and that is the
   whole difference. Not a bug to patch casually — fixing it needs a
   design that gives each `local` a private, untouchable identity for
   its own bookkeeping (a prompt, the way `Delim.Stacked` gives one to
   each `reset`), which is `effect-instances-tunnelling`'s scope, not
   this one's.
3. **A generic scoped handler cannot see inside another effect's
   OPAQUE PAYLOAD, and `Delim` is exactly built that way — a more
   basic limit than the multi-shot dynamic-binding one the same
   literature warns about, and the one this spec first (wrongly)
   claimed did not apply.** `Effects.handle`'s forwarding arm DOES
   correctly re-wrap an ordinary Bind's own continuation with itself,
   recursively — which is why `local` composes across a plain
   `flatMap` chain, and why `Delim.Capture`'s continuation, reached
   via handle's own Bind-processing, WOULD carry the override if
   invoked through that path. But `Delim.push(prompt)(body)` builds
   `Inject(Push(prompt, body))`, with `body` — a WHOLE Free program —
   a PAYLOAD FIELD of the operation, not the Bind's own attached
   function. A generic handler (`handle` is generic over `G`, and must
   be, to forward ANY effect without knowing its shape) treats a
   forwarded operation as opaque data; it cannot reach into a value
   carried as a FIELD of that operation, only into what follows it
   via an ordinary Bind. Measured directly: `local(_*10)` wrapped
   around `Delim.push(prompt)(body)` where `body` performs two asks
   inside a captured continuation's own re-invocations answers as if
   `local` were never applied at all — the true ambient, unaffected.
   Kiselyov, Shan & Sabry's "delimited dynamic binding" (ICFP 2006) is
   about a DIFFERENT, narrower case (a continuation invoked from
   somewhere its capturing scope does not reach); this is the coarser
   fact that a program hidden inside ANY effect's payload — Delim's
   `push`/`shift`, but the shape recurs anywhere a program carries a
   sub-program as data — is invisible to a forwarding handler, full
   stop.

The genuinely POSITIVE, verified case: `local` composes correctly
across ordinary composition inside `p` — a `flatMap` chain, nesting
of `local` itself (with the inside-out law above), and any effect
`p` performs besides `Reader % R` (forwarded transparently, State
alongside it in the same program).

## Interface

```scala
def local[R, A, F[+_]](f: R => R)(p: A ! Reader % R + F): A ! Reader % R + F
```

`local(f)(p)`: `p`'s own `Reader.ask`s answer `f(r)` where `r` is
whatever the AMBIENT `ask` would answer — asked ONCE per `local` call,
via an ordinary `Reader.ask` embedded in `local`'s own output (which
is exactly what makes finding 2 true: this bookkeeping ask is not
protected from an enclosing `local`). Everything `p` performs besides
`Reader % R` forwards unchanged, EXCEPT what is buried inside another
effect's own opaque operation payload (finding 3) — most notably
anything inside a `Delim.push`/`shift` body reached that way.

`Throws.recover` is UNCHANGED — this spec adds no interface to it,
only the laws.

## Behavior

- [x] `recover`'s handler runs in the state left by the failed
      attempt, not a snapshot from before the guarded block: a
      `State.set` before a caught `raise` is visible to the recovery
      handler and survives past it
- [x] the explicit save/restore idiom (`State.get` before, `State.set`
      inside the recovery handler) gives a caller GLOBAL state a
      transactional retry without a new primitive
- [x] `State.run` applied INSIDE a `recover`ed scope isolates that
      scope's writes completely: a raise inside it leaves the OUTER
      state exactly as `recover` found it
- [x] `local` overrides every `ask` reachable by ordinary composition
      inside `p`, including across a `map`/`flatMap` chain and a
      forwarded effect's own continuation (State alongside Reader)
- [x] nesting: `local(f2)(local(f1)(p))`'s asks see `f1(f2(r))` — FOUND
      inside-out, not the outside-in guess a first draft assumed;
      asking OUTSIDE either `local` still sees the ambient `r`
- [x] THE FIRST DOCUMENTED LIMIT: `local` does not reach inside an
      OPAQUE payload — demonstrated against `Delim.push`'s `body`
      field, where two asks inside a captured continuation's own
      re-invocations answer the true ambient, `local`'s override never
      applied
- [x] THE SECOND DOCUMENTED LIMIT: a Reader program VALUE carries no
      memory of where it was built — `local` wraps whoever hands it a
      tree, regardless of that tree's own textual origin (the
      mechanical fact behind Kiselyov-Shan-Sabry's dynamic-binding
      warning, demonstrated without needing `Delim` at all)

## Out of scope

- **Giving `local` a private identity for its own bookkeeping ask**
  (finding 2's fix) — the same shape `Delim.Stacked` already solved
  for prompts (a fresh identity per `reset`, checked by a stack). It
  would make nesting compose the mtl-expected way and is a real
  design change to `local`'s signature; `effect-instances-tunnelling`
  (backlog) is where it belongs if a consumer ever needs correct
  nesting rather than documented surprise.
- **Reaching inside an opaque payload** (finding 3) is not a bug to
  patch in `local` or `recover` — it is what "generic over G" MEANS,
  and the fix, if one is ever wanted, is on `Delim`'s side: a shift/
  push whose body is reached via an ordinary Bind rather than a
  payload field, which changes `Delim`'s own shape and is far outside
  this spec.
- **A general `Scoped[F]` typeclass** unifying `recover`/`local`/
  `Logic.cut` under one interface (Piróg, Schrijvers, Wu & Jaskelioff,
  LICS 2018; Bach Poulsen & van der Rest, "Hefty algebras", POPL 2023)
  — each of the three already has a hand-written, efficient
  definition over `Effects.handle`; a shared abstraction is a second
  lane if a fourth scoped operation is ever added and the duplication
  starts to hurt.

## Decisions

- **`local` asks the ambient environment ONCE PER CALL, not per-ask
  — and that single ask is an ORDINARY operation, which is exactly
  what makes the nesting law what it is (finding 2).** A Reader's
  environment is a single value for the whole run; `local`'s override
  is a pure function of THAT value, applied once. Making it anything
  more protected (a private prompt) is the `effect-instances-
  tunnelling` lane, not this one.
- **The forwarding arm both laws lean on is itself a law now
  (row-parametricity-forwarding-law, 2026-09-25).** "Forwarded
  UNCHANGED" and "forwarded transparently" above were assumed from the
  free theorem (Biernacki, Piróg, Polesiuk & Sieczkowski, POPL 2018)
  and never tested directly. TestRowForwarding checks it as a `Bisim`
  law against a reference that answers the handled signature in place
  and re-emits everything else (specs/row-parametricity-forwarding-law.md);
  it caught `Writer.collect` splitting a `byValue` row on the wrong test
  the day it was written, so it is not a formality.
- **`local` is built on `Effects[Free].handle`, the same tool
  `recover` uses**, not a bespoke loop: the two are the same shape
  (peel one operation's own signature, forward the rest). The GADT
  refinement `Ask(): Reader[R, R]` (fixing the answer type to `R`)
  had to move into a private METHOD rather than sit inside the
  `[X] => ...` polymorphic function literal `handle` wants — the same
  trap `SharedOnce.answer` met (gadt-on-a-covariant-enum) — because
  `Throws`'s own handler never needed this (a case CLASS with one
  shape, no GADT refinement to lose).

## Results

Landed as written: `Reader.local` (src/main/scala/Reader.scala),
TestScopedEffects (10 tests, all green), this spec. Two of the three
findings above were WRONG in the first draft and corrected only after
running the tests and reading the actual failures — a fact this repo's
own memory already carries a name for (the-record-outlives-the-truth):
a claim about composition, however carefully reasoned from the source,
is not verified until a test disagrees with it and loses.
