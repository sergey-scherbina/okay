# Staged effects: carrier-generic fold and runIn

## Overview

Effects staging, completing the deferred decision of staged-tagless:
the missing piece was that `foldCont` fixed the interpretation carrier
to `Cont`, so every effect program ran through the middle tree. This
spec adds the carrier-generic fold — `foldIn[C: Control, S]` — and the
derived `runIn[C]`: the same effect program runs stack-safely at `Cont`
or staged at `Func`, where the handler fuses into plain closures and no
Cont tree is materialized between the program and its answer.

## Interface

- `type Interp[F[_], C[_, _, _], S] = F ==> ([X] =>> C[X, S, S])` — an
  interpretation of F into any Control carrier; `F !> S` is now defined
  as `Interp[F, Cont, S]` (the same type as before, verbatim).
- `Effects` trait gains: abstract
  `foldIn[C[_, _, _], S](h: Interp[F, C, S])(using Control[C]): C[A, S, S]`,
  derived `runIn[C[_, _, _]](using Handler[F], Control[C]): A`, and the
  comonadic adapter `handlerIn[C, F, S]`.
- `transparent inline def stagedEffects[M[_[+_], _]]: Effects[M]` — the
  staging entry, as `staged` is for Control.
- Everything else is untouched: `foldCont`, `runWith`, `handle`,
  `fromFree`/`toEff`/`reify` keep their signatures and semantics.

## Behavior

- [x] One inline effect program agrees across encodings and carriers:
      `sprog[Free].runIn[Cont] == sprog[Free].runIn[Func] ==
      sprog[Eff].runWith == sprog[Eff].runIn[Func]`
      (TestEffects "staged effects").
- [ ] The payoff is measured: a statically-unrolled 24-op Produce chain
      at `runIn[Cont]` vs `runIn[Func]` (StagedBenchmark), medians in
      src/jmh/history.tsv. Prediction, written before measuring:
      Func ≥ 1.3x faster (both build the same Free tree; the difference
      is the middle Cont tree plus its interpretive walk vs direct
      closures); below 1.15x the claim is weak and must be revisited.
- [x] All existing tests stay green (21).

## Design

foldCont stays abstract next to foldIn rather than becoming
`foldIn[Cont]`: each encoding keeps its specialized fast path (Free — a
Cont-specific fold with no Control dispatch; Eff — direct application
`m[S](h)`), and the derived machinery (runWith, handle, reify, toEff)
keeps running on those paths unchanged.

Eff.foldIn reifies first: the final encoding committed its observation
carrier to Cont when it was defined, so changing carriers goes through
the tree (documented at the override). The zero-tree alternative — a
carrier-polymorphic final encoding
`[C[_, _, _], S] => Interp[F, C, S] => Control[C] ?=> C[A, S, S]` — is
possible and would make foldIn a plain application, at the price of
changing Eff's type (and relying on higher-kinded binders in
polymorphic function types); deferred.

runIn inherits the carrier choice rule: Cont is stack-safe on any bind
shape, Func is fused but not stack-safe — for deep programs use
runWith/runIn[Cont].

## Decisions

- **`Interp` as a separate 3-parameter type, `!>` redefined as its Cont
  case** — call sites keep reading `F !> S`; no churn.
- **Free keeps its specialized foldCont** — the 1M runWith stress path
  stays free of Control-dispatch; foldIn is the generic twin.
- **Eff's type unchanged** — the 2013 continuation-encoding story and
  the simple `[S] => F !> S => A /> S` stay; carrier-polymorphic Eff is
  a separate future design.

## Out of scope

- Carrier-polymorphic Eff (see Design).
- Expr/quotes staging — same wall as staged-tagless: raw host functions
  in the signatures; a representation-abstracted (Symantics-style)
  layer is a separate design.
- Any change to existing semantics.

## Results

To be filled after the benchmark: medians and the ratio, also appended
to src/jmh/history.tsv.
