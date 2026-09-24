# shift0 and dollar ($): the dynamic delimiter, done properly

## Overview

`Delim` already has `shift0` and `control0` (one `Capture` operation with
two flags, specs/delimited-control.md "The family"). Two things are
missing, and the operator asked for both on 2026-09-24:

1. **`$` (dollar).** Materzok & Biernacki, "A dynamic interpretation of
   the CPS hierarchy" (APLAS 2012), make `e1 $ e2` the primitive
   delimiter of λ$: evaluate `e1` to `v`, run `e2` in a new context,
   and when it returns `x`, remove the delimiter and call `v x`.
   `reset0 e` is `(λx.x) $ e`. The point is NOT sugar for
   `push(p)(e).flatMap(v)`: the context a `shift0` captures is
   `v $ K`, so it CONTAINS `v`. The contraction rules are
   ```
   (λx.e) v   ⇝ e[v/x]                 (βv)
   v' $ v     ⇝ v' v                   ($v)
   K̂[S0 f.e]  ⇝ e[λx.K̂[x]/f]           ($/S0)   where K̂ = v $ K
   ```
   So `v $ E[S0 f. e]` reduces to `e` with `f = λx. v $ E[x]`: when
   `e` drops `f`, `v` is never applied, and when `e` calls `f` twice,
   `v` is applied twice. `push(p)(E[…]).flatMap(v)` applies `v` exactly
   once, to whatever `e` returned. The two differ observably whenever a
   0-capture does not resume exactly once.
2. **`shift0`/`control0` in `Delim.Stacked`** (backlog `stacked-shift0`):
   the body runs with the delimiter consumed, so its prompt stack is
   the part below `p`, which is a match type `Below[S, P]` the stacked
   door does not have.

WHY IT MATTERS BEYOND COMPLETENESS. Piróg, Polesiuk & Sieczkowski,
"Typed equivalence of effect handlers and delimited control" (FSCD
2019) show DEEP handlers and shift0 are inter-definable in a typed
setting, and SHALLOW handlers and control0 likewise. In that
correspondence `$` is the handler's RETURN CLAUSE, and `shift0` is
performing an operation. okay's handlers are continuations already
(`F !> S`), so `$` is the missing piece that relates `Delim` and
`Handler` inside one machine. `Bisim.check` (specs/handler-equivalence-
oracle.md) is the tool that checks the relation.

## Literature, read (2026-09-24)

- Materzok & Biernacki, "Subtyping delimited continuations", ICFP 2011.
  The type-and-effect system for shift0/reset0: the effect annotation
  is a stack of contexts, `σ ::= ε | [τ σ] τ σ`, each entry one context
  with its own answer-type change. The shift0 rule:
  ```
  Γ, f: τ1 →σ1 τ2 ⊢ e : τ3 σ2
  ─────────────────────────────
  Γ ⊢ S0 f.e : τ1 [τ2 σ1] τ3 σ2
  ```
  types the body under the stack with the top context removed.
  Subtyping `ε ≤ [τ σ] τ σ` lifts a pure term under any stack whose
  contexts compose. Proved: strong type soundness, termination, and
  a correct type-reconstruction algorithm. Also a SELECTIVE CPS
  translation that leaves pure terms in direct style.
- Materzok & Biernacki, APLAS 2012: λ$, its reduction semantics,
  abstract machine, CPS (`e1 $ e2 = λk. e1 (λv1. e2 v1 k)`: the pushed
  value is literally the continuation), and a translation of the CPS
  hierarchy `shift_i/reset_i` into shift0/$, proved sound w.r.t. CPS,
  types, reduction and machines, all checked in Twelf. Also the
  macro-expression of `$` by `reset0` and `shift0`:
  `e1 $ e2 = (λk. ⟨(λx. S0 z. k x) e2⟩) e1`.
- Piróg, Polesiuk & Sieczkowski, FSCD 2019 (above), with a Coq
  formalisation.
- Biernacki, Pyzik & Sieczkowski, "Reflecting stacked continuations in
  a fine-grained direct-style reduction theory", PPDP 2021: a
  direct-style reduction theory for shift0/reset0. For reasoning about
  rewrites, not needed to build the operators.

## What okay has today (read from Delim.scala)

- The machine's continuation is `Segs`: `K(f, rest)` frames and
  `Mark(p, rest)` delimiters, innermost first. `split(kont, p)` cuts at
  the first `Mark(p)`. `Capture(p, f, underPrompt, delimitK)` covers
  the four operators.
- `Mark` carries no return function. A delimiter returns its body's
  value unchanged, and whatever follows the delimiter is an ordinary
  `K` frame OUTSIDE it, which a 0-capture does not take.
- `Prompt[R]` fixes the delimiter's answer type `R`, for the body and
  for every capture to it.
- `Delim.Stacked` has `shift`, `control`, `abort`. Its header records
  why not `shift0`/`control0`: `Below` was never exercised.

## Design (to be settled by stage 0; nothing below is built yet)

```scala
// unstacked door
def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Delim + F)(body: R0 ! Delim + F): R ! Delim + F
// reset0 is dollar with ret = pure: push(p)(body) stays as it is
```

- The machine: `Mark` gains the return function, or a sibling case
  `Ret(p, ret, rest)` keeps `Mark` as it is (the push-only lane in
  DelimBenchmark prices the difference). A value reaching a `Ret`
  leaves the delimiter and continues with `ret(x)` under `rest`. A
  0-capture to `p` takes `Ret` INTO the captured segment, which is
  the `$/S0` rule, and `reify` re-installs it as a `Ret`.
- Types: the prompt carries the OUTER answer `R` (what the delimited
  expression answers, what `f` in a 0-capture must answer and what `k`
  returns). The body answers `R0`. For the under-prompt captures
  (`shift`, `control`) to a `$` delimiter, `f`'s body runs inside the
  delimiter, so it would have to answer `R0`, while `k` answers `R`.
  OPEN: allow only the 0-captures to a `$` prompt (a separate prompt
  type), or require `R0 = R` for under-prompt captures. Stage 0
  answers this with the literature's semantics for `S` under `$`
  (APLAS 2012 builds shift from shift0 and `$`), not by guessing.
- Stacked: `Below[S <: Tuple, P]` drops THROUGH `P` (our prompts are
  named, ICFP 2011's contexts are positional), and
  `shift0`/`control0` type `f` under `Under[F, R, Below[st.S, p.type]]`.
  `dollar` in Stacked pushes onto the stack like `reset`.

## Stages

0. **Probe (tests only, no machine change).** Pin today's semantics
   and state the difference: the ICFP 2011 example (`⟨"Alice" ++
   ⟨" has " ++ (S0 k1. S0 k2. "A cat" ++ k1 (k2 "."))⟩⟩` evaluates to
   "A cat has Alice."), and a `$` written as `push(…).flatMap(v)`
   against the λ$ rules for a 0-capture that drops `k` and one that
   calls it twice. The `$` tests are EXPECTED to fail against the
   `flatMap` encoding, and are watched failing. Also decides the open
   typing question above.
1. **`Delim.dollar`** in the machine, unstacked door. Laws as tests:
   `($v)`, `($/S0)` including `v`, `reset0 = pure $`, and APLAS's
   macro-expression of `$` via `reset0`/`shift0` checked with
   `Bisim.check` against the primitive. DelimBenchmark push-only and
   generator lanes before and after (performance skill).
2. **Stacked `shift0`/`control0`/`dollar`** with `Below`. Tests: a
   shift to the consumed prompt inside the body is REFUSED at compile
   time, and a shift to an outer one resolves.
3. **Handlers as `$` + `shift0`**: one deep handler (State) written as
   `dollar` + `shift0` checked against `State.handle` with
   `Bisim.check`, and one shallow handler via `control0`. This is the
   FSCD 2019 correspondence made executable. The verdict decides
   whether anything in Handler.scala should use it, and the price is
   measured before anything is adopted.
4. **The CPS hierarchy** via APLAS 2012's translation, as examples in
   the docs, if stage 3 shows a use.

## Behavior

- [ ] Stage 0: the ICFP 2011 "A cat has Alice." example runs on
      today's `shift0`.
- [ ] Stage 0: `push(p)(e).flatMap(v)` differs from λ$'s `v $ e` for a
      0-capture that drops `k` and for one that calls it twice
      (watched failing, then kept as the spec of stage 1).
- [ ] Stage 1: `($v)`, `($/S0)` with `v` captured, `reset0 = pure $`.
- [ ] Stage 1: `$` via `reset0`/`shift0` (APLAS 2012) is
      `Bisim`-equivalent to the primitive.
- [ ] Stage 2: stacked `shift0`/`control0`/`dollar`, a consumed prompt
      refused at compile time.
- [ ] Stage 3: a deep State handler as `$` + `shift0` is
      `Bisim`-equivalent to `State.handle`.

## Out of scope

- Answer-type change PER CONTEXT (ICFP 2011 allows it, `Prompt[R]`
  fixes `R`). It is not needed by anything above.
- Type reconstruction: Scala infers, and the stacked door already
  spells its type arguments.

## Decisions

## Results
