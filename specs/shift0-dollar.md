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
  ANSWERED by stage 0 (see Decisions): an under-prompt capture's body
  runs under a PLAIN delimiter (identity return), not under `v`, so it
  answers `R` like `k` does, and `R0` never reaches it.
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

- [x] Stage 0: the ICFP 2011 "A cat has Alice." example runs on
      today's `shift0` (TestDollarProbe).
- [x] Stage 0: `push(p)(e).flatMap(v)` differs from λ$'s `v $ e` for a
      0-capture that drops `k` and for one that calls it twice, and
      APLAS 2012's macro-expression on today's operators does not
      (watched failing, see Results).
- [x] Stage 1: `($v)`, `($/S0)` with `v` captured, `reset0 = pure $`
      (TestDollar), and `k` re-installs the dollar (a second shift0
      inside the continuation is caught by it).
- [x] Stage 1: `$` via `reset0`/`shift0` (APLAS 2012, `dollarMacro` in
      TestDollarProbe) agrees with the primitive on six bodies (returns,
      drops k, k twice, k once, shift under, abort). They are compared
      as values after `Delim.run`. NOTE from stage 0:
      `Bisim.check` compares operations with `==`, and a `Delim`
      operation carries a function, so compare AFTER `Delim.run`, on
      the residual row.
- [x] Stage 2: stacked `shift0` and `dollar` (not `control0`, see
      Decisions), a consumed prompt refused at compile time
      (TestStackedShift0).
- [x] Stage 3: a deep State handler as `$` + `shift0` is
      `Bisim`-equivalent to `State.handle`, and so is a shallow one as
      `control0` (TestHandlersAsDollar).
- [x] dollar-doors: the evidence door `Delim.dollar(ret) { body }`
      (TestDollar), a stacked `control` to a `dollar` refused at compile
      time and allowed to a `Reset` (TestStackedShift0),
      `!Delim.shift0[A]` in a direct block (TestDirectShift0), and
      `abort` to a dollar skipping `ret` pinned as a value (TestDollar).

## Out of scope

- Answer-type change PER CONTEXT (ICFP 2011 allows it, `Prompt[R]`
  fixes `R`). It is not needed by anything above.
- Type reconstruction: Scala infers, and the stacked door already
  spells its type arguments.

## Decisions

- **`$` is the primitive, as in λ$ (stage 1, 2026-09-24).** The
  operator pointed out that APLAS 2012 has no reset0 at all: `⟨e⟩`
  is sugar for `(λx.x) $ e`, and the machine, the CPS translation and
  the hierarchy's translation are all built on `$`. So `Delim.Dollar`
  is an operation of the machine, and its frame is `Segs.Ret(p, ret,
  rest)` (from the body's `R0` to the prompt's `R`). `push` KEEPS its
  own plain `Mark`: it is `dollar(p)(pure)`, pinned by the law
  "reset0 = pure $" on six bodies, and the plain frame saves a call
  to an identity function per delimiter on the hot lane. That is a
  representation choice, not a second semantics.
- **The cut carries the delimiter.** `split` now returns the captured
  chain, the delimiter itself (`close`: the `Mark` or the `Ret` with
  its function) and whether it was plain (`P0 =:= P`). A shift/shift0
  continuation is `captured` then `close`, so a `Ret` brings `ret`
  along, which is the `$/S0` rule. For a plain mark this is exactly
  the old `Push(p, seg)`. The existential `P0` is a type MEMBER of
  `Cut`, so `captured` and `close` stay linked through one stable
  value, and no cast was added.
- **Control-captures to a `dollar` are refused at run time.** Their
  bare continuation answers the body's `R0`, and `Capture` types `k`
  at the prompt's `R`. Typed shallow handlers (FSCD 2019: shallow
  handlers correspond to control0) need `k: A => R0`, which is stage
  3's question. Until then the refusal names the prompt and the spec.

- **Under-prompt captures to a `$` delimiter (stage 0, 2026-09-24).**
  APLAS 2012 builds `S k.e` as `S0 k.⟨e⟩`: the body runs under a FRESH
  reset0, which is `(λx.x) $ …`, not under the captured `v`, while `k`
  (= `λx. v $ E[x]`) carries `v`. So the body answers the outer `R`,
  as `k` does, and `R0` stays the body type of `dollar` alone. Our
  machine's `underPrompt` already re-installs a PLAIN `Mark(p)` for
  `f`'s body, which is this rule, so no prompt kind is needed and
  `Prompt[R]` stays as it is. Pinned: "shift under $" in
  TestDollarProbe, on both `shift` and its `S0 k.⟨e⟩` spelling.

- **Stage 2's typing (2026-09-25).** `Below` is a type MEMBER of the
  `Has` evidence (`Has.Aux[S, P, B]`), found by the same induction that
  finds the prompt. A match type `Below[S, P]` cannot reduce: two
  prompts' singleton types are not provably disjoint. `shift` and
  `control` bodies run under `p *: B`, and a `shift0` body under `B`,
  each with that stack as a GIVEN of its own (a context-function
  parameter of `f`). The existing `shift`/`control` typed the body under
  the whole stack, which was unsound (Results). `control0` is left
  unstacked: its bare continuation runs where `p` is gone, but its
  code was typed with `p` present, and a per-continuation requirement
  is not something this index can carry. The index is conservative
  where ICFP 2011 is not. A continuation captured under `p` is typed as
  needing `p` even when its code never captures to `p`, so the paper's
  own example is refused (pinned in the tests).

- **Stage 3's verdict: nothing in Handler.scala adopts it
  (2026-09-25).** Both encodings are exactly `State.handle` by
  `Bisim`, and they cost 3.8x and 4.7x the time and 7.4x and 7.8x the
  bytes. A handler's own loop answers an operation in place. The
  encodings build a state-passing function per operation, capture a
  segment, and run it through the machine. The correspondence is kept
  as what it is good for: a reference semantics, and an oracle
  (TestHandlersAsDollar) that any handler can be checked against.
- **A shallow handler does not need a typed control0-to-`$`.** Stage 1
  refused control-captures to a dollar because the bare continuation
  answers the body's type. The shallow State handler avoids that. Its
  return clause rides INSIDE a plain `push` as a `map`, so the bare
  segment already answers the handler's type, and the clause re-installs
  the handler around `k`. So the refusal costs no expressiveness that
  stage 3 needed.
- **A dollar can be told when it is resumed (lexical-tail-guard-abort,
  2026-09-25).** `Delim.dollarResumed(p)(ret, resumed)(body)`: the
  machine calls `resumed(n)` when it enters the delimiter, `n` the
  number of runs of ONE captured context that took it (fresh per
  capture, `Delim.Shots`; 1 at the `dollar` call itself). `ret` runs on
  a normal return, and only then; a resumption that leaves by `abort`
  runs no `ret`, which is the `$/S0` rule and is exactly what
  `Lexical.tail`'s guard could not see. The plain `dollar` carries a
  null count and the machine pays one null test per `Dollar` step; a
  `Mark` and the plain cut are untouched (delimGenerator byte-identical,
  Results). The λ$ operator is unchanged: this is instrumentation of
  the delimiter frame, not a new reduction rule.
- **Compile time where the arc promised it (dollar-doors, 2026-09-25).**
  Three doors the arc left at the raw-prompt level or at run time:
  (1) `Delim.dollar(ret) { body }` with `Prompted[R]` in scope, the
  same word as the primitive told apart by the first clause (the
  `shift` rule), so the recommended `scope`/`delimited` spelling can
  write a `$` without a prompt in hand; (2) `Delim.Stacked.control` asks
  for `Plain[p.type]`, which only a `Reset` (what `reset`/`delimited`
  hand their body) provides — a `dollar`, a `Layered` layer and a
  `Lexical` instance hand a bare `In`, so a control-capture to any of
  them is now refused by the compiler with the reason in the message,
  where the machine used to throw; `Prompt` is final, so the kind
  lives on the evidence the stack hands out, not on the prompt; (3)
  `!Delim.shift0[A]` in a direct block, the inline mirror of `shift`.
  Also pinned as a value: `abort` to a dollar answers the aborted
  value with no `ret` (it is `$/S0` with `f` ignoring its argument),
  where the `flatMap` encoding would wrap it.

STAGE 0, 2026-09-24 (TestDollarProbe, 5 tests, okayJVM):

- Every outcome was predicted by hand from the reduction rules before
  the first run, and the first run matched all five.
- The ICFP 2011 example gives "A cat has Alice." on today's `shift0`,
  with ONE prompt pushed twice. Named prompts pushed twice reproduce
  the paper's positional contexts.
- `dollarMacro` (APLAS's `e1 $ e2 = (λk. ⟨(λx. S0 z. k x) e2⟩) e1`,
  written as `push(p)(e.flatMap(x => shift0(p)(_ => v(x))))`) obeys
  `($v)` and `($/S0)`: dropped k gives "dropped" (v never applied), and
  k called twice gives "<a!><b!>" (v per call).
- `push(p)(e).flatMap(v)` gives "<dropped>" and "<a!b!>": `v` sits
  outside what the 0-capture takes.
- WATCHED FAILING: with `dollarMacro`'s body replaced by the flatMap
  encoding, exactly the three λ$ tests went red (dropped, twice, shift
  under $) and `($v)` stayed green.
- What this means for stage 1: `$` is already EXPRESSIBLE today, at
  the price of an extra delimiter and an extra capture per `$`. The
  primitive earns its place only if it is cheaper or simpler to type,
  and stage 1 measures the macro against it before adopting it.

STAGE 1, 2026-09-24/25 (TestDollar 11; DelimBenchmark, history.tsv):

- Every expected value in TestDollar was worked by hand from the λ$
  rules before the first run, including `"A cat{ has [Alice.]}"` for
  ICFP 2011's example with a return function on each dollar and
  `"n=10|n=20"` for `R0 = Int`, `R = String`. The first run matched
  all of them.
- A GAP IN THE FIRST TEST LIST, found by a mutant: no test required
  `k` to RE-INSTALL the dollar. A mutant that reified a `Ret` as a plain
  `flatMap(ret)` passed all ten tests. The test "k RE-INSTALLS the
  dollar" was added (a second shift0 inside the continuation), and the
  mutant then failed it with `NoPrompt`.
- Primitive against the stage-0 macro, gated at load < 4, 3 forks:
  nothing captured 24.08 against 38.81 µs (1.61x) and 372 against 548
  B per dollar; one shift0 per dollar 48.46 against 68.59 µs (1.42x)
  and 724 against 956 B. The macro pays a capture per `$` even when
  the body captures nothing. The primitive earns its place.
- `dollar` against `push`: 24.08 against 23.38 µs (1.03), +14 B per
  delimiter (the return function). `push` keeps its plain `Mark`.
- THE CAPTURE PATH, and what the first cut cost. One `Cut` shape for
  both kinds of delimiter (the delimiter as a frame, plainness as
  evidence) cost delimGenerator, which never meets a dollar, +104 B
  per capture (910 329 to 1 014 330 B/op) and 7-9%. Two shapes
  (`Plain` = the old cut, `AtRet` = a dollar's) brought the bytes to
  +32. Returning `NotFound` instead of an `Option` per frame brought
  them to exactly the baseline (910 330). The time was still 1.023 to
  1.028 over three gated rounds. Moving the `Dollar` case after
  `Capture` in the step, and matching `Plain | AtRet | NotFound`
  directly, gave 1.002, 1.012 and 1.044 (±2.5 on the last). That is
  within the noise in two of three rounds, and is recorded as ≤ 2%,
  not as zero.

STAGE 2, 2026-09-25 (TestStackedShift0 8, TestProg 12 unchanged):

- FOUND FIRST: the stacked `shift` typed its body under the WHOLE
  stack. ProbeStackedHole shifted to an outer prompt and, from the
  body, to the inner prompt the capture had just taken. It COMPILED and
  threw `NoPrompt` while the program was being built, which is the
  exact failure `Delim.Stacked` exists to make impossible. It is now a
  compile error ("CLOSED" test).
- Values worked by hand, all matched on the first run: 22 (shift body
  to a prompt below), 202 (shift0 body to the outer prompt), 30 (shift0
  at the root, k twice), "n=10|n=20" (stacked dollar, R0 ≠ R).
- WATCHED FAILING: a mutant `Has.here` whose `Below` keeps the prompt
  turned "a shift to the CONSUMED prompt" green-to-red. The ICFP refusal
  was read and is the expected one (`Found: (k1 : String => Under[…,
  (p1.p)])`), not an unrelated error. The test asserts on that.
- TestProg's twelve tests needed no change: a `k => …` lambda adapts to
  the new context-function body.

STAGE 3, 2026-09-25 (TestHandlersAsDollar 5; DelimBenchmark state* lanes):

- The deep encoding (`ret $ body`, State operations rewritten into
  shift0 clauses over a state-passing answer) and the shallow one
  (control0, handler re-installed around k) are both `Same(1, 0)`
  against `State.handle` on a six-step counter and a 20-step loop,
  observed on the residual Writer row. Values `(22, 12)` with tells
  `a=1, b=11`. 10 000 operations run through each in constant stack.
- MUTANT: a deep Set clause that passes the OLD state to the rest
  gives "Differ after Say(a=1) -> (): left performed Say(b=1), right
  performed Say(b=11)". The oracle names the first observable
  difference.
- PRICE, one run (load 3.8 at start, 15 at end), 1000 get/set pairs:
  `State.handle` 35.0 µs / 222 040 B, deep 133.9 µs / 1 632 867 B
  (3.8x / 7.4x), shallow 164.5 µs / 1 729 014 B (4.7x / 7.8x). The
  bytes do not depend on the load, and a 4x time gap is well outside
  what the load could produce.
