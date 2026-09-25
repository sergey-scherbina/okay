# Layered monadic reflection: several monads in one direct block

## Overview

specs/monadic-reflection.md is Filinski's construction for ONE monad
(Filinski, "Representing monads", POPL 1994) on `Cont`: `reflect =
shift(k => m.flatMap(k))`, `reify = p / pure`. With one prompt there is
one monad per block. The operator asked on 2026-09-24 for the monad
STACK: `Option` and `List` (or `Either` and a foreign `Future`) in ONE
direct block, each reflected value reaching its OWN `reify`, and no
monad transformers.

## Literature, read (2026-09-24)

- Filinski, "Representing layered monads", POPL 1999: an effect-typed
  ML-like language, semantics by a series of monadic translations,
  each removing one layer. Each layer has its own reflect/reify, and a
  layer's computations may use the layers below it.
- Filinski, "Monads in action", POPL 2010: the effect-typed successor.
- Brachthäuser, Boruch-Gruszecki & Odersky, "Representing monads with
  capabilities" (2020; Scala 3, on fibers). The statement this spec
  builds on: NATIVE MULTI-PROMPT delimited control gives layered
  reflection directly. Nested `reify` calls introduce separate
  capabilities (`Reflect[M]`), and calling `reflect` on one "will
  immediately transfer control to the correct reify call". Their
  example is `reify[Option]` around `reify[Future]` around code that
  reflects both.
- Materzok & Biernacki, APLAS 2012: shift0/$ express the CPS
  hierarchy, which Danvy & Filinski introduced for layering effects.
  In λ$ Filinski's operators read `μ(m) = S0 k. m >>= k` and
  `[e] = η $ e`: reify is dollar with the unit as the return function
  (specs/shift0-dollar.md).

## What okay has

- `Delim` is multi-prompt with named prompts, and `Delim.Prompted[R]`
  is unforgeable evidence that a delimiter is installed: it is already
  the `Reflect[M]` capability of Brachthäuser et al. What is missing
  is the monad on it.
- `Monadic.reflect`/`reify` live on `Cont` (single prompt).

## Design (stage 0 decides; nothing built yet)

```scala
object Layered:
  /** the capability: a delimiter whose answer is M[_] */
  final class Reflect[M[_]] …                   // holds a Prompt and the Monad[M]
  def reify[M[_]: Monad, A, F[+_]](body: Reflect[M] ?=> A ! Delim + F): M[A] ! Delim + F
  extension [M[_], A](m: M[A]) def reflect[F[+_]](using r: Reflect[M]): A ! Delim + F
```

- `reflect` is a capture to the layer's prompt with `k => m.flatMap(k)`.
  Which capture is the question the first stage answers by
  measurement. `shift` works (the body is a pure `flatMap`, it never
  captures again), and `shift0` is the λ$ reading and re-installs the
  delimiter once instead of twice.
- The order of `reify` blocks is the order of the layers, the way the
  order of handlers is: `reify[Option](reify[List](…))` against
  `reify[List](reify[Option](…))` give different answers for the same
  body, exactly as `OptionT[List]` and `ListT[Option]` do. That is
  what the tests pin.
- `Reflect[M]` escaping its `reify` is the same hazard as `Prompted`
  escaping (backlog `delim-region-prompts`). The stacked form
  (`Delim.Stacked`, once `stacked-shift0` lands) closes it at compile
  time.

## Stages

0. **Unstacked, on today's `Delim`.** `Layered.reify`/`reflect` over
   `Prompted`, with the Option/List ordering tests and a
   `Either`-inside-`List` example. Multi-shot (List) across an inner
   layer is the case to watch: the continuation passed to `List.flatMap`
   must re-install the inner layer's delimiter on every call.
1. **`reflect` as `shift0`** (after specs/shift0-dollar.md stage 1),
   and `reify` as `dollar(p)(pure)`, measured against stage 0.
2. **Stacked**: `Reflect[M]` as a stacked prompt, escape refused at
   compile time (after stacked-shift0).
3. **Docs**: a page with the Brachthäuser example translated, plus
   literature.

## Behavior

- [x] Two layers in one block, each `reflect` reaching its own `reify`
      (and three: Either over List over Option).
- [x] Layer order changes the answer, as transformer order does
      (Option over List against List over Option), pinned.
- [x] Multi-shot through an inner layer (List outside Option).
- [x] Stage 0: a capability used outside its `reify` fails loudly
      (`NoPrompt`).
- [x] Stage 2: the same does not compile (`Layered.Stacked`,
      TestLayeredStacked).
- [x] Stage 3: the docs page — docs/direct-style.md "Layer 1½", with
      both papers, the capabilities paper's `reify[Option](reify[…](…))`
      example translated with `List` in place of their `Future` (a
      `Future` has no `Layer`, said on the page), pinned in TestLayered
      (shift0-dollar-close closed the box; the section had landed with
      layered-reflection stage 0 and direct-layers-instances).

## Out of scope

- Fibers/Loom (the capabilities paper's road): one-shot and JVM-only,
  and okay's continuations are multi-shot on all three platforms
  (specs/monadic-reflection.md, Decisions).
- Answer-type-modifying reflection.

## Decisions

- **`Layer[M]`, not `Monad[M]` (stage 0, 2026-09-24).** The design
  sketch above said `Monad`. It cannot work on `Delim`: the captured
  continuation is a PROGRAM (`X => M[R] ! G`), because it holds the
  inner layers and may reflect into outer ones, which only the running
  machine answers. `m.flatMap(k)` would need `k` to be pure. The layer
  therefore supplies `bind(m)(k: A => M[B] ! G): M[B] ! G`, a monad
  transformer over what runs outside it. Instances: Option, Either,
  List (a traversal in order). The capabilities paper can use plain
  `flatMap` only because its fibre continuation is an impure function,
  which is also why that road is one-shot.
- **`reflect` is `shift0`** on today's `Delim`, the λ$ reading. `shift`
  gives the same answers here (the body is one bind that never captures
  again), so stage 1 decides between them on price, not semantics.
- **`M` is read off the receiver**: `Some(2).reflect` finds no
  `Reflect[Some, R]`. Documented rather than worked around. A
  contravariant `Reflect[-M, R]` would fight `Layer[M]`'s invariance.

- **Stage 1: `reify` is `η $ e` (2026-09-25).** `Layered.reify` is
  `Delim.dollar(p)(r => pure(η r))(body)`, the λ$ reading, where stage
  0 had `push(p)(body.map(η))`. They mean the same thing: in both, a
  shift0 to the layer takes `η` with it. The dollar form allocates
  40 B less per resumption, because `η` is the delimiter's return
  function instead of one more `K` frame.
- **Stage 2: the layer IS the stacked dollar's `In`.** `Layered.Stacked.reify`
  is `Delim.Stacked.dollar` with `η` as the return function, and the
  capability the body receives is that dollar's `In`. So its prompt is
  the one on the stack, and `m.reflect(layer)` asks `Has` for it. A
  separate `Reflect` wrapping `d.p` would not work: `refl.prompt.type`
  and `d.p.type` are different singletons, and `Has` would never find
  the wrapper's.

STAGE 0, 2026-09-24 (TestLayered, 7, okayJVM): `Layered.reify` /
`reflect` / `Layer`, on today's multi-prompt `Delim`.

- Every expected value was written from the semantics before the run:
  List over Option gives `List(Some(11), None, Some(33))`, Option over
  List gives `None`, and Either over List over Option gives `Left("four")`
  or `Right(List(Some(1), None, Some(3)))`. The first compiling run
  matched all of them. The only red before that was the `Some`
  receiver (see Decisions).
- WATCHED FAILING: `Layer.list` with its concatenation reversed turned
  five of the seven tests red (every test with a List layer).
- docs/direct-style.md said "what one block cannot do: mix two
  different monads". That is no longer true of `Layered`, so the page
  now says it of `Monadic` only and has a Layer 1½ section.

STAGES 1-2, 2026-09-25 (TestLayered 7 + TestLayeredStacked 3; DelimBenchmark layered lanes):

- Stacked layers give the same answers as stage 0 in both orders, and a
  layer kept past its `reify` is refused with "not on the prompt stack".
  All ten tests passed on the first run.
- `reify` as `η $ e` against `push(e.map(η))`, one List layer over 1000
  elements: 682 315 against 722 354 B/op, 40 B less per resumption.
  TIME NOT MEASURED CLEANLY: the only run had load 60-108 (a VM on the
  box at ~1000% CPU), and 121 ± 37 against 243 ± 46 µs says nothing
  at that load. Backlog `layered-reify-time` re-prices it. The change
  is adopted on semantics (λ$'s form) and bytes.
  PRICED 2026-09-25 (layered-reify-time), load 3-5, one run with both
  lanes: 58.34 ± 0.35 against 64.58 ± 0.68 µs, ratio 0.90. The dollar
  form is cheaper on time as well as on bytes.

OKAY2 TWIN, 2026-09-25 (okay2-layered, specs/okay2.md stage 47):
`Layer`, `Reflect`, `reify` (η $ e) and `reflect` (shift0) in the Scala 2
core, every value of TestLayered reproduced; the body receives its
capability instead of summoning it; the stacked layers followed in
okay2-lexical-walk-stacked (`Layered.Stacked.reify`, `m.reflectAt`).
