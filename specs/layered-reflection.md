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

- [ ] Two layers in one block, each `reflect` reaching its own `reify`.
- [ ] Layer order changes the answer, as transformer order does
      (Option over List against List over Option), pinned.
- [ ] Multi-shot through an inner layer (List outside Option).
- [ ] A capability used outside its `reify` fails loudly (stage 0) and
      does not compile (stage 2).

## Out of scope

- Fibers/Loom (the capabilities paper's road): one-shot and JVM-only,
  and okay's continuations are multi-shot on all three platforms
  (specs/monadic-reflection.md, Decisions).
- Answer-type-modifying reflection.

## Decisions

## Results
