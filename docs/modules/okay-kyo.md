# okay-kyo

> Value bridges plus the STRUCTURAL effect-row mapping: kyo's
> ArrowEffect shapes match okay's operation-with-typed-answer arrow
> for arrow, so effects translate operation by operation — multi-shot
> included.

Depends on: `okay` (JVM), kyo-core.

## Guide

**The anatomy match.** kyo's effects are `ArrowEffect[Input, Output]`
pairs; okay's are operations `F[A]` whose type parameter IS the
answer. They line up structurally:

| kyo | okay |
|---|---|
| `Emit[V] = ArrowEffect[Const[V], Const[Unit]]` | `Writer % V` (tell) |
| `Abort[E] = ArrowEffect[Const[Error[E]], Const[Unit]]` | `Throws % E` (raise) |
| `Choice = ArrowEffect[Seq, Id]` | `Choose` — literally the same |
| `Env[R] = ContextEffect[TypeMap[R]]` | `Reader % R` (not an arrow — reader family) |

**Outbound** (okay → kyo) is a resume-walk: each okay operation maps
to its kyo counterpart (`Ask` → `Env.get`, tell → `Emit.valueWith`,
raise → `Abort.fail` with the dead continuation dropped, `Choose` →
`Choice.get`) and the continuation follows.

**Inbound** (kyo → okay) rides kyo's `ArrowEffect.handleFirst`: the
handler returns OUR tree as its value, kyo's continuation repacked
into our operation's flatMap, and recursion re-handles — which is
what makes MULTI-SHOT work: a kyo Choice computation explored by our
`runChoice` visits every branch.

**Values.** `fromKyo` (pure eval in), `fromKyoAsync` (their async
runs to completion inside one okay operation — `runAndBlock`, a
virtual thread parks), `toKyo` (an okay program as a kyo IO
suspension).

## Tutorial

```scala
import okay.kyo.KyoInterop

// a kyo Env computation under an okay Reader handler:
val k: Int < Env[Int] = Env.get[Int].map(_ * 2)
val okayProg: Int ! Reader % Int = KyoInterop.fromKyoEnv(k)
Reader.run(21)(okayProg)                       // 42

// okay nondeterminism explored on the kyo side and vice versa:
val choices: Int < Choice = KyoInterop.toKyoChoice(effect(Choose(Seq(1, 2, 3))))
val back: Int ! Choose = KyoInterop.fromKyoChoice(choices)
runChoice(back)                                // Seq(1, 2, 3), every branch
```

## API reference

| member | signature | meaning |
|---|---|---|
| `KyoInterop.fromKyo` | `(A < Any) => A ! Pure` | pure eval in |
| `KyoInterop.fromKyoAsync` | their async inside one okay op | runAndBlock on a parked thread |
| `KyoInterop.toKyo` | `(=> A ! Async) => A < IO` | okay as a kyo suspension |
| `toKyoEnv/toKyoEmit/toKyoAbort/toKyoChoice` | operation-for-operation outbound | the structural mapping |
| `fromKyoEnv/fromKyoEmit/fromKyoAbort/fromKyoChoice` | inbound via handleFirst | multi-shot preserved |

## Gotchas

- kyo wants evidence in places: `Tag[W]` AND `Tag[Emit[W]]` for Emit,
  `SafeClassTag[E]` for Abort.run, `Flat` for eval.
- In `package okay` scopes, `Comonad[Id]` puts `.map` on every type —
  kyo lambdas may need an ascribed parameter, or use flatMap.
- kyo evaluates at CONSTRUCTION (its eagerness contract): see
  compare/TestLaziness for the exact differences the bridges
  preserve rather than paper over.
