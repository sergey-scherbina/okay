# Prob

Probabilistic programming as an effect, in the design of Kiselyov and
Shan's Hansei: a program makes weighted choices and conditions on
evidence, and INFERENCE IS A HANDLER over the ordinary program.

## Operations and handlers

| | |
|---|---|
| `Prob.dist((a, w)*)` | a weighted choice |
| `Prob.uniform(as*)` | an unweighted one |
| `Prob.observe(cond)` | condition on evidence: branches where it is false are dropped |
| `Prob.runExact(p)` | exact inference: the joint weight of every reachable value |
| `.posterior` | those weights normalized to sum to 1 (`import Prob.posterior`) |
| `Prob.sampleOnce(p)` / `Prob.runRejection(n)(p)` | sampling, for models too big to enumerate |

`runExact` resumes the continuation once per alternative, so it needs a
multi-shot runtime, the property [Choice](choice.md) is built on.

## Example

```scala
val coins: Int ! Dist =
  for
    a <- Prob.uniform(0, 1)
    b <- Prob.uniform(0, 1)
  yield a + b

val heads = !.run(Prob.runExact[Int, Pure](coins)).posterior   // Map(0 -> 0.25, 1 -> 0.5, 2 -> 0.25)

val someHeads: Int ! Dist = coins.flatMap(n => Prob.observe(n > 0).map(_ => n))
val conditioned = !.run(Prob.runExact[Int, Pure](someHeads)).posterior   // 1 -> 2/3, 2 -> 1/3
```

A shared sub-model is memoised with [Once](once.md): share the value,
not the effect.

See also: `src/main/scala/Prob.scala`; Kiselyov and Shan, "Embedded
probabilistic programming" (DSL 2009).
