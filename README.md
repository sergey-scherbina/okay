Okay! Extensible effects for Scala 3.

Inspired by Oleg Kiselyov.

https://okmij.org/ftp/Haskell/extensible/more.pdf
"Freer Monads, More Extensible Effects" Oleg Kiselyov

https://bentnib.org/paramnotions-jfp.html
"Parameterised notions of computation" Robert Atkey

Zero dependencies. JDK 21+ (Loom).

## Architecture

- `Cont[A, S, R]` (Cont.scala) — the parameterised continuation monad
  (answer-type modification, shift/reset), defunctionalized like Free,
  so running a flatMap chain is stack-safe.
- `Control[M[_, _, _]]` (Cont.scala) — final tagless interface of
  delimited control; instances: `Cont` (stack-safe data) and `Func`
  (the function encoding, the reference).
- `Effects[M[_[+_], _]]` (Effects.scala) — final tagless interface of
  extensible effects, founded on the continuation paramonad: a handler
  is `F !> S = F ==> ([X] =>> X /> S)`, an interpretation of the
  operations in Cont, and the meaning of a computation is its `foldCont`;
  `runWith` and `handle` derive from it. Instances: `Free` (initial,
  defunctionalized) and `Eff` (final, Church). Choosing: the tree is
  for tools (stepping, staged relay, stack safety on any bind shape),
  the function is for speed (fused build-and-run pipelines), and the
  interface is for not choosing too early — `fromFree` and `reify`
  move programs between the encodings.
- `!.relay` (Effects.scala) — tail-resumptive handling: the answer-polymorphic
  handler must resume exactly once, which keeps the loop tail-recursive.
  `Effects.handle` — general handlers (abort, forwarding), via foldCont.

## Effects

- `Reader` — the environment, handled at relay speed (Reader.scala).
- `Writer` — telling IS streaming: the opaque identity signature, zero
  allocation per tell, the element type separate from the answer
  (`A ! Writer % W` computes A telling W); run/fold into any Fold
  algebra, uncons as `Either[A, (W, rest)]` (Writer.scala).
- `State` — get/set with a bespoke tail-recursive handler; `PState` —
  type-changing (typestate) state on the paramonad (State.scala).
- `Throws` — typed errors: abort, runEither, the `throws` union
  (Throws.scala).
- `Choice` — nondeterminism with a genuinely multi-shot handler; the
  canonical MonadPlus (Choice.scala).
- `Async` — Loom-style: one operation, "run this (possibly blocking)
  computation"; blocking parks a virtual thread. `spawn`/`par`/`race`/
  `timeout`, `Fiber` + `Scheduler` (loom default, forkJoin and plain
  threads for JVMs without Loom) (Async.scala).
- `Resource` — the region: acquires release at the end of the scope in
  reverse order, surviving handled aborts and mid-step exceptions;
  `bracket` over any Handler-able row (Resource.scala).

## Streams

A stream is codata: one observation, `uncons` — effectful, in
`Stream[S[_], F[+_]]` (F = `Zero` for pure, `Async` for awaited
elements). LazyList is the final coalgebra every stream unfolds into.
Consumption modes, slow-to-fast on a map/filter/take(1000)/sum
pipeline (JMH, us/op; plain Iterator floor = 14.1):

| mode | us | note |
|---|---|---|
| `.toLazyList` + combinators | 143 | memoized, re-observable |
| `.iterator` | 53 | linear, fused, consume-once |
| `Chunks` + `.elements` | 23.6 | chunked source |
| `Chunks.map/filter/take` | 16.9 | chunk-in, chunk-out array passes |

(kyo 239, ZIO 692, fs2 1410 on the same pipeline.)

- `Fold`/`Foldable` — the push side; `Monoid` derives folds.
- Writer programs, producers, generators (`generate`/`Put`: one unfold,
  three carriers — LazyList, Producer, Teller) are all streams; effect
  handlers forward the telling, so they are stream transformers.
- `Take`/`pipe` (Pipe.scala) — coroutine pipelines: tell meets await
  one element at a time, no channel, no materialization; the consumer
  drives, a finite consumer ends an infinite producer.
- `Chunks[A] = Producer[Chunk[A]]` (Chunks.scala) — the tree steps per
  chunk, an element costs an array index: generators, transformers,
  zip, rechunk, fold, pipe; spec in specs/chunked-streams.md.

## Concurrency

- `Channel` — the queue between fibers; `merge` combines streams by
  readiness (chunked merge: 14.7 us vs ZIO 47 on 2x500), `buffer`
  runs the producer ahead (Channel.scala).
- Everything runs on virtual threads by default; fork/join of 100
  trivial tasks: 29 us (raw Loom 21, kyo 25, ZIO 50, cats-effect 140).

Benchmarks: `sbt 'Jmh/run .*FibBenchmark.*'`, comparisons in the
`compare` module (`sbt 'compare/Jmh/run ...'`); history and refuted
experiments in src/jmh/history.tsv.
