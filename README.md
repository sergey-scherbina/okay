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
`Stream[S[_], F[+_]]` (F = `Pure` for pure, `Async` for awaited
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

## Benchmarks vs the ecosystem

JMH, average time in us/op, lower is better. Versions: cats-effect
3.5.7, ZIO 2.1.14, kyo 0.16.2, atnos-eff 7.0.4, fs2 3.10.2. Full
history, protocols and refuted experiments: src/jmh/history.tsv.

**Bind chain** — 10k left-nested flatMaps, built and run:

| kyo | **okay Cont** | **okay Free** | cats Free | cats Eval | cats IO | ZIO | atnos |
|---|---|---|---|---|---|---|---|
| 58 | **89** | **95** | 129 | 136 | 153 | 181 | 260 |

(kyo's lead is front-loaded eager work: it runs 513 iterations at the
CONSTRUCTION of an infinite program and crashes where laziness is
required — see compare/TestLaziness.)

**Reader** — 10k asks:

| **okay** | ZIO | cats Kleisli/Eval | atnos | kyo Env |
|---|---|---|---|---|
| **110** | 240 | 350 | 1737 | 362 756* |

**Writer** — 10k tells, collected:

| **okay** | cats WriterT/Chain | atnos | kyo Emit |
|---|---|---|---|
| **286** | 1127 | 3202 | 386 322* |

(*kyo Env/Emit go quadratic on left-nested bind chains with handled
operations; the same shape every other lane runs.)

**Choice** — 2^13 branches, all collected (plain List is the floor):

| List | **okay** | kyo | atnos |
|---|---|---|---|
| 580 | **1603** | 3834 | 5392 |

**Fork/join** — 100 trivial fibers (raw virtual threads are the floor):

| raw Loom | kyo | **okay** | ZIO | cats IO |
|---|---|---|---|---|
| 21 | 25 | **29** | 50 | 140 |

**Stream pipeline** — map/filter/take(1000)/sum (Iterator is the floor):

| Iterator | **okay chunked** | **okay elements** | kyo | ZIO | fs2 |
|---|---|---|---|---|---|
| 14 | **16.9** | **23.6** | 239 | 692 | 1410 |

**Merge** — two 500-element streams merged by readiness:

| **okay chunked** | ZIO | okay elementwise | fs2 |
|---|---|---|---|
| **14.7** | 47 | 158 | 9031 |

**Resource** — 1000 bracketed acquire/use/release:

| **okay region** | **okay bracket** | ZIO | cats IO | kyo |
|---|---|---|---|---|
| **18.7** | **26.3** | 106 | 197 | 8566 |

**Generators** — the 1000th Fibonacci number, element by element:

| Iterator | LazyList | **okay Producer** | okay LazyList | kyo | ZStream | fs2 |
|---|---|---|---|---|---|---|
| 12 | 13.5 | **18.4** | 35 | 61 | 172 | 245 |

Benchmarks: `sbt 'Jmh/run .*FibBenchmark.*'`, comparisons in the
`compare` module (`sbt 'compare/Jmh/run ...'`); history and refuted
experiments in src/jmh/history.tsv.
