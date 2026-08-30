# The user guide

okay is built in layers, each founded on the one below. This guide
walks them bottom-up; the [tutorial](tutorial.md) walks them by use.

## 1. Delimited control: `Cont`

`Cont[A, S, R]` is the parameterised (Atkey-style) continuation monad:
a computation of `A` that changes the answer type from `S` to `R` —
i.e. `(A => S) => R`, defunctionalized so that running any flatMap
chain is stack-safe. `shift` captures the continuation, `reset`
delimits it. `Control[M[_,_,_]]` is the final-tagless interface;
`Cont` (data, stack-safe) and `Func` (the raw function encoding) are
its instances. You rarely touch this layer directly — it is what
handlers are made of.

## 2. Effects: `A ! F`

A computation of `A` performing operations of the signature `F` is
`A ! F` — a freer-monad tree. Signatures combine as unions:
`A ! (State % Int + Throws % String)`. The empty signature is `Pure`
(`A ! Pure` is a pure computation; `F + Pure = F`).

A handler interprets operations into continuations — `F !> S` is
literally a natural transformation into `Cont`. Three ways to run:

- `runWith` — a per-operation `Handler[F]` (comonadic: each operation
  answers with a value);
- `!.relay` — tail-resumptive handling: the answer-polymorphic handler
  must resume exactly once, so the loop is tail-recursive; the fastest
  path (Reader and Async run at relay speed);
- `Effects.handle` — the general form: abortive handlers (Throws),
  multi-shot handlers (Choice explores every branch), forwarding.

Three ENCODINGS, one interface (`Effects[M]`): `Free` (the tree — for
stepping, relaying, stack safety on any bind shape), `Eff` (the Church
function — for fused build-and-run speed), and the opt-in `Eager`
(`import Eager.given` — the kyo trick: pure binds apply at
construction, 10x under kyo on pure chains, with kyo's hazards stated:
construction evaluates, so self-referential programs diverge).

The standard effects: `Reader` (environment), `Writer` (telling IS
streaming — see below), `State` (+ the type-changing `PState`),
`Throws` (typed errors, `runEither`), `Choice` (nondeterminism,
multi-shot), `Async` (Loom-style, below), `Resource` (the region:
releases run at the scope's end, in reverse, surviving handled aborts).

## 3. Streams: codata by `uncons`

A stream is defined by ONE observation:
`Stream[S[_], F[+_]] { def uncons(s): Option[(A, S[A])] ! F }` — the
next element and the rest, inside the effect `F` (`Pure` for pure
streams, `Async` when the next element must be awaited). LazyList is
the final coalgebra every stream unfolds into (`toLazyList`, memoized);
`iterator` is the linear consume-once view (fused, faster).

`Writer` is the stream with a RESULT: `A ! Writer % W` computes `A`
telling `W` — zero allocation per tell (an opaque identity signature),
observed by `Writer.uncons: Either[A, (W, rest)]`. `Producer` is the
diagonal cousin. Effect handlers forward the telling, so State,
Reader and Throws handlers ARE stream transformers.

Consumption is algebra: `Fold[A, S]` (a start and a step; every
`Monoid` gives one; `Group` adds the inverse that makes sliding
windows subtract instead of recompute), `Foldable` (the push side),
`Aggregator[-In, Acc, +Out]` (Fold + merge + present — the merge is
`(zero, seqOp, combOp)`, which is why one aggregator runs over Chunks,
on Spark and on Flink unchanged; `zip` computes several statistics in
one pass; sketches — HyperLogLog, Count-Min, t-digest — are the
approximate ones, honest monoids with stated error).

## 4. Chunks: the tree steps per batch

`Chunks[A] = Producer[Chunk[A]]` — a stream of array batches. The
freer tree steps once per CHUNK and an element costs an array index,
which is where the benchmark numbers come from (pipeline 16.9us vs
kyo 239 / ZIO 692 / fs2 1410; merge 14.7us vs ZIO 47). Generators
(`Chunks.generate/range/nats/fibs`), transformers
(`Chunks.map/filter/take/drop/takeWhile/dropWhile` — chunk-in,
chunk-out array passes), `zip` (realigns boundaries), `rechunk`,
`fold`, `pipe`. `Pipeline` reifies a chunk pipeline as a typed
operator tree and `Pipeline.optimize` rewrites it (fusion, take
pushdown into sources) before compiling back onto the transformers.

## 5. Coroutines: `Take`, `pipe`, `Stage`

`Take.await` is the dual of `Writer.tell`; `pipe(producer)(consumer)`
pairs them one element at a time — the consumer drives, a finite
consumer ends an infinite producer. `Stage[I, O, A]` is a transducer
as a program (awaits I, tells O); `through` composes stages
demand-driven. Tokenizers and parsers are stages (okay-lex,
okay-parse).

## 6. Concurrency: Loom first

`Async` has one blocking operation (`Run`) and one universal callback
operation (`Await`); on the JVM the handler parks a virtual thread —
blocking IS asynchrony. `Fiber` (join/cancel/joinEither) and
`Scheduler` (loom by default; forkJoin and plain threads for JVMs
without Loom; the cats-effect and ZIO runtimes plug in as Scheduler
instances from the interop modules). `spawn`, `par`, `race` (cancels
the loser), `timeout`, `sleep`, `bracket`. `Channel` is the queue
between fibers — `merge` combines streams by readiness, `buffer` runs
a producer ahead. `parMap` maps a chunked stream with a fiber per
chunk; `retry` takes its policy as a STREAM of delays; `retryChunks`
recomputes a failed chunk from the stream's own program — the value
is the lineage, Spark-style.

## 7. The laziness contract

Programs are values: construction does no work (the opt-in Eager is
the sole, stated exception). An infinite program constructs in O(1);
`take(3)` computes three elements; re-observation repeats work rather
than caching (only the LazyList bridge memoizes). This contract is
load-bearing: it is what makes handlers stream transformers, chunk
retry a lineage recompute, and the whole lex/parse stack incremental
— and it is exactly where eager runtimes crash (see
compare/TestLaziness).
