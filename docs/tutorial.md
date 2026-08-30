# Tutorial: from a pure program to a streaming pipeline

Everything below is `import okay.*` away (tests in the repo run every
snippet's shape).

## 1. A program is a value

```scala
val prog: Int ! State % Int =
  for
    x <- State.get[Int]
    _ <- State.set(x + 40)
    y <- State.get[Int]
  yield y + 2

State.run(0)(prog)   // (40, 42) — the final state and the answer
```

Nothing ran until `run`. Signatures union freely:

```scala
type F = State % Int + Throws % String
def risky(n: Int): Int ! F =
  if n < 0 then effect(Throws("negative")) else effect(State.Set(n))

runEither(State.handle(0)(risky(5)))   // handle State, then Throws
```

## 2. Telling is streaming

```scala
def countdown(n: Int): Unit ! Writer % Int =
  if n == 0 then pure(())
  else Writer.tell(n).flatMap(_ => countdown(n - 1))

countdown(1000000).toLazyList.take(3).toList   // List(1000000, 999999, 999998)
```

A million-step program, three elements computed: programs are lazy
values. `Writer.uncons` gives you the elements one by one with the
answer at the end; `Writer.fold` collects them into any `Fold`
algebra.

## 3. Chunks make it fast

```scala
val sum = Chunks.fold(
  Chunks.take(
    Chunks.filter(
      Chunks.map(Chunks.nats[Int]())(_ * 2))(_ % 3 == 0))(1000))(using Fold.sum[Int])
```

Same semantics, the tree steps once per 64 elements: this pipeline
measures 16.9us where kyo takes 239 and fs2 1410. Or reify and let
the optimizer fuse it:

```scala
Pipeline.fold(
  Pipeline.generate(0)(identity)(_ + 1)
    .map(_ * 2).filter(_ % 3 == 0).take(1000))(using Fold.sum[Int])
```

## 4. One aggregator, one pass, anywhere

```scala
val stats = Aggregator.mean[Double].zip(Sketch.tDigest())
val (mean, digest) = stats.run(data)        // one pass, both statistics
digest.quantile(0.99)

// the same value, distributed (okay-spark):
SparkInterop.aggregate(rdd)(stats)
```

`Aggregator`'s merge IS Spark's combOp and Flink's merge — one
definition runs locally, chunk-parallel (`parMap`), or on a cluster.

## 5. Async is just blocking, on Loom

```scala
val fetch: String ! Async = async(blockingHttpCall())
val both = Async.par(fetch, async(readFile()))
val fast = Async.race(fetch, Async.timeout(500)(fetch).map(_.getOrElse("fallback")))
retry(Retry.exponential(100).take(5))(fetch)   // the policy is a stream
```

A virtual thread parks wherever you block; `spawn` gives a `Fiber`;
`Channel.merge` combines two async streams by readiness.

## 6. Lex and parse, totally

```scala
// chars -> tokens -> instructions -> a lossless tree, all stages:
val cst = Parse.toCst(
  through(through(chars(text))(Scan.stage(Json.scan)))(JsonParse.driver)
    .toLazyList)

Cst.lexemes(cst) == text        // lossless, damage included
Cst.errors(cst)                 // the diagnostics ARE in the tree
```

Nothing throws: a truncated document is a tree with holes — which is
why a cut-off LLM answer still decodes:

```scala
case class Answer(city: String, country: String)
given Schema[Answer] = Schema.derived
Json.read[Answer]("""{"city": "Kyiv", "country": "Ukraine""")
// Right(Answer("Kyiv", "Ukraine"))
```

## 7. Where to go next

The [guide](guide.md) explains each layer; the
[typepedia](typepedia.md) is the reference; each module page under
[modules/](.) shows its bridge idioms. The specs directory holds the
design decisions — including the experiments that were tried, measured
and rejected, so you don't have to re-run them.
