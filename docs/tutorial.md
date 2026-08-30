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

## 5. Async is just blocking, on Loom — and the same source on JS

```scala
val fetch: String ! Async = async(blockingHttpCall())
val both = Async.par(fetch, async(readFile()))
val fast = Async.race(fetch, Async.timeout(500)(fetch).map(_.getOrElse("fallback")))
retry(Retry.exponential(100).take(5))(fetch)   // the policy is a stream
```

A virtual thread parks wherever you block; `spawn` gives a `Fiber`;
`Channel.merge` combines two async streams by readiness. The same
program runs where nothing may block: `Async.runAsync` drives the
tree through the event loop and answers a `Future` —

```scala
// runs unchanged on the JVM, under Node, and as a native binary:
Async.runAsync(Async.sleep(50).map(_ => 42))        // Future(42), loop never blocked
Async.runAsync(Async.par(Async.sleep(20).map(_ => 1),
                         Async.sleep(10).map(_ => 2)))   // Future((1, 2))

val f = Async.spawn(Async.sleep(50).map(_ => work()))
f.cancel()          // stops the drive AND unregisters the parked timer
f.joinAsync         // the effect-world join: an Await, good anywhere
```

Callbacks carry an error channel — `Async.await(k => ...)` can answer
`k(Left(e))` and the program fails at that operation, which is what
lets `par` propagate a child failure (cancelling the sibling) without
parking anything. A blocking `f.join()` needs `CanBlock` evidence and
simply does not compile on JS.

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
Json.read[Answer]("""{"city": "Kyiv", "country": "Ukraine"""")
// Right(Answer("Kyiv", "Ukraine"))
```

## 7. Edit, relex, reparse — incrementally

An editor session keeps snapshots; both layers resume from them and
reconverge, so an edit costs O(damage), not O(document):

```scala
val doc = "{\"alpha\": [1, 2, 3],\n \"beta\": 123,\n \"gamma\": {\"x\": true}}"
val session = Parse.full(JsonLex.scan, JsonParse.instrs)(doc)

val edited = doc.replace("123", "987")          // same length, one member
val at = doc.indexOf("123")
val re = Parse.reparse(JsonLex.scan, JsonParse.instrs)(
  session, doc, edited, at, at + 3, at + 3)

re.tree == Parse.full(JsonLex.scan, JsonParse.instrs)(edited).tree  // exact
// and the untouched subtree is the SAME object, not a rebuild:
gammaNode(re.tree) eq gammaNode(session.tree)   // true
```

A length-changing edit still reparses O(damage); the reused suffix
gets its spans shifted (the absolute-span tax). The contract behind
it: the driver maps one token to its instructions with no cross-token
state — all parsing state lives in the persistent builder, and a
builder snapshot is a pointer.

## 8. One Schema, many wires

`Schema.derived` reifies a datatype's shape once; every format is an
algebra folding it:

```scala
case class Person(name: String, age: Int, tags: List[String], boss: Option[Person])
given Schema[Person] = Schema.derived

val p = Person("ann", 41, List("a"), None)
Json.read[Person](Json.write(p))    // Right(p) — text
Cbor.read[Person](Cbor.write(p))    // Right(p) — RFC 8949 binary, same content
```

The dialects are total and lossless: `Json.render(Json.cst(s)) == s`
byte-for-byte (duplicate keys, odd spacing, damage included), and the
Markdown dialect handles crossing emphasis by REFRAMING —
`Markdown.parse("*a _b* c_\n")` is a well-nested tree where the
underscore emphasis closes and reopens around the star's close, every
marker kept, no faults anywhere.

## 9. A tokenizer is a Scan, even BPE

```scala
val bpe = Bpe(List(("h","e"), ("l","l"), ("he","ll"), ("hell","o")))
Scan.all(bpe)("hello hell her").tokens.map(_.lexeme)
// hello | hell | he | r  (whitespace rides the Trivia channel)
```

The same `Scan` interface as every lexer: incremental, span-exact,
chunked (`Scan.chunks`) and snapshot-friendly — an LLM's tokenizer
and a JSON scanner are the same machine with different dictionaries.

## 10. When the shape is known, stage it

```scala
Staged.fold(
  Staged.take(
    Staged.filter(Staged.map(Staged.range(0, 1000000), _ * 2), _ % 3 == 0),
    1000))(0L)(_ + _)
```

This is the map/filter/take/sum lane as ONE fused while-loop: 1.6us
against Iterator's 19.3 and the interpreted tree's 15.9. The rule:
the `Pipeline` tree is for tools (optimize, inspect, ship), the
inline shape is for speed — same choice the effects layer offers with
Free and Eff.

## 11. Chunks across machines

```scala
val source = Chunks.map(Chunks.range(0, 1000, 16))(_ * 0.5)
val agg = Aggregator.variance[Double]

Cluster.distribute(source, Vector(wireWorker, localWorker))(agg.init, agg.merge)
```

A worker is one function `Chunk[A] => Acc` — in-process or a wire
away (send the chunk, await the partial); a dead worker throws, and
that is the whole protocol: its chunk — still in hand, the source is
a value — goes to a survivor, and the partials merge by the same
combOp that Spark and Flink call merge.

## 12. Where to go next

The [guide](guide.md) explains each layer; the
[typepedia](typepedia.md) is the reference; the
[benchmark explainer](benchmarks.md) walks every measured case; each
module page under [modules/](modules) is that module's full
documentation — guide, tutorial, API reference, gotchas. The specs
directory holds the design decisions — including the experiments that
were tried, measured and rejected, so you don't have to re-run them.
