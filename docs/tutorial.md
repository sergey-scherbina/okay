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
`Channel.merge` combines two async streams by readiness, and
`merge` does it in the program shape — two live feeds joined
into one source whose elements are their union:

```scala
// two differently shaped feeds, joined and consumed by ONE pure stage
val events: Source[Battery | Charging] = battery merge charging
Writer.run(through(events)(widen(combine(repo))))   // okay-demo/Combine.scala
```
 The same
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

// and the third wire, the same Schema, the same decode algebra:
Yaml.read[Person]("name: ann\nage: 41\ntags:\n  - a\nboss:\n")
// Right(Person("ann", 41, List("a"), None))
```

The dialects are total and lossless: `Json.render(Json.cst(s)) == s`
byte-for-byte (duplicate keys, odd spacing, damage included), and the
Markdown dialect handles crossing emphasis by REFRAMING —
`Markdown.parse("*a _b* c_\n")` is a well-nested tree where the
underscore emphasis closes and reopens around the star's close, every
marker kept, no faults anywhere.

## 9. Search that backtracks, fairly

`Choose` is multi-shot nondeterminism; `guard` prunes; `Logic` makes
it a search engine:

```scala
val triples =                            // pythagorean, in order
  choose((1 to 20)*).flatMap(a => choose((a to 20)*).flatMap(b =>
    choose((b to 20)*).flatMap(c =>
      guard[[A] =>> A ! Choose](a*a + b*b == c*c).map(_ => (a, b, c)))))
runChoice(triples)                       // (3,4,5), (5,12,13), (6,8,10), ...

// an INFINITE choice point is a LazyList of alternatives:
def nats: Long ! (Choose + Pure) = effect(Choose(LazyList.from(0).map(_.toLong)))

Logic.observe(6)(Logic.interleave(evens, odds))   // 0,1,2,3,4,5 — fair turns
Logic.fairBind(nats)(x => if x*x == 16 then pure(x) else fail)
                                         // finds 4 where flatMap diverges
Logic.once(m)                            // the cut: first answer only
Logic.ifte(cond)(th)(el)                 // soft cut: el ONLY on no answer
```

## 10. A tokenizer is a Scan, even BPE

```scala
val bpe = Bpe(List(("h","e"), ("l","l"), ("he","ll"), ("hell","o")))
Scan.all(bpe)("hello hell her").tokens.map(_.lexeme)
// hello | hell | he | r  (whitespace rides the Trivia channel)
```

The same `Scan` interface as every lexer: incremental, span-exact,
chunked (`Scan.chunks`) and snapshot-friendly — an LLM's tokenizer
and a JSON scanner are the same machine with different dictionaries.

## 11. When the shape is known, stage it

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

## 12. Chunks across machines

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

## 13. Your own control operator

Delimited control is an effect too, so a new control structure — or a
whole new effect — is user code, not a library change:

```scala
// a generator: a prompt whose answer type is the list being built
def emit[A](p: Prompt[List[A]])(a: A): Unit ! (Delim + Pure) =
  Delim.shift(p)(k => k(()).map(a :: _))

Delim.reset[List[Int], Pure] { p =>
  emit(p)(1).flatMap(_ => emit(p)(2)).map(_ => Nil)
}                                        // List(1, 2)
```

`Prompt[R]` is a first-class tag carrying the delimiter's answer
type, so several delimiters of DIFFERENT answer types live in one
row, and a `shift` can capture past an intervening one — which is
what multi-prompt means and what nested handlers cannot express. All
four classic operators are there: `shift`, `shift0`, `control`,
`control0` (they are two independent bits — does the body keep the
delimiter, does the continuation re-install it).

## 14. An agent is a program

```scala
case class SearchArgs(query: String)
given Schema[SearchArgs] = Schema.derived
val spec = ToolSpec[SearchArgs]("search", "look something up")  // schema DERIVED

val conversation: String ! Agent = Agent.converse("find okay", Seq(spec))
```

No message list appears in the program: it performs `remember` and
`recall`, and the HANDLER owns the policy. So the same program is a
unit test or a production agent depending on what you install:

```scala
// a test
given Handler[Model] = Handlers.scripted(Seq(Reply("hi", Nil)))
// a live model — OpenAI-compatible, so most providers and every
// local runtime; Provider.anthropic speaks the Messages API instead
given Handler[Model] = Provider.openAi(Transports.http(), key, "gpt-4o-mini")
```

The conversation is compacted by an `Aggregator`, so staying inside a
token budget is the default path rather than an emergency branch:

```scala
val (state, ctx) = Handlers.context(Compact.window(4000)(Compact.chars))
```

## 15. Retrieval that the agent does not have to ask for

```scala
val repo = RepoAgent.index(RepoAgent.load(File(".")))   // parse, don't regex
val retriever = Retrieve.hybrid(Seq(
  Retrieve.symbols(repo.index, repo.corpus.sources),    // exact, no vectors
  Retrieve.keyword(repo.keyword)))                      // BM25

val (_, ctx) = Grounded.context(policy, retriever, budget = 6000, share = 0.6)
```

`recall` now contains the relevant code, under the SAME budget as the
conversation — no tool call, no round trip. Every passage carries the
exact byte range it came from, so a citation cannot drift, and
`Corpus.widen` reads more of the document without a second search.

`RepoAgent.load` indexes whatever `Language` knows — Scala, Java,
JavaScript, TypeScript, Rust, Go, C and Python — and each file is
parsed by its own grammar, so a polyglot repository needs no ceremony:

```scala
Symbols.project(files)          // language per file, from the path
Code.source(src)                // parse a Source as its id names
Code.parse(text, 64, Language.python)   // or say which, explicitly
```

Adding one is data, not code, because `Code.scanner` and `Code.driver`
are functions of a `Language`:

```scala
val kotlin = Language("kotlin", Set("kt", "kts"), "//", Some(("/*", "*/")),
  Some("/**"), Set('"'), triple = true,
  definers = Set("fun", "class", "object", "val", "var", "interface"),
  layout = Layout.Braces)
```

That works on day one *because the parser is total*: an imperfect
description degrades into ordinary leaves, so a rough language is
useful immediately and sharpens later without a rewrite. Point
`okay.demo.IndexReport` at a repository to see what it found.

## 16. Cutting generation when the value is complete

```scala
val cut = Structured.cut[Answer](tokenStream)
cut.value      // Some(Answer(...)) — decoded mid-stream
cut.stopped    // true: the tokens after the closing brace were never pulled
```

Each arriving token is an APPEND, which is an edit, so the
incremental parser re-drives only the token rather than the answer so
far; when the tree has no holes and the value decodes, the stream is
simply not pulled again — and since it is demand-driven, not pulling
IS cancelling.

## 17. Durability without paying twice

```scala
Durable.tools(inner, journal)(policy = {
  case "charge" => Durable.OnRepeat.WithKey   // retry carries the first key
  case _        => Durable.OnRepeat.Redo
})
```

Exactly-once EXECUTION of an external effect is impossible, and the
module says so: what it gives is the DECISION, per operation, taken
where the tool is declared. The journal is written intent-first, so
recovery can tell "already happened" from "outcome unknown" from
"never ran". `Durable.replaying` runs an incident again offline, with
the world untouched.

## 18. Tools from anywhere: MCP

```scala
// a real third-party server, spawned over stdio
val link = Stdio.of(Stdio.spawn(Seq("npx", "-y", "@modelcontextprotocol/server-everything")))
val session = Client.connect(link, Mcp.Info("okay", "1")).runWith

given Handler[Tool] = session.handler          // the only line that changes
Agent.converse("...", session.tools.runWith)   // its tools, discovered
```

The agent program is UNCHANGED — a tool call is an effect, and where
it executes is the handler's business; `TestAgentOverMcp` runs the
same program against a local table and a server and compares the
answers. The other directions are as short: our tools are already
what a server serves (`Server.run(Stdio.std, info, tools, table)` —
`RepoMcp` serves this repository that way), a server's resources
become a `Corpus` the retriever indexes (`session.corpus`), its
prompts become the `Seq[Turn]` an agent starts from, and
`sampling/createMessage` is answered by whatever `Handler[Model]` you
already had — an MCP server borrows your model. Transports: stdio, or
streamable HTTP (`McpHttp.link`), with server push on the GET stream.
All of it verified live against the protocol's reference server
(`TestLive`), which passed on the first run.

## 19. Needs are types: capabilities

```scala
val api: (Principal, Tracer) ?=> Traced.Route = {
  case r if r.url.contains("/quote") =>
    okay.async {
      wire[Tracer].span("db.lookup") { () }
      Response(200, Nil, Http.one(s"for:${wire[Principal].name}".getBytes))
    }
}
```

No parameter threading appears in the route: its needs are its TYPE,
and `wire[A]` pulls each one from the nearest installation. So the
same value is a production endpoint or a unit test depending on what
you install — the agent chapter's lesson, generalized:

```scala
// production: doors install from the wire — a verified JWT becomes
// the Principal, a traceparent becomes the Tracer
Traced.route(tracer)(Secure.granted(verify, Policy.scoped("read"))(api))

// unit test: provide installs the SAME needs directly
provide(ada, tracer)(api)                       // no token anywhere

// environments are values: one base, one overridden layer
(base and providing[Principal](bob)) { api }    // answers for:Bob
```

A missing capability is a compile error, not a container exception —
the wiring IS the type checker. The whole story, with its theory and
its exact boundaries, is [capabilities](capabilities.md); the shape
above runs as `TestShowcase` in okay-obs.

## 20. Monads as plain code: the direct block

```scala
def told: Env ?=> Int ! (Writer % String) = direct {
  Writer(s"hello ${wire[Env].user}")   // a bare statement runs — do-notation
  Writer("bye")
  wire[Env].uid                        // the capability, inside the block
}

provide(Env("ada", 7)) { !.run(Writer.run(told)) }
// Vector(hello ada, bye) -> 7
```

No `for`, no `yield`, no `<-`: the `direct` block rewrites plain
statements into the binds you would have written, and marks (`m.?`)
or opt-in auto-coloring let monadic values stand in plain positions.
Multi-shot survives — a bare `List(1, 2, 3)` statement re-runs the
rest of the block per element. The block composes with chapter 19:
the door outside answers *what is available*, the block inside
answers *how it reads* (`TestDirectDoors`). The layers, the gates
and the graveyard of rejected designs are in
[direct style](direct-style.md).

## 21. Errors you can repair: conditions

```scala
def decode(raw: String): Int ! Op =
  raw.toIntOption match
    case Some(n) => pure(n)
    case None    => signal[Int](Damaged(raw))   // raise WITHOUT unwinding

def loop(raws: List[String]): Vector[Int] ! Op = ...
  // one frame per element: within("skip")(decode(r).map(Some(_)))(_ => None)
```

`throw` discards the continuation; `signal` keeps it alive while a
POLICY decides — so "here is the corrected value, continue from
where you were" is an answer, not a wish. The loop offers the menu
(named restarts); the policy, supplied at `run`, picks per incident:

```scala
Condition.run { case (Damaged(_), _) => Resume(2) }(loop(in))       // Vector(1, 2, 3)
Condition.run { case (Damaged(_), _) => Invoke("skip", ()) }(loop(in)) // Vector(1, 3)
Condition.run { (_, _) => Fail }(loop(in))   // Unhandled(Damaged("x"), menu)
```

One decode loop, three outcomes, chosen at the edge — mechanism in
the loop, policy at `run`, exactly the effect discipline the rest of
the tutorial has been practicing. `Throws` and damage-as-data stay
what they are; a program that never signals never pays
(specs/condition.md, `TestCondition`).

## 22. Where to go next

The [guide](guide.md) explains each layer; the
[typepedia](typepedia.md) is the reference;
[capabilities](capabilities.md) and [direct style](direct-style.md)
tell the wiring and syntax stories end to end; the
[benchmark explainer](benchmarks.md) walks every measured case; each
module page under [modules/](modules) is that module's full
documentation — guide, tutorial, API reference, gotchas. The specs
directory holds the design decisions — including the experiments that
were tried, measured and rejected, so you don't have to re-run them.
