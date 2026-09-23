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

A `Fold` reads to the end. When the CONSUMER knows when it has seen
enough, use a `FoldUntil` — a fold with a `done` that is asked before
every pull, so the producer is never resumed past the element that
satisfied it:

```scala
countdown(1000000).foldUntil(using FoldUntil.find[Int](_ % 7 == 0))
// Some(999999) — two steps of the million; the rest is never built
```

`find`, `headOption`, `exists`, `forall`, `take(n)` are the built-in
ones; `FoldUntil.until(z)(step)(finish)` is the general shape, a
`step` answering `Left(next)` to go on or `Right(result)` to stop.
The same fold runs over chunks (`Chunks.foldUntil`), over an
asynchronous source (`Source.runFoldUntil`), over a plain collection,
and as an iteratee — a consumer program `pipe` pairs with any
producer — one instance, every carrier:

```scala
pipe(countdown(1000000))(Take.foldUntil(using FoldUntil.find[Int](_ % 7 == 0)))   // Some(999999)
List(3, 1, 4, 1, 5).foldUntilTo(using FoldUntil.find[Int](_ > 3))                 // Some(4)
```

The dual — a LOOP whose state decides when to stop — is `!.loop`, the
`tailRecM` of programs: continue from a `Left`, answer a `Right`, and
the recursion lives in the tree rather than on the stack, so a
million rounds are fine:

```scala
val digits: Int ! Writer % Int = !.loop(2024) { n =>
  Writer.tell(n % 10).map(_ => if n < 10 then Right(1) else Left(n / 10))
}
!.run(Writer.run(digits))   // (Seq(4, 2, 0, 2), 1) — the digits told, the answer 1
```

`countdown` IS a generator, in Python's sense — the body runs to its
next tell when asked and no further — and `Gen[W]` is its name with
the words a for-comprehension uses and a `stop`:

```scala
val squares: Gen[Long] = for n <- Gen.unfold(1L)(i => Some((i, i + 1))) yield n * n
squares.take(3).toList                        // List(1, 4, 9); the body ran three steps

val fib: Gen[Long] = generator[Long] {        // or a block: while/if/recursion, emit, stop
  var (a, b) = (0L, 1L)
  while true do { Gen.emit(a).!?; val t = a; a = b; b = t + b }
}
fib.iterator.drop(10).next()                  // 55 — and the body has run exactly 11 steps
```

`take`, `first`, `find` and every other reader stop the body where
they have read enough (fold-until); `Gen.stop` ends it from inside a
loop; the details, the three endings and the papers are in
[direct style](direct-style.md#generators-yield-pulled-by-the-reader).

## 3. Chunks make it fast

```scala
val sum = Chunks.fold(
  Chunks.take(
    Chunks.filter(
      Chunks.map(Chunks.nats[Int]())(_ * 2))(_ % 3 == 0))(1000))(using Fold.sum[Int])
```

Same semantics, the tree steps once per 64 elements: this pipeline
measures 10.2us — 8.2 when the whole input is one chunk, which is what
the competitors' own chunked sources get, and there fs2 `emits` takes
21.9, `ZStream.range` 35.8 and kyo `Stream.range` 65.9 (plain Iterator
15.2). Or reify and let
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

When the programs are INDEPENDENT, say so in the instance instead of
in the plumbing. `Par` reads `A ! Async` as one leaf of an applicative
spine, so its `app` joins two leaves with `par` — and every generic
combinator written against `Applicative` runs them at once:

```scala
traverse(keys)(fetch)        // sequential, as ever
Par.traverse(keys)(fetch)    // the same program, leaves at once

// leaves of DIFFERENT types, joined by a plain function:
Par.map2(Par(user(id)), Par(orders(id)))(Profile.apply).seq
```

`Par` has no `flatMap`, on purpose: a bind would sequence the spine
while the type still claimed independence. `.map` maps one leaf's
answer, and `map2` joins two leaves of different types. For a flat sequence of same-typed programs on the JVM,
`parAll` is cheaper still (one fiber per leaf, no nesting) — chapter
12 of the [theory book](theory/12-applicative-static.md) has the
numbers and the reason.

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
Logic.cut(m)                             // the cut: first answer only
```

`!.once(p)` is call-by-need: `p` runs at its
first demand and answers from a cell after, under `Once.run`. In a
`direct` block it is `lazy val`:

```scala
val prog: Int ! (Once + Writer % String) = direct:
  lazy val x = !told("abc")              // runs at the first use, once
  val y = !told("de")                    // runs here
  x + x + y + !told("f")                 // log: de, abc, f
Once.run(prog)
// runChoice(Once.run(p)): a cell per branch; Once.run(runChoice(p)): one cell for all
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
the `Free` tree and an inline handler-passing program over `Control`.

The same rule reaches a `direct` block (direct-staged, 2026-09-22):
when the handlers are known where the block is written, the block
compiles to a function of its continuation with each operation
already replaced by its handler's arm — no dispatch, no tree:

```scala
val sw = Stager.StateWriter[Int, String, Int]()      // the row's staged interpreter

def step(i: Int, acc: Int): Handled[sw.Row, sw.R, Int] =
  if i >= 100 then Handled.pure(acc)
  else Direct.staged(sw) {
    val a = State.get[Int].!?
    State.modify[Int](_ + i).!?          // compound programs are walked too
    Writer.tell("w").!?
    step(i + 1, acc + a).!?
  }

sw.run(0)(step(0, 0))                    // ((state, log), answer)
```

Measured on a thousand operations: 7.5 µs and 85 KB against 16.8 µs
and 165 KB for the identical block as a plain `direct` block run by
`State.run(Writer.run(_))` — 2.24x, to within 1% of the same program
written by hand. What made the difference was found by measuring the
alternatives: a handler passed as a value is 0.89x of the tree; only
the arm chosen by the COMPILER, `Stager.stage`'s `inline match` on the
operation as written, pays. The price is stated where it is paid: a
`Stager` object per row and answer layout, and a staged block is
`Func` — fast, and not stack-safe on a left-nested chain of
millions. The stager for the rows you actually write is
`Stager.All[E, S, W, Err, A]` — Reader, State, Writer and Throws in
one layout, `Unit`/`Nothing` in the slots a block does not use — so a
block that reads a configuration and may fail is
`Direct.staged(Stager.All[Cfg, Unit, Nothing, String, Int]()) { … }`,
run as `.run(cfg, ())` to `((state, log), Either[String, Int])`; a
`raise` inside ends the block with the `Left`
([direct style](direct-style.md#layer-2½--the-staged-block-the-handler-known-at-the-call-site)). The lineage is Xie & Leijen's evidence passing and Schuster
et al.'s capability-passing compilation (references in
[direct style](direct-style.md)); the numbers are in
specs/direct-staged.md.

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

The library names the shapes people actually write, so a raw `shift`
is rarely needed. `collect`/`emit` is the generator above with the
evidence typed: a producer that pushes — a tree walk — is read as a
list, and stays an ordinary recursion that knows nothing about lists.
`collectUntil` reads the SAME producer until a `FoldUntil` (§2) has
seen enough, and the rest of the walk never runs:

```scala
enum Tree[+A]:
  case Leaf(a: A)
  case Node(l: Tree[A], r: Tree[A])

def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! (Delim + Pure) = direct:
  t match
    case Tree.Leaf(a)    => !Delim.emit(a)
    case Tree.Node(l, r) => !walk(l); !walk(r)

val tree = Tree.Node(Tree.Node(Tree.Leaf(1), Tree.Leaf(2)), Tree.Node(Tree.Leaf(3), Tree.Leaf(4)))

!.run(Delim.collect[Int, Pure](walk(tree)))                                                          // List(1, 2, 3, 4)
!.run(Delim.collectUntil[Int, Vector[Int], Vector[Int], Pure](using FoldUntil.take(2))(walk(tree)))    // Vector(1, 2) — the walk stops at its second leaf
!.run(Delim.collectUntil[Int, Boolean, Boolean, Pure](using FoldUntil.exists[Int](_ > 2))(walk(tree)))  // true, after three leaves
```

`collect` builds its list on the way back, in the continuation;
`collectUntil` passes the fold's state on the way down as the
prompt's answer, so a stop is just a continuation never called
(docs/continuations-in-practice.md §2 has the reasoning, theory ch. 2
the theorem it rests on).

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
statements into the binds you would have written, and marks (`m.reflect`, `m.!?`, prefix `!m`)
or opt-in auto-coloring let monadic values stand in plain positions.
Multi-shot survives — a bare `List(1, 2, 3)` statement re-runs the
rest of the block per element. And a block may call its own def:

```scala
def fib(n: Int): Long ! Pure = direct:
  if n < 2 then n.toLong else fib(n - 1) + fib(n - 2)   // or !fib(n - 1) + !fib(n - 2)
```

runs a million deep on the default stack, because a self-call inside
a block is deferred into the tree and trampolined by the interpreter
rather than the JVM (the coloured spelling wants
`import scala.language.implicitConversions`; the `!` one wants nothing). The block composes with chapter 19:
the door outside answers *what is available*, the block inside
answers *how it reads* (`TestDirectDoors`). The layers, the gates
and the graveyard of rejected designs are in
[direct style](direct-style.md).

### The whole `for`, and the HOFs a mark lands in

Since direct-loops v2 a block takes the entire for-comprehension —
guards, several generators, a `yield` into whatever collection the
line is typed as — and the higher-order methods an effect most often
sits inside. Each is the same loop underneath (an immutable
`LazyList`, a recursive `def`, the body compiled per element):

```scala
def look(i: Int): Int ! (Writer % String) = Writer.tell(s"look $i").flatMap(_ => pure(i * 10))

val prog: List[Int] ! (Writer % String) = direct {
  for
    x <- List(1, 2)
    y <- List(10, 20) if y > 10        // a guard between generators
  yield look(x + y).!?                 // List(210, 220); log: look 21, look 22
}

direct[Option] { for (k, n) <- Map("a" -> 1) yield (k, Some(n * 10).!?) }   // Some(Map(a -> 10))
direct { List(1, 2, 3, 4).exists(x => look(x).!? > 15) }   // true, and the log stops at "look 2"
direct { List(1, 2, 3).foldLeft(0)((acc, x) => acc + look(x).!?) }          // 60
```

`exists`/`forall`/`find` stop at the element that decides, `filter`
keeps the matches, `foldLeft` threads its accumulator; a `None`
anywhere — an inner generator, a guard, a step — ends the whole
comprehension. What is not on that list (`collect`, `sortBy`,
`count`, `zip`…) keeps the "under a lambda" refusal until someone
needs it, and a `yield` into a lazy target is refused because a
strict traverse would force it — generators are their own road.
The desugaring the macro reads is Wadler's *Comprehending Monads*
(1992), which is why "the whole `for`" is four combinators; the
details and the refusals are in [direct style](direct-style.md).

### Independent binds, run together

A `direct` block reads one line after another, and one line after
another is what it emits — unless you say the binds are independent:

```scala
import okay.Direct.parallelBinds.given
val profile: Profile ! Async = direct:
  val u = fetchUser(id).reflect     // neither mentions the other,
  val o = fetchOrders(id).reflect   // so both run at once
  Profile(u, o)
```

The macro takes a maximal run of consecutive binds whose right-hand
sides do not mention a name bound earlier in the run, and emits N
spawns then N joins — the flat shape, which is `parAll`'s and measures
the same. Without the import nothing changes, to the byte. A bind that
needs an earlier answer ends the run and stays sequential, and so does
anything that is not a plain `X ! Async` leaf.

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

## 22. Every error, not the first

A check that stops at the first problem makes a person fix their
configuration one line per run. The rung below the monad cannot stop,
so it collects:

```scala
// the same traverse, two carriers
traverse(fields)(checkEither)      // Left(first problem)
traverse(fields)(checkValidated)   // Invalid(all of them)
```

`Validated[E, A]` needs a `Semigroup[E]`, which is the one thing the
caller supplies: a vector for a form, a count for a sampler, a map
keyed by field for an API. It has no `flatMap` on purpose — a Monad
instance would be forced by law to stop at the first error, which is
the behaviour the type exists to refuse. When a later step really does
need an earlier answer, `andThen` says so where it happens.

`okay-conf` is the first consumer: three mistyped environment
variables come back in one message instead of three runs.

And it reads in direct style, which is the part that used to be
impossible: a `direct` block asks its carrier only for what the block
uses, so a run of independent binds needs an `Applicative` and no
more.

```scala
val checked: Checked[Form] = direct:
  val name  = nonEmpty(raw.name)
  val email = looksLikeEmail(raw.email)
  val age   = inRange(raw.age)
  Form(name, email, age)          // three problems, or a Form
```

The carrier comes from the expected type, and a val whose type is a
program of it binds without a mark. An `if` whose CONDITION is an
effect becomes `ifS`, so a branch that is not taken does not run:

```scala
val order: Validated[Errors, Order] = direct:
  val item = checkItem(raw.item)
  val ship = if wantsDelivery(raw.delivery) then checkAddress(raw.address)
             else pickup
  Order(item, ship)
```

A bad address is not reported on an order that was never going to be
shipped, and the checks around the conditional still accumulate. `.reflect` and a type annotation
are the two louder ways to say the same thing, and all three mix.

A bind that needs an earlier answer is refused by name, because that
one really does need a monad.

## 23. One optic, three effects

A traversal's signature asks for an `Applicative` and nothing more:

```scala
def traverseOf[F[_]](f: A => F[B]): S => F[T]      // Applicative[F]
```

So the applicative slot is where the effect goes, and every carrier
drops into it with no code in the optics for any of them. One optic —
every line of an order:

```scala
val eachLine = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])
```

**Report every bad line, not the first.** At `Validated` the walk
collects; at `Either` it stops.

```scala
eachLine.traverseOf(check)(order)
// Invalid(["ink: qty must be > 0", "pad: qty must be > 0"])
```

**Visit the foci at once.** At `Par` each focus runs on its own fiber
and the structure is rebuilt from the answers.

```scala
eachLine.traverseOf[Par](line => Par(price(line)))(order).seq
```

**Ask what the walk WOULD do.** At `Static` the operations are a value
before anything runs, so you can audit them, dry-run them, or answer
them all in one round trip.

```scala
val plan = eachLine.traverseOf(priceLine)(order)
plan.leaves        // Vector(Of("pen"), Of("ink"), Of("pad"))
plan.toFree.runWith  // and the same value, run the ordinary way
plan.foldMap(toBatch)  // or answered in ONE call
```

The worked versions of all three are `TestOpticCarriers`, which is
where the outputs above come from.

### The three on one screen

A form is a value, `Validated` collects every problem, and an optic
puts each message next to the field it names. The validation is a
`direct` block, which needs no monad because the checks do not depend
on each other:

```scala
type Errors = Vector[(String, String)]          // field key -> message

def validate(in: Map[String, String]): Validated[Errors, Signup] = direct:
  val name  = nonEmpty("name", in)
  val email = hasAt("email", in)
  val age   = number("age", in)
  Signup(name, email, age)

def withErrors(tree: Ui, errs: Errors): Ui =
  errs.foldLeft(tree) { case (t, (k, msg)) =>
    Ui.key(k).modify(field => Ui.Column(Vector(field, Ui.Text(msg))))(t)
  }
```

Carrying the field key in the error is what makes the write-back
possible: `Ui.key(k)` is the traversal that finds a node by key, so
each message lands under its own field and the user's edits stay where
they were. `TestUiFormValidation` is the worked version.

One boundary the same test pins: the per-focus function is an ordinary
method, not a nested block. A mark under a lambda is what `direct`'s
v1 refuses, so an optic and a direct block meet at the call, not
inside it.

## 24. What the program will do, before it does it

A `flatMap` hides the rest of the program behind a function, so the
only way to learn what it does is to run it. When the program does not
need that power — a fetch of twenty keys, a module's declared needs, a
rule pack explained before it is applied — write it as a `Static` and
the structure stays readable:

```scala
val plan = traverse(keys)(k => Static.op(Get(k)))   // the same traverse

plan.leaves                 // every operation it MAY perform, before running
plan.toFree.runWith         // the ordinary program, run the ordinary way
plan.foldMap(toBatch)       // N leaves, ONE round trip
```

`Static` is the free selective: `Ap` for application, `Select` for a
conditional whose both sides are written down, so `leaves` is an upper
bound that is exact when there is no branch. `toFree` makes the
difference good at run time — a program that DECLARES three operations
performs two, because a `Select` runs at most one side.

The batching carrier is the payoff: an `app` that accumulates its
leaves' requests turns fifty fetches into one call, with the program
unchanged. [Chapter 12](theory/12-applicative-static.md) builds it.

## 25. Where to go next

The [guide](guide.md) explains each layer; the
[typepedia](typepedia.md) is the reference;
[capabilities](capabilities.md) and [direct style](direct-style.md)
tell the wiring and syntax stories end to end; [optics](optics.md)
puts the nested `copy` and the `Option.map` chain beside the optic
that replaces each, with what both cost; the
[benchmark explainer](benchmarks.md) walks every measured case; each
module page under [modules/](modules) is that module's full
documentation — guide, tutorial, API reference, gotchas. The specs
directory holds the design decisions — including the experiments that
were tried, measured and rejected, so you don't have to re-run them.

If part of your codebase is still on Scala 2.13, most of this tutorial
carries over through `okay-scala2`. That covers chapter 2's telling
(`Source`), chapter 5's async (`Async`, fibers, channels), `shift` and
`reset` (`Cont`), and your own effects. Chapter 13's named prompts
(`Delim`) are not in the Scala 2 facade yet. Direct style (chapter 20)
and staging (chapter 11) will not carry over, because they are Scala 3
metaprogramming. [okay from Scala 2.13](scala2.md) shows what each
looks like in Scala 2.
