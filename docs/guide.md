# The user guide

Okay is built in layers, each founded on the one below. This guide
walks them bottom-up; the [tutorial](tutorial.md) walks them by use.

## 1. Delimited control: `Cont`

`Cont[A, S, R]` is the parameterised (Atkey-style) continuation monad:
a computation of `A` that changes the answer type from `S` to `R` —
i.e. `(A => S) => R`, defunctionalized so that running any flatMap
chain is stack-safe. `shift` captures the continuation, `reset`
delimits it. `Control[M[_,_,_]]` is the final-tagless interface;
`Cont` (data, stack-safe) and `Func` (the raw function encoding) are
its instances. You rarely touch this layer directly — it is what
handlers are made of. When you DO want it, the door is `Delim`
(multi-prompt delimited control as an effect): cancellable Dialog
scopes (`Scope`), the streaming cut (`Cut`), the agent stepper and
the sim scheduler are its shipped consumers — and the prompts can
be AMBIENT (`Scope.bounded { … Scope.exit(v) … }` exits the nearest
scope by nesting; a bound prompt still crosses).

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

The floor of the library is available as an effect too: **`Delim`**
is multi-prompt delimited control — `Prompt[R]` tags a delimiter and
carries its answer type, `push` installs one, and
`shift`/`shift0`/`control`/`control0` capture the continuation up to
a NAMED prompt (so a capture can cross an intervening delimiter,
which nested handlers cannot express). The captured continuation
comes back as a PROGRAM, hence multi-shot for free. This is the door
through which a user defines their own effects: a generator is a
prompt plus a shift, with no signature and no handler added.

Over `Choice` sits BACKTRACKING as a library (`Logic`, LogicT-style):
`msplit` splits a search into its first answer and a program for the
rest, and everything derives — `once` (cut), `ifte` (soft cut /
negation-as-failure), `interleave` and `fairBind` (fair search: two
infinite branches take turns, so a witness is found where the plain
bind diverges), `observe(n)`. A `LazyList` of alternatives is an
infinite choice point. And the typeclass hierarchy earns its keep in
the generic combinators — `traverse`/`sequence`/`replicateA`,
`guard` (the pruning conditional of every search), `*>`/`<*`,
`whenS`/`unlessS` — written once, running over any instance.

## 3. Streams: codata by `uncons`

A stream is defined by ONE observation:
`Stream[S[_], F[+_]] { def uncons(s): Option[(A, S[A])] ! F }` — the
next element and the rest, inside the effect `F` (`Pure` for pure
streams, `Async` when the next element must be awaited). LazyList is
the final coalgebra every stream unfolds into (`toLazyList`, memoized);
`iterator` is the linear consume-once view (fused, faster).

`Writer` is the stream with a RESULT: `A ! Writer % W` computes `A`
telling `W` — a one-constructor GADT whose tell answers nothing,
observed by `Writer.uncons: Either[A, (W, rest)]`. `Producer` is the
diagonal cousin. Effect handlers forward the telling, so State,
Reader and Throws handlers ARE stream transformers.

The bridge goes both ways: a writer program IS a `Stream` (in `Pure`,
or in whatever effects it also performs), and `Writer.of(s)` turns any
stream — a List, a LazyList, a Producer, a Channel — back into the
program shape that `pipe`, `through` and the stages consume, its own
effects forwarded into the row rather than run behind the caller.
`Writer.map` transforms the told values in place, where `Stream.map`
lands in LazyList and forgets that the elements are still to be
performed. An asynchronous stream has a name of its own —
`Source[W] = Unit ! (Writer % W + Async)`, built by `Source(a, b, c)`
or `Source.of(stream)` — and is an ordinary `Stream` in `Async`.

Consumption is algebra: `Fold[A, S]` (a start and a step; every
`Monoid` gives one; `Group` adds the inverse that makes sliding
windows subtract instead of recompute), `Foldable` (the push side),
`Aggregator[-In, Acc, +Out]` (Fold + merge + present — the merge is
`(zero, seqOp, combOp)`, which is why one aggregator runs over Chunks,
on Spark and on Flink unchanged; `zip` computes several statistics in
one pass; sketches — HyperLogLog, Count-Min, t-digest — are the
approximate ones, honest monoids with stated error).

Both sides of that algebra are specialized, and the split is the same
one everywhere: where the step is written at the call site, `inline`
takes it (`Chunks.foldLeft(p)(z)(f)`, 38.2us -> 7.0 per 10k Longs);
where the fold arrives as data and nothing can inline — an
`Aggregator`'s, a java `Collector`'s, one chosen at run time — the
accumulator is declared where it is already primitive
(`Fold.OfLong` and its siblings, `Aggregator.OfLong` and its). Only
the accumulator, because that is measured to be nearly the whole cost;
boxing the element read costs almost nothing. See
[the typepedia](typepedia.md) for the shapes and
[existentials.md](existentials.md) for why the one remaining cast
cannot be removed.

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

When the pipeline's shape is known where it is written, `Staged` goes
the last mile — whole-stage codegen by inline partial evaluation:

```scala
Staged.fold(
  Staged.take(
    Staged.filter(Staged.map(Staged.range(0, 1000000), _ * 2), _ % 3 == 0),
    1000))(0L)(_ + _)
```

Nested calls beta-reduce into ONE while-loop with every lambda
inlined: 1.6us where `Iterator` takes 19.3 and the interpreted tree
15.9 (the standard map/filter/take/sum lane). The choice rule is the
library's usual one, one level up: the `Pipeline` TREE is for tools
(optimize, inspect, ship to another node), the INLINE SHAPE is for
speed — a GADT tree cannot partially evaluate through `inline match`
(pattern binding erases the inline-ness of subtrees), so the two
stay separate on purpose.

## 5. Coroutines: `Take`, `pipe`, `Stage`

`Take.await` is the dual of `Writer.tell`; `pipe(producer)(consumer)`
pairs them one element at a time — the consumer drives, a finite
consumer ends an infinite producer. `Stage[I, O, A]` is a transducer
as a program (awaits I, tells O); `through` composes stages
demand-driven. Tokenizers and parsers are stages (okay-lex,
okay-parse).

`Stage.transduce(z)(step)(flush)` is the skeleton they all share —
carry a state, step it per input telling whatever that input is
worth, flush at the end. A stream with PHASES (a header before
rows) gets `Stage.phased`/`phased3`: the accumulator CHANGES TYPE
at the switch, the wrong-phase step does not compile, and the
transition runs through `PState` — typestate on the stream. The step ANSWERS the new state and is itself
a stage, so it may tell nothing, one, or many, and nothing is
allocated per element to say which; the lexer's scanner, SSE framing,
`chunked` and the demo's stream join are all this one call.
`Stage.mapAccumulate` is fs2's 1:1 special case on top of it, kept
for people who arrive with the name — and it is the special case, not
the primitive, because of the five stages written here NONE are
one-output-per-input: conditional emission has to say "nothing here"
with an `Option` that `transduce` never allocates.

Stages may be EFFECTFUL: a row `Take % I + (Writer % O + G)` carries
arbitrary operations G (Async above all) between awaits and tells,
and the `through` overloads forward them through composition in the
order the pull crosses them — laziness intact, associativity intact.
A pure stage joins an effectful row by `!.widen` (plus a union-ACI
ascription); an SSE line stream through an event-framing stage is
this shape in production (okay-llm).

## 6. Concurrency: Loom first

`Async` has one blocking operation (`Run`) and one universal callback
operation (`Await`); on the JVM the handler parks a virtual thread —
blocking IS asynchrony. The Await callback carries an ERROR CHANNEL
(`Either[Throwable, A] => Unit` — a Left fails the program at that
operation, a failure is a value on the wire) and its registration
answers with a CANCELLER, so cancelling a fiber also unregisters the
timer or I/O completion it was parked on. The simple top-level
`await(k => ...)` keeps the success-only shape; `Async.await` is the
full form.

Blocking is evidence-gated (`CanBlock`, given on JVM/Native only): on
JS the SAME programs run through the event loop by
`Async.runAsync(prog): Future[A]`, and a blocking join is a compile
error, not a frozen loop. `Fiber` is onComplete/cancel everywhere
plus `joinAsync` (the effect-world join — itself an Await, good on
every platform); the parking `join`/`joinEither` exist only under the
evidence. `Scheduler` takes the PROGRAM — which is exactly what lets
the event loop be a scheduler (`Schedulers.loom` by default on the
JVM, forkJoin and plain threads for JVMs without Loom, one OS thread
per fiber on Native; the cats-effect and ZIO runtimes plug in as
Scheduler instances from the interop modules).

The combinators are cross-platform: `spawn`, `par` (pairs by
completion callbacks; a child failure fails the pair and cancels the
sibling), `race` (first SUCCESS wins and cancels both; two failures
fail the race instead of hanging), `timeout`, `sleep` (an Await on
the platform `Timer` — a sleeping virtual thread, setTimeout, a
thread), `bracket`. One shared-source Await suite runs on the JVM,
under Node and as a linked Native binary in CI.

`Channel` is the queue between fibers — `merge` combines streams by
readiness, `buffer` runs a producer ahead. `source merge source` is that
merge in the program shape: two sources in, one source out (so a
stage consumes it directly), the fibers starting at the first pull —
and the two element types need NOT agree, because the result tells
their UNION, which the consumer splits by an ordinary type test.
Both derived merges are BOUNDED by default (64), because the channel
takes whatever is offered and an unbounded merge of an endless source
is unbounded memory — measured, ten pulls deep, 1.27M elements
produced against 74 at the bound, and the bound costs nothing the
benchmark can see. `Channel.merge` underneath keeps its unbounded
default: there the capacity is the caller's explicit business.
Chaining merges does not serialize them — each hop is its own fiber,
so eight chained sources overlap as eight (2.4s of parked work in
0.3s). What the program shape COSTS is measured and flat: ~300ns per
element, against ~130ns for the bare `Channel.merge` under it and
~11ns for a native `LazyList` walk — and flat is the operative word,
since a scaling sweep over 8x found the per-element price constant
(the Free tree is linear, `docs/theory/04-free-freer.md`). Most of
that gap is not the interpretation itself but the interpretation
INSIDE the contention: the same layer costs ~30ns per element alone
and ~160ns once two fibers race for the channel's cell. So where
throughput is the point rather than per-element semantics, merge the
CHUNKED streams — `Chunks.merge` is one queue operation per chunk
instead of per element, and measures 10.7us against 299.7us for the
same 2x500. For a source that is elementwise BY NATURE but consumed
in bulk, `merge`'s own `chunked = true` does the same trick
underneath and still hands back an ordinary `Source`: 2.6x at the
default capacity, 5.1x with `capacity = 1024`, since `capacity`
counts elements either way and the rest of the win is bought
explicitly with memory. It is off by default because chunking on its own has no
flush on time — on a slow or unending source an element waits for 15
others that may never come. `flushAfter = Some(millis)` bounds that
wait and makes chunking safe on a live source; it costs nothing when
it does not fire, and it never races the source's pull (cancelling an
in-flight `uncons` could lose an element), taking only what has
already accumulated. A timer is a guess, though, and a producer often
KNOWS where the boundary is — this token ended the model's turn, this
byte ended the frame. `Flush.now` says so as an OPERATION (not a
distinguished element, which would widen every element type and make
every consumer match on something that is not its data): a source in
the `Flushing` row emits it, and `a.mergeFlushing(b)` puts what that
side holds on the wire at exactly that point. It costs the ordinary
chunked path nothing — the two feeds are separate walks precisely
because routing both through the flushing one measured 11% dearer.

Chunking is a property of the STREAM, not a parameter of whatever
consumes it: `s.chunked(size)` gives `Source[Chunk[A]]` and
`.unchunked` gives the elements back, so `merge`, `buffer` and
anything else that crosses a channel gets batching without a flag of
its own — `s.chunked(8).buffer(4)` needs nothing added to `buffer`.
Reading a channel back is batched the same way and with no trade at
all: `c.drained` takes what is already buffered under one
transaction instead of one per element (2.4x on a buffered
producer), delaying nothing, since what is in the buffer is
already late.
`merge(chunked = true)` is the fused spelling of exactly that
composition, and it exists because the TIMED case would otherwise
need a second channel (a timer has to fire while the source is
silent); composing costs nothing where no timer is involved (222.3us
against the fused 223.7 on 2x2000). Keep the size modest — a stage
that accumulates without emitting is bounded by `PullBudget` but a
chunk in the thousands buys nothing here anyway. And where throughput
really is the point, the deeper answer is not to chunk a per-element
source at all but to start chunked: `Chunks.merge` never builds a
program node per element and measures 22.3us against ZIO's own
chunk-native default of 126.2 on the same 2x2000 — okay ahead by
5.7x on equal footing, not the 2.5x an earlier, unfair comparison
claimed (docs/benchmarks.md §6b). For a source that really is
elementwise (a live feed, arriving one token at a time, nothing to
pre-chunk), `Source.range` generates a half-open range with no
collection underneath, and okay's per-element `merge` measures 12x
ahead of ZIO forced onto the same footing (`chunkSize = 1`), not
behind it as comparing against ZIO's chunked default once suggested. On JVM/Native it parks
(bounded, backpressure by parking); JS gets the Await-based channel
behind the same surface (capacity advisory — a JS sender cannot
park). `parMap` maps a chunked stream with a fiber per chunk; `retry`
takes its policy as a STREAM of delays; `retryChunks` recomputes a
failed chunk from the stream's own program — the value is the
lineage, Spark-style. One level up, `Cluster.distribute` (okay-cluster)
rides the same fact across machines: workers behind one seam
(`Chunk[A] => Acc`; a dead worker throws), a failure hands the chunk
— still in hand, the source is a value — to a survivor, partials
merging by the Aggregator's combOp.

`type Blocking[A] = CanBlock ?=> A` names the trade as a
first-class VALUE: a returned `Blocking[A]` is storable and
composable, and only an edge holding the capability can force it.

## 7. Text is a stream: lex → parse → codec

The P5 stack is three small modules over the coroutine layer, all
TOTAL — errors are data in the result, never faults.

**okay-lex.** A lexer is a pure step function `Scan[K, S]`
(`step(s, c) => (S, tokens)`, `flush` finishes the tail): the state
is a VALUE, so it crosses chunk boundaries (`Scan.chunks` — a tight
while per chunk, a token spanning chunks is emitted once, where it
completes) and snapshots for free. `Scan.relex` resumes from the
nearest snapshot before an edit and RECONVERGES — past the edit and
the next newline, a state equal to the old run's state means the old
tokens are reused with shifted spans. Everything is a token, garbage
included (the Error channel); concatenated lexemes of all channels
equal the input.

**okay-parse.** Both parsing surfaces — a hand-written driver and
combinators — emit the ONE instruction language (`Open/Emit/Close/
Bad`), and the total builder folds any instruction stream into a
lossless CST: a Close with nothing open is an error leaf, open nodes
at the end close with an "unclosed" marker — a truncated stream (the
LLM case) is a tree with holes. Incremental reparse is the same
discipline one level up: `Parse.full` snapshots the persistent
builder at root-level node boundaries (a snapshot is a pointer), and
`Parse.reparse` relexes, resumes before the damage and SPLICES once
the token stream is the old one again — unchanged subtrees come back
BY REFERENCE for a length-preserving edit, with rebased spans
otherwise. The contract making token-level reconvergence sound: the
driver is a per-token function, no cross-token state — the stateful
part of parsing is the builder, and the builder is what snapshots.

**okay-codec.** Typeclass codecs are ALGEBRAS OVER A SCHEMA:
`Schema[A]` reifies a datatype's shape once (via Mirrors, thunked
fields for recursion), and every format folds it — `Json` renders
text, `Cbor` renders RFC 8949 binary, one derived Schema serving
both with equal semantic content. Dialects prove the model:
`Json.cst/render` is the byte-for-byte lossless layer; `Markdown`
proves REFRAMING (crossing emphasis `*a _b* c_` closes the inner
frames tokenless, closes the target with its token and reopens the
inner frames — well-nested, lossless, no faults). okay-llm rides the
same totality: a BPE tokenizer is just another `Scan`, and a cut-off
model answer still decodes because a tree with holes projects the
fields that are there.

## 8. The laziness contract

Programs are values: construction does no work (the opt-in Eager is
the sole, stated exception). An infinite program constructs in O(1);
`take(3)` computes three elements; re-observation repeats work rather
than caching (only the LazyList bridge memoizes). This contract is
load-bearing: it is what makes handlers stream transformers, chunk
retry a lineage recompute, and the whole lex/parse stack incremental
— and it is exactly where eager runtimes crash (see
compare/TestLaziness).

## 9. Capabilities: context functions

The stack's implicit evidence (`CanBlock`, `Scheduler`) generalizes:
`A ?=> B` is a capability arrow — returnable, storable,
self-applying where a given `A` is in scope. Three shipped routes:
`Traced.route` (a handler written against `using Tracer`),
`Secure.granted` (the principal ambient in a protected route), and
the ambient prompts of `Scope`/`Cut`. They COMPOSE: one stored
`(Principal, Tracer) ?=> Route` is protected and traced at every
installation site. `provide` is the installer half of
the pair — expression-scoped, nearest-wins:

```scala
provide(prodHttp, Secrets.env) { app }     // the edge
provide(stubHttp, testSecrets) { app }     // the test — same program
```

And installers COMPOSE as values — `providing[A](a)` holds one
`given` for later, `and` chains them flat (the right side wins on
overlap), so a base environment is built once and overridden per
use, with no nesting and no arity cap:

```scala
val base = providing[Http](prodHttp) and providing[Secrets](Secrets.env)
base { app }                                        // the edge
(base and providing[Http](stubHttp)) { app }        // override just Http
```

The consumer side is one line too: `wire[Http]` pulls the ambient
capability by naming its type — `val get: Http ?=> Response =
wire[Http].send(req)` is a door with no `summon` and no parameter.

The payoff, on one page (executable: TestShowcase in okay-obs) —
ONE value whose needs are its type, living in two worlds without
changing a letter:

```scala
val api: (Principal, Tracer) ?=> Traced.Route = {
  case r if r.url.contains("/quote") =>
    okay.async {
      wire[Tracer].span("db.lookup") { () }
      Response(200, Nil, Http.one(s"for:${wire[Principal].name}".getBytes))
    }
}

// production: the doors install from the wire — a verified JWT
// becomes the Principal, a traceparent becomes the Tracer
Traced.route(tracer)(Secure.granted(verify, Policy.scoped("read"))(api))

// unit test: provide installs the SAME needs directly — no token,
// no HTTP machinery; a missing capability would not compile
provide(ada, tracer)(api)

// environments are values: one base, override one layer
(base and providing[Principal](bob)) { api }
```

Together the doors and `provide` are the DEPENDENCY-INJECTION
story: compile-time resolution (a missing dependency is a type
error, not a container exception), given-scopes as the object
graph, modules as ordinary values, zero framework. The rules that
keep it honest — the environment-vs-resource line, no newtypes for
strings, the eager-auto-application trap — plus the two-line recipe
for adding a door to any API are in [typepedia](typepedia.md); the
linear-context patterns and the experimental base in
specs/context-functions.md; the whole story, told in one place with
its theory and boundaries, is [capabilities](capabilities.md).

## Direct style, in one paragraph

Any monad in this library can be written as plain code:
`direct[F] { val x = m.reflect; ... }` compiles the block into the
reflect/reify chain of `Monadic` (Filinski's construction over the
`Cont` of chapter one), so short-circuit, multi-shot and handlers
all behave exactly as in the monadic spelling. Effects are
first-class (`Writer("a")` on its own line tells; loops and `while`
work; `!prog` performs a program in one glyph), auto-coloring can
remove marks entirely behind explicit gates, and every refusal is a
positioned compile error naming the workaround. The whole story,
with the reasoning and the graveyard of refuted alternatives:
[direct-style.md](direct-style.md); the theory with the literature:
[theory ch. 8](theory/08-direct-style.md).
