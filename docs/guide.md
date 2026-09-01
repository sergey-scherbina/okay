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
worth, flush at the end. The step ANSWERS the new state and is itself
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
0.3s). On JVM/Native it parks
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
