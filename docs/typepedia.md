# Typepedia

Every public type and typeclass of the core, with its meaning and its
gotchas. Source files are authoritative — their doc comments carry the
same material with the measurements attached.

## Control (Cont.scala)

- **`Cont[A, S, R]`** — the parameterised continuation monad,
  `(A => S) => R` defunctionalized (Pure/Shift/Bind); running is
  tail-recursive with left-nested binds rebalanced; flatMap fuses
  closures up to a depth budget. The foundation everything stands on.
- **`Control[M[_,_,_]]`** — final-tagless delimited control (`shift`,
  `reset`, `/`); instances `Cont` and `Func`. `transparent inline def
  Control[M]` is the staging entry: resolved statically, the ops
  inline away.
- **`A /> S`** — `Cont[A, S, S]`, the diagonal. **`Loop[A, R]`**
  (alias `<<`) — `Cont[A, R, A => R]`: open recursion; `take` is the
  loop's input, `loop` ties the knot.

## Effects (Effects.scala, Free.scala)

- **`A ! F`** — a computation of `A` over the signature `F` (a freer
  tree). **`%`** fixes a binary signature's parameter (`State % Int`);
  **`+`** unions signatures; **`Pure`** (= `Nothing`) is the empty
  signature — in scopes importing `!.*` write `okay.Pure` (the
  Free.Pure case shadows it).
- **`Module[F]`** (specs/di.md, [the guide](di.md)) — a description of
  what to build, not a built thing: `module[Db](open)(close)` acquires
  in a `Resource` region, `Module.value` needs no building,
  `moduleAs[A, R]` acquires an `R` and installs it as an `A`, and
  `prototype` installs the ability to MAKE one. `and` composes (the
  right side is built inside the left's context, so a dependent module
  is written `Db ?=> Module[…]`), `m { body }` / `m.use { body }` run
  it inside the region, `plan` and `exports` read it, `shadowed` names
  a capability installed twice.
- **`Fact[V]`** — a kind of thing modules DECLARE about themselves and
  somebody else collects; how two declarations merge is a `Monoid[V]`,
  so `object Routes extends Fact[Vector[Route]]` is the whole
  declaration. `declaring(k)(v)` computes it inside the module's own
  installer (so it can read what that module installs), `declare(k)(v)`
  outside it, `Module.contributing(k)(v)` installs nothing at all, and
  `installing(k)` turns the merged value into a capability. Installing
  SHADOWS, declaring ACCUMULATES — that is the whole reason the type
  exists.
- **`New[A]`** — the ability to make an `A`: `fresh[A]` answers
  `A ! Resource`, and the region it runs in releases the instance.
  Always a program, even where nothing is released, so a provider can
  start closing what it makes without touching a consumer.
- **`A |=> B`** — a partial function, infix: `Request |=> Response !
  Async` is the type every route in this stack has. The spelling is
  forced by precedence, not taste: an infix type takes its precedence
  from its FIRST character, `!` sits at the `=`/`!` level, and every
  tighter arrow (`~>`, `-?>`, `=?>`) parses `A ~> B ! F` as
  `(A ~> B) ! F` — measured. `|`, `^` and `&` are the looser ones, `^`
  is already `Cont`, and `=?>` would sit one transposition away from
  the language's `?=>`. A union on the left binds first, so
  `Get | Post |=> Res` reads as it looks.
- **`F !> S`** — a handler: `F ==> ([X] =>> X /> S)`; handlers are
  continuations, literally.
- **`Parse.Step[K, D]`** (okay-parse) — a driver as a pure step
  function with state, the Scan shape one layer up; snapshotted
  beside the builder so incremental reparse stays sound for stateful
  drivers (brace depth, a held doc comment), with `finish` releasing
  whatever the driver deferred at end of input.
- **`!.translate`** — a handler valued in ANOTHER ROW:
  `F ==> ([X] =>> X ! G)`, so an operation answers with a PROGRAM
  rather than a value. This is the general shape the other two are
  ends of — `Handler[F]` is `F ==> Id` (and `Id` is exactly where a
  suspension cannot go, which is why a comonadic handler cannot do
  I/O where nothing may park), `F !> S` is the Cont-valued handler
  `Effects.handle` takes (abort and multi-shot, through Cont), and
  `translate` is the tail-resumptive middle: one walk, no Cont, the
  rest of the row forwarded. `Free.run(f: F ==> M)` is the same idea
  when the row is handled entirely.
- **`Handler[F]`** — the comonadic (per-operation) handler;
  `runWith` runs with it. **`Handler.union`** composes one handler
  per effect into a row handler (an explicit combinator, not a given:
  a given over a union type lambda crashes the 3.7.1 type comparer). **`TypeableK[F]`** — the runtime test that
  splits unions (`<|>`, `split`): `unapply` for pattern positions,
  `test` — a plain boolean, no Option — for the split itself;
  identity-style signatures are split by the runtime class of their
  values, so keep them class-distinct.
- **the direct marks: `.reflect` / `.!?` / `!prog`** — one mark,
  three spellings, one dispatch-by-type inside `direct { }` blocks
  (docs/direct-style.md): an `F[T]` of the block reflects, a row
  operation injects then reflects. `.reflect` never collides; `.!?`
  is the postfix symbol for chains; prefix `!` is the one-glyph
  gesture (`unary_!` under the hood — shadows nothing). Gotcha: the
  RETIRED `.?` belongs to the Throws machinery, not to direct — if
  you see `Ambiguous extension methods` on a `?`, you are on an old
  branch. Distinct from Monadic's `reflect`/`reify` pair below and
  from the Effects encoding pair below THAT — three uses of one
  word, each namespaced.
- **`reify` / `reflect` / `convert`** — one function at two ends. An
  encoding is fixed by `pure` and `perform` and `foldCont` is its
  fold, so there is exactly ONE structure-preserving way across:
  `reify` observes an abstract encoding as syntax (what a debugger, a
  rewriter or `Pipeline`'s optimizer wants), `reflect` spends syntax
  at an encoding (what running it fast wants — a tree built once can
  be reflected into `Eager`, where pure binds apply at construction),
  and `convert` crosses between any two without passing through a
  tree. A round trip in both directions, asserted for every encoding.
  Gotcha: `reflect` shadows `scala.reflect` inside package `okay`.
- **`Effects[M]`** — the interface; instances **`Free`** (initial),
  **`Eff`** (final/Church) and **`Eager`** (opt-in, companion-scoped
  given: pure binds run at construction; the type is opaque so the
  encoding cannot leak into inference).
- **`!.relay`** — tail-resumptive handling; **`Effects.handle`** —
  abortive/multi-shot/forwarding; **`!.widen`** — effect-row
  subsumption (Free is invariant, so it walks the tree).

## The standard effects

- **`Reader % R`** — `Ask`, handled at relay speed.
- **`Writer % W`** — opaque identity signature: telling w IS emitting
  w, zero allocation; `A ! Writer % W` keeps the element type apart
  from the answer; `Writer.uncons: Either[A, (W, rest)]`;
  `Writer.fold/run` collect through any `Fold`. The diagonal is
  **`Teller`**; `Put[Teller]` closes the generate triangle.

  A tell answers NOTHING — `tell[W](w: W): Unit ! Writer % W` — and
  anything a caller wants back it says explicitly
  (`tell(w).map(_ => w)`). The operation is a one-constructor GADT,
  `case Say(w: W) extends Writer[W, Unit]`, which is what makes that
  answer type recoverable: under a `Bind` it is existential, and
  matching `Say(w)` refines it to `Unit`, so resuming a continuation
  asserts nothing. It also makes the row split unconditional, where
  the previous identity encoding (`opaque type Writer[W, +A] = W`, the
  operation IS the told value) could only forward effects whose
  operations were class-distinct from `W`. Measured, the wrapper costs
  nothing on the real benchmark: 198.0us against 203.2. Five encodings
  were tried before this one to keep the identity representation and
  recover the type anyway; [existentials.md](existentials.md) records
  each, what the compiler said, and the bytecode.
- **`State % S`** — bespoke tailrec handler; **`PState`** — the
  type-changing (typestate) variant on the paramonad, ~1.7x the
  price — no longer only an exhibit: `Stage.phased`/`phased3`
  execute their phase switches through it, and the typed
  transaction region (sql-typestate) is its second consumer.
- **`Blocking[A]`** — `CanBlock ?=> A`: parks-a-thread as a
  first-class value; forced only where the capability is given.
- **`Prompt[R]`** — a delimiter's identity AND answer type (Delim);
  ambient in the capability forms (`Scope.mark/exit/bounded`,
  `Cut.guard/violation`) — nested using-params resolve to the
  NEAREST scope, verified.
- **`Throws % E`** — typed aborts; `runEither/runThrows`; the `throws`
  union type for direct style, which is declared `into` (see below), so
  a caller writes `val x: String throws Fault = "a"` with no language
  import.
- **`Choose`** — nondeterminism; the handler is genuinely multi-shot;
  the canonical `MonadPlus`. A `LazyList` of alternatives is an
  INFINITE choice point (Seq is the parameter, laziness crosses).
- **`Logic`** — backtracking search over Choose (LogicT): `msplit`
  (first answer + the rest as a program — the one primitive), `once`
  (cut), `ifte` (soft cut), `gnot` (negation as failure),
  `interleave` (fair or), `fairBind`/`>>-` (fair bind), `observe(n)`
  (first n of an infinite search). A library over the effect, not a
  new effect. See specs/backtracking.md.
- **`Delim`** — delimited control AS AN EFFECT, multi-prompt
  (Dybvig/Peyton Jones/Sabry): **`Prompt[R]`** is a first-class tag
  carrying the delimiter's answer type, `push` installs one (an
  OPERATION, not a handler — one machine must own the whole prompt
  stack, or a capture cannot cross an intervening delimiter), and
  `shift`/`shift0`/`control`/`control0` capture up to a NAMED prompt.
  The tags are what let several answer types share one row. `Delim.run`
  is the machine; the captured continuation is turned back into a
  PROGRAM, so it is an ordinary value and multi-shot is free. With it
  a user can define new effects (a generator is a prompt and a shift)
  without touching the library. See specs/delimited-control.md.
- **`Async`** — `Run(thunk)` (blocking = a JVM/Native ability) and
  `Await(register)` (the universal callback form; the callback takes
  `Either[Throwable, A]` — the Left is the error channel and fails
  the program at that operation — and the registration answers a
  CANCELLER, so cancellation unregisters the timer/completion too;
  the simple top-level `await` wraps success-only registrations,
  `Async.await` is the full form). **`Fiber`** (onComplete/cancel
  everywhere plus `joinAsync` — the effect-world join, an Await;
  join/joinEither only under **`CanBlock`** evidence — absent on JS,
  so a blocking join is a compile error, not a frozen loop),
  **`Scheduler`** (takes the program: loom/forkJoin/threads in
  `Schedulers` on the JVM, the event loop on JS, one OS thread per
  fiber on Native; interop modules add cats-effect and ZIO
  instances), `runAsync` (the universal `Future` terminal — a
  while-loop drive with an atomic handshake per Await: the callback
  may fire during registration, on any thread, and whoever loses the
  exchange continues), `spawn/par/race/timeout/sleep` — all
  cross-platform: par pairs by completion and a child failure cancels
  the sibling; race's first SUCCESS wins, two failures fail it;
  `sleep` rides the platform **`Timer`**; **`bracket`** (any
  Handler-able row).
- **`Channel`** — the queue between fibers, and the primitive pull
  cannot express (readiness, pacing). `merge` feeds one channel from
  two sources by READINESS; `buffer` runs a producer ahead of its
  consumer. `fail` records a producer's error WITHOUT closing (the
  other source is still feeding) and `close` then ends the stream
  with it — so a consumer receives everything actually produced and
  only then hears that something broke. Before that existed, a
  producer that threw was indistinguishable from one that finished:
  the exception died on its own fiber, `finally` closed the channel,
  and a merge silently returned half its elements.
- **`Resource`** — the region: acquires release at the scope's end in
  reverse order, surviving handled aborts and mid-step exceptions;
  run it OUTERMOST.

## The typeclass hierarchy (Monad.scala)

- **`Functor` → `Applicative` → `Selective` → `Monad`**, plus
  **`Alternative` → `MonadPlus`** and **`Comonad`** (the basis of
  per-operation handlers: `given [F: Comonad]: Handler[F]`).
  `ParaMonad` founds the Cont layer; every diagonal is a `Monad`.
- The GENERIC combinators the classes exist for — written once, they
  run over programs, LazyList, Choose searches: **`traverse`** /
  **`sequence`** / **`replicateA`** (Applicative), **`guard`**
  (MonadPlus — the pruning conditional of backtracking),
  **`*>`/`<*`** (sequence and pick a side), **`whenS`/`unlessS`**
  (Selective: the branch is DECLARED statically, run at most once),
  **`>>>`** (Kleisli composition).
- `Selective`'s `ifS`/`branch`/`select` sit between Applicative and
  Monad: both branches visible, at most one runs.

## Streams and consumption

- **`Stream[S[_], F[+_]]`** — codata: `uncons: Option[(A, S[A])] ! F`.
  Consumers need `Handler[F]` (free for `Pure`; Async pulls park).
  `toLazyList` (memoized bridge), `iterator` (linear, fused;
  specialized per instance). Combinators (`filter/take/zip/++/...`)
  land in LazyList; `Stream.map/flatMap/fold` are spelled explicitly —
  the postfix names belong to the monad.
- **`Fold[A, S]`** — the left-fold algebra (`Fold(z)(step)`, `count`,
  `sum`, `first`, `last`; every `Monoid` gives one). **`Foldable`** —
  the push side. **`Monoid`** (`|+|`) and **`Group`** (adds `inverse`,
  `|-|`) — a sliding window (`sliding`) requires Group and rejects
  Monoid-only elements at compile time.

  Two ways to spend one, and the difference is measured. `Chunks.fold`
  takes a `Fold` as **data** — an `Aggregator`'s, a java `Collector`'s,
  one chosen at run time — and `Chunks.foldLeft(p)(z)(f)` takes the
  step at the **call site**, where `inline` can beta-reduce it into the
  loop. Per 10k Longs in chunks of 64: 38.2us against 7.0.
- **`Fold.OfLong` / `OfInt` / `OfDouble` / `OfBoolean`** — the same
  algebra with the accumulator declared where it is already primitive,
  for the data path that has nothing to inline. `Fold.long(z)(f)` and
  friends build one; `count`, `sumLong`, `exists`, `forall` are ones.

  Why a differently-named `addLong` rather than an override of `add`:
  **erasure is fixed at the declaration**. `add(s: S, a: A): S` is
  `(Object, Object)Object` in the generic parent and stays that way in
  every subtype, so re-declaring it at `S = Long` would be the same
  symbol and the same boxing — the reason the JDK has
  `LongBinaryOperator` next to `BinaryOperator<Long>`. Only the
  accumulator is specialized: measured, it is essentially the whole
  cost (29.4us against 2.8 for boxing the element read instead).
  `Chunks.fold` dispatches on the four shapes, and GADT refinement
  hands `S` back from the type test, so the dispatch needs no cast.
- **`Aggregator[-In, Acc, +Out]`** — init/add/**merge**/present; the
  merge is `(zero, seqOp, combOp)` — the distributed contract; `zip`
  is one-pass composition; `Serializable` so it ships as Spark tasks.
  **`Sketch`** — HyperLogLog, Count-Min, t-digest: approximate
  monoids with stated error. Their state is flat arrays mutated in
  place, with `init` allocating fresh and `merge` allocating its
  result — the two rules that make in-place accumulation safe under
  the same contract Spark's `seqOp` has. The persistent-`Vector`
  versions they replaced cost 3x, 12x and 580x respectively.

- **`Bulk[D[_]]`** — a collection too large to be in one place, as a
  typeclass: `of`, `csv`, `map`, `flatMap`, `filter`, `join` (the
  equi-join), `cache`, `aggregate(agg)`, `toChunks`. Instances:
  `Bulk[Chunks]` (core, files read by scala-jvm), `SparkBulk.Rows`
  (okay-spark, an opaque `RDD[Any]` — one documented cast, no
  `ClassTag` per intermediate type), `java.util.List` over parallel
  streams (okay-java `Parallel`). The extension methods are the
  collection view for code generic in `D`; a concrete `Chunks` is a
  program and its own `map` wins, so local code calls `B.map(d)(f)`.
  **`Csv`** — the RFC 4180 subset on one line, `Row = Map[String, String]`.
- **`Tables[+A]`** — the same road as an EFFECT: `Of`, `Read`, `Select`,
  `Expand`, `Where`, `Join`, `Cache`, `Aggregate`, `Collect`, answering
  `Table[A]` — an opaque slot on the handler's heap (`Refs.Ref`).
  `Tables.via(B)` translates into `State % Heap[D]` through any
  `Bulk[D]`; `Tables.run(B)(prog)` runs a plan on a platform. A plan is a
  value: `!.tracing` prints it. **`Sort`** — an operation `Bulk` does not
  have, added as a signature in the row: `Sort.viaTables` (through the
  primitives, any platform) or `SparkBulk.sort` (native). Direct style
  binds handles with a mark: `val deps = !departures.cache`.
  **`Tables.Plan[A]`** — the first-order tree a building operation puts
  on the heap (`Of`, `Read`, `Columns`, `Select`, `Expand`, `Where`,
  `Join`, `Held`); an action forces it — `Plan.optimize` (a `Columns`
  into its `Read`, the smaller join side to the right by
  `Plan.estimate`) then `Heap.compile` through the instance.
  `Plan.show` prints it.

  `fold` is the seam the specialization travels through, so it is not
  final: **`Aggregator.OfLong` / `OfDouble` / `OfInt`** override it to
  hand over the matching `Fold.OfX`. `count` is one; `sum` selects one
  by `Numeric`. Accumulators are flat — `Aggregator.Mean` and
  `Aggregator.Variance` are case classes with primitive fields, where
  a `(N, Long)` and a `(Long, Double, Double)` used to cost three and
  four allocations per **element** (the tuple, plus a box per field,
  since a tuple's fields are `Object`). Per 10k: count 37.8 -> 19.5,
  sum 40.8 -> 18.5, mean 87.0 -> 37.3, variance 90.9 -> 74.7.

  `Numeric` cannot specialize anything — `plus(x: T, y: T): T` erases
  exactly like `add` — but it can **say** which type this is, and the
  `=:=` that says it also transports the fold: `substituteCo` at
  `[X] =>> Fold[X, X]` turns a `Fold[Long, Long]` into a `Fold[N, N]`
  with no cast, because they are provably the same type.
- **`Chunk[A]`**/**`Chunks[A]`** — array batches / a producer of
  them; generators fill chunks in while-loops (no tree node per
  element); transformers are chunk-in, chunk-out; `a merge b` rides
  Channel (bounded at 64 by default — an endless source merged
  unbounded is the heap). **`Pipeline[A]`** — the reified operator tree with
  `optimize` (fusion, pushdown) and `chunks` (compile).
- **`Take % V`** / **`pipe`** — the consumer dual of Writer and the
  coroutine pairing. **`Stage[I, O, A]`** — a transducer as a program;
  `through` composes demand-driven; `Stage.id/chunked/unchunk`;
  `Stage.transduce(z)(step, end)` — the state-step-flush skeleton
  every stage here shares (the two functions share ONE parameter list
  so the types infer; a third list commits `I` to `Any` before the
  lambda is typed); `Stage.mapAccumulate` — the 1:1 special case.
  **`Source[W]`** = `Unit ! Writer % W + Async` — the asynchronous
  stream as a program; `Source(a, b, c)`, `Source.of(stream)`,
  `Writer.of` (any stream, effects kept), `Writer.map` (re-tell at
  another type), `a merge b` (readiness, union element type, bounded
  default).
  Effectful rows compose too: the `throughG`/`throughProducerG`
  overloads forward arbitrary G ops from either side in the order the
  pull crosses them (a pure stage joins the row by `!.widen` and a
  union-ACI ascription).
- **`Staged`** / **`Push[A]`** — whole-stage codegen as an INLINE
  PROGRAM SHAPE (`range/gen/map/filter/take/drop/fold`): nested calls
  beta-reduce into one while-loop, 1.6us on the lane where Iterator
  takes 19.3. The tree (`Pipeline`) is for tools, the inline shape is
  for speed — a GADT cannot partially evaluate through `inline match`.
- **`Channel[A]`** — the bounded queue between fibers (park-based
  backpressure on JVM/Native; the JS variant is Await-based behind
  the same surface, capacity advisory); a LINEAR async stream;
  `merge` (readiness), `buffer`.
- **`Retry`** — policies as delay streams; `retry` (parks a thread:
  JVM/Native), `Retry.async` (the same as an Async program over
  `attempt` + `sleep`: every platform), `supervised`,
  `retryChunks` (per-chunk lineage recompute), `parMap` (a fiber per
  chunk).

## The text stack and above (their own modules)

- **Dialects** (okay-codec) — four, and they cover the four ways a
  document nests, which is what makes them a test of the parser
  rather than a feature list: **`Json`** by punctuation, **`Yaml`**
  by indentation, **`Markdown`** not at all (hence REFRAMING —
  crossing emphasis closes and reopens), **`Xml`** by NAMED tags (the
  only one where a close can be WRONG: mismatched closes mark the
  unclosed, a close with nothing open is an error leaf, void elements
  never open). All lossless and total, checked under generated input.
  **`Cbor`** is the binary algebra over the same `Schema`.
- **`Schema.SBytes`** (okay-codec) — raw bytes as a PRIMITIVE of the
  algebra, because CBOR has a first-class byte string and JSON has
  none: without it every binary payload gets smuggled through a text
  or number field, which is how an embedding index came to persist as
  `List[Double]` at nine bytes and two boxed objects per component.
  Writes as a CBOR byte string, as base64 in JSON (where a dump gets
  MORE readable — one token instead of 1536 float literals), and as
  `contentEncoding: base64` in a tool's JSON Schema. The cost it
  carries honestly: `Array[Byte]` has reference equality, so a product
  holding one is not a value for `==`.
- **`Structured.cut`** (okay-llm) — validate a structured answer as
  it streams and STOP when it is complete: each token is an append,
  which is an edit, so the incremental parser costs the token; not
  pulling further IS cancelling generation.
- **`Corpus`** (okay-rag) — the sources segments point into, which is
  what makes a passage lineage: `widen` grows it, `whole` returns the
  document, `current` detects an index that drifted from the file.
- **`Grounded.translating`** (okay-agent) — grounding as
  `Context ==> ([X] =>> X ! F)` rather than `Context ==> Id`. The
  comonadic handler must ANSWER, so it must finish, so its retriever
  must already be pure; valued in a program, `Recall()` may hand back
  a retrieval that suspends and `!.translate` forwards it outward.
  The three handler forms, once more, in one concrete place.
- **`Similarity`** (okay-rag) — a function, not a typeclass, and the
  general rule for this layer: a typeclass asserts canonicity, and a
  program holds several stores, several retrievers and possibly two
  metrics. `Handler` is a typeclass precisely because a row IS
  canonical where it is discharged.
- **`Language`** (okay-rag) — a language as DATA: comments, strings,
  the words that introduce a definition, and `Layout.Braces` or
  `Layout.Indent`. `Code.scanner` and `Code.driver` are functions of
  it, so a new language is a nine-field value and not a grammar. Only
  workable because the parser is total: an imperfect description
  degrades into ordinary leaves. The indent driver is the `Yaml`
  indent stack one level up — the same distinction, at the scale of
  definitions rather than mappings.
- **`Language.text`** (okay-rag) — the prose fallback, and a
  reminder that a grammar applied to the wrong material is worse than
  none: under Scala's rules a README saying "the type of a given
  value" opens two definitions.
- **`Large.projecting`** (okay-agent) — the same doctrine for tool
  output: a result over the limit is stored whole, the context gets
  its head plus a handle, and `expand` reads any window later.
- **`Durable`** (okay-agent) — the journal is intent-first and the
  recovery decision is per operation (`Redo`, `WithKey`, `Reconcile`,
  `Escalate`, `Fail`); `replaying` re-runs an incident offline.
- **`Provider`** (okay-agent) — `openAi` and `anthropic` are both
  `Handler[Model]`; `relay`/`openAiRelay` are the PORTABLE form,
  since a comonadic handler cannot do I/O where nothing may park.
- **`Chunks.ofChars`** — a string as chunks without boxing (a
  primitive `Array[Char]`); see the benchmark note about what it did
  and did not buy (8%, where 23% was predicted).
- **`Embedding`** (okay-rag) — `ArraySeq[Float]`, not `Vector[Float]`:
  the same boxing question asked one module along, and this time the
  answer was 11.3x on a cosine and 10.4x on a corpus scan, tying a raw
  `Array[Float]`. The two results are not in tension — a scoring loop
  reads three components per iteration and does nothing else, so
  per-element cost IS the cost; where there is real work per element
  it disappears into it. Which is why the rule is to measure, not to
  generalize from the last measurement.

## The build compiles with zero warnings, under `-Wall`

It reported 626 at the start of the cleanup. None of the difference is
blanket suppression; the categories and what each turned out to be:

- **199 "type test cannot be checked at runtime" → 0.** Mostly FALSE.
  A signature whose only parameter is its erased answer type has the
  class as its whole identity, so the test is total — `typeableK` says
  that once per signature. Where the limitation is real (`Reader`,
  `State`, `Take`, `Throws` keep no runtime trace of their parameter)
  it is named and `TestRowIdentity` demonstrates it — and it binds the
  BARE row only: [several instances of one
  effect](many-instances.md) are had by key (`Tag`), by cell (`Refs`)
  or by prompt (`Delim`).
- **100 "match may not be exhaustive" → 0.** All one claim: `resume`
  normalizes two of `Free`'s cases away, so a three-case match is
  correct and the type cannot say so. Written `(x.resume: @unchecked)`
  at all 42 sites, with the invariant explained where `resume` is.
- **77 "unused value" → 0.** Two real bugs among them (a producer's
  failure lost in `Remote.listen`; a rejected `fetch` on JS that
  called no callback at all, so the program waited forever). The rest
  were deliberate discards, now written `val _ = …`, which is the
  form that says so.
- **9 "Unstable inline accessor" → 0**, and this was the one that
  mattered for publishing: an `inline` method reaching a privately
  captured given makes the compiler synthesize an accessor whose name
  is unstable across compiler versions, so a downstream JAR could
  break on a mere recompile. `DiagonalMonad` and `ComonadHandler` are
  named classes with a public member instead — the `inline` is kept.
- **178 unused imports → 0**, mechanically.
- Two lints are filtered in build.sbt with the reason written there:
  the interpolation lint (every occurrence is a diagnostic message,
  where the value's own toString is the point) and the safe-init
  checker on munit's `test(…) { … }` (the framework's shape, nothing
  at the call site to change). One `@nowarn`, on a cats given whose
  unused `using` is load-bearing for resolution — removing it makes
  the instance ambiguous, which a test caught.

## Where the unchecked casts live, and why they are there

A cast the type system cannot verify is a claim, and a claim scattered
across twenty call sites is a claim nobody can audit. So each family
has ONE named function, in the file that owns the equation it asserts,
and nothing else in the library casts for that reason:

- **`okay.out` / `okay.answer`** (Writer) — `opaque type Writer[W, +A]
  = W`, and `Writer(w): Writer[W, W]` is the only injector, so an
  operation IS its element and its answer type equals it. `out` needs
  no cast at all (inside the file the opaque type is transparent);
  `answer` asserts the phantom equation once. Making `Writer` a GADT
  would let the compiler check it and cost an allocation per `tell` —
  which is the whole of why it is 286ns against cats' 1127.
- **`okay.produced`** (Produce) — the same equation for the identity
  signature the streams are built on.
- **`Chunks.bound`** — the element under a `Bind`, which is the BIND's
  intermediate and genuinely existential. `case Effect(c)` needs
  nothing: GADT refinement gives the type back.
- **`ChunkBuf.update` / `.chunk`** — the array assertion, once, with
  four measured alternatives recorded against it.
- **`<|>`** and **`split`** — the union split, sound by the excluded
  middle of `F[A] | G[A]`, documented as the trusted kernel. `<|>`
  answers an `Either` (a value, for walks that pass it on); `split`
  takes the two branches as `inline` continuations and answers
  nothing but their result — no Either, no Option per operation —
  and is what the hot loops use (`State.handle`, `Writer.foldWith`,
  `relay`, `Effects.handle`, `Handler.union`; split-without-either,
  2026-09-09, measured to the byte in specs/handler-fusion.md). In a
  RETURNING arm of `split`, ascribe the loop's answer inside the
  branch: the constructor has refined the answer type there, and the
  ascription is where the refined value meets the loop's type. Both
  are ONE method each since 2026-09-11 (see "Two type clauses" below);
  the value classes that used to carry the test between two stages are
  gone, and no call site changed.
- **`over`** — the row PRISM, `split`'s reverse direction: rewrite the
  operations of one member of a row in place (`over[F, R](e)(f)`,
  `f: F[A] => F[A]`), leaving the others as they are. The class test
  proves the operation is an F, `f` keeps it one at the same answer
  type, and the row is erased — one cast, beside `split`'s, for what
  no witness can say about an abstract row. It is how a typeclass
  instance written for ONE effect becomes the instance for every row
  holding it: `Failing.anyRow` (Resource's forwarded-failure hook, the
  TOTAL default of the recipe below) is `Failing[Async]` lifted by
  `over`, and Failing.scala itself casts nowhere (failing-over,
  2026-09-09). The alternative to a total default was measured and was
  SILENCE (see the recipe).
- **Two type clauses, and what it is worth** (generalized-method-syntax,
  2026-09-11). Scala 3 allows a method to take type parameters in more
  than one clause, so some may be written and the rest inferred. The
  rule that decides where it applies: **two type clauses may not be
  adjacent** — a term or `using` clause must separate them, which our
  row combinators already have as a context bound. Three uses here:
  - `split`, `over` and `<|>` are single methods. They were a method
    plus a value class each, whose only purpose was to make `A`/`R`
    inferable while `F`/`G` were written. Call sites did not change,
    and the bytecode did not either: no `invokedynamic`, branches
    beta-reduced, checked with `javap` against the old form before the
    change was made.
  - `Effects.handle[F, G](m)(ret)(h)` and the `Tag` trio take their
    ROWS first and read the answer types off the program:
    `Tag.tag["small", State % Int](p)`, which is the syntax Tag's own
    doc comment had been showing since the day it was written, before
    the compiler could give it.
  - **and the four combinators that look identical do NOT get it, by
    measurement.** Splitting the clauses puts a `using` between them,
    and that clause is resolved BEFORE any value argument is typed —
    so the first clause's parameters stop being inferable and become
    mandatory. `!.tracing(prog)(show)` turns into "Ambiguous given
    instances ... TypeableK[F]", because F is still a variable when
    the context bound is searched. The rule that follows: **the
    reorder is a win only where EVERY call site already writes those
    parameters, and a loss anywhere inference is used.** Counted
    before deciding: `tracing` 0 explicit against 8 inferred,
    `interpret` 3 against 9, `translate` 10 against 5, `relay` 7
    against 1 — all four keep one clause; `Effects.handle` 21 against
    0 and the `Tag` trio all-explicit — both take two.
  - `State.handle[Int](0)(p)`, where the separator is the state
    itself rather than a using clause.
  What it CANNOT do, measured before the work: a row inferred from a
  single OPERATION widens (`op: F[A]` gives `[X0] =>> St[Int, Int|X0]`,
  not `St % Int`), which is why `Tag.one` still names its row; and
  `pure[F, A]` cannot be split at all, since nothing separates `F`
  from `A` — its 128 call sites keep both arguments.
- **`into`, on exactly one type** (throws-into, 2026-09-11). Scala 3.9
  lets a type declaration say "conversions to me are allowed", so the
  caller no longer writes `import scala.language.implicitConversions`
  per call site. It may be written only on a class, a trait or an
  opaque type alias, and it marks the conversion's TARGET — which is
  the whole of where it applies and does not:
  - `throws` TAKES it. It is an opaque alias whose design is absorbing
    four shapes (a value, a raw error, an `Either`, a `Try`), the four
    conversions were already written, and the import was pure
    ceremony. The fifth conversion, which went the other way
    (`A throws E => Either[E | Unsafe, A]`), was DELETED with it: its
    target is `Either`, which is not ours to mark, so it was the one
    thing still demanding the import — and it was redundant, being
    `.wrap`, which is public and used explicitly 38 times. Eliminating
    is an act now, not a coercion.
  - `Direct` CANNOT take it, and this is where four of the
    repository's five language imports are. Its conversions are
    `Conversion[F[A], A]`: the target is a bare type variable, and
    `into` marks a declaration. There is nothing to write it on.
    Auto-coloring keeps asking the language for consent, and should.
  - `Json` was REFUSED although it fits mechanically (`into enum`
    compiles, and ~850 construction sites of `JStr`/`JNum`/`JBool`
    would become bare literals). A `Conversion[String, Json]` turns an
    already-serialized document into a JSON string LITERAL with no
    error anywhere — double encoding, in the module whose job is
    encoding. The feature's own advice is the same: restrict `into` to
    the absolute minimum, and never write it in case someone might
    want a conversion later.
- **No `Tagged`, and the reason is worth more than the type was.** An
  existential package — a value with its `ClassTag` beside it — turns
  an unchecked cast into a checked one, and is the right tool for
  something stored heterogeneously and read back at a GUESSED type. It
  was built, tested, and then found to have no home here: every
  candidate turned out to be a GADT, where refinement removes the cast
  outright and no check is needed. `Durable`'s journal looked like the
  clearest case and was not — it stores a `String`, and `Tool.Call
  extends Tool[String]` proves the type. Two facts from the attempt
  survive it: a `ClassTag` names a CLASS, so
  `ClassTag[Chunk[Int]]` and `ClassTag[Chunk[String]]` are both
  `ArraySeq` and such a check cannot distinguish element types at all;
  and packing a tag WITH an existential does work, which is what
  `Pipeline.Mapped` and `TaggedBuf` do — the tag captured where the
  type was still concrete, not guessed where it is not.

What is not on this list is deliberate: GADT refinement removes casts
outright wherever the ADT records the type (`Schema`, `Context`,
`Model`), and 35 were removed that way rather than named.

## Recurring gotchas

- Postfix `.map`/`.flatMap` on program carriers are the MONAD's (they
  transform the answer, not stream elements) — elementwise operations
  are spelled `Stream.map`, `Chunks.map`, etc.
- `Comonad[Id]` puts `map`/`extract` on every type in package scope —
  when a foreign `.map` misbehaves, use flatMap or qualify.
- Same-name extensions in different files of one package are NOT
  overloads; toplevel defs across files cannot overload either (that
  is why the stream `take` lives beside the Loop `take`).
- Union splitting is by runtime class: forward only effects whose
  operations are class-distinct from identity-signature values.
- Satellite modules need `import okay.given` for the extension methods
  of package-level givens (`runWith` above all).
- `inline match` does NOT reduce through pattern-bound subtrees: a
  GADT operator tree cannot drive partial evaluation — the staged
  artifact must be an inline program shape (`Staged`, `Control[M]`).
- Two files with the SAME NAME in one package cannot both hold
  top-level definitions — they collide on the synthesized
  `<name>$package` object (why the platform halves of Async live in
  `Platform.scala`, not a second `Async.scala`).
- A poly-function literal (`[X] => ...`) cannot be passed with the
  colon-argument syntax — parenthesize the call.

## The edge patterns: linear context without nesting (ctx-edge-docs)

Two verified styles for application-edge code (experimental base:
specs/context-functions.md, E1-E8):

**The type-changing given-chain (E3)** — phases as types, each line
seeing the previous line's context:

```scala
given Conn   = connect()      // load -> resolve -> connect -> ...
given TxOpen = begin()        // sees Conn
given TxDone = commit()       // sees TxOpen
```

No nesting; the compiler orders the protocol. Honest hole: STALE
phases stay in scope (use-after-commit compiles) — discipline, not
types, until capture checking.

**The import-thread (E6/E7)** — SAME-typed context evolving
linearly, via a holder whose given member has a FIXED name:

```scala
class Step(c: Ctx) { given ctx: Ctx = c }
val s1 = step("one");   import s1.given
val s2 = step("two");   import s2.given   // sees s1's ctx
```

Mechanism: NAME shadowing (different member names restore
ambiguity — E7). FOOTGUN, stated: a forgotten `import sN.given`
silently uses the stale context; there is no error.

## The row-typeclass recipe: a typeclass over `F + G` (row-typeclass-recipe)

A typeclass indexed by a ROW — `Failing[F]` is the worked example, and
`Handler` is the older one — cannot be derived the obvious way, and the
reasons are measured rather than argued:

- **An unanchored `given [F[+_], G[+_]]: TC[F + G]` does not work.**
  dotty selects it and then cannot pin `F`: splitting needs
  `TypeableK[F]`, and against a free `F` that query is ambiguous
  (`TypeableK[Vector]` and `TypeableK[Op]` both match). `Handler`
  meets the same wall one step earlier and worse — an implicit row
  given enters scope for EVERY `Handler` query and crashes the 3.7.1
  type comparer — which is why `Handler.union[F, G]` is called BY
  NAME at a concrete call site and never given implicitly.
- **Anchor the instance on the CONCRETE effect instead.**
  `given [G]: TC[Async + G]` and `given [F]: TC[F + Async]` pin
  everything: the split runs on `Async`'s own `TypeableK` through the
  kernel `<|>`, the branches come back by plain upcast, and no cast
  appears. This is the typed road, and it covers `Async`, `Async + G`
  and `G + Async` — which is every row a `Resource.run` in this
  repository passes today.
- **The anchors are NOT the whole story, and the gap is silent.**
  `A + B + C` nests to the left, so `(Async + S) + P` is not
  `Async + ?G` to the implicit search and no anchored instance
  matches. With only anchors plus an identity default, such a row
  compiles, resolves, and does NOTHING — the finalizers are abandoned
  exactly as before the fix, with no error anywhere. That was measured
  (TestFailing) after a first probe read `summon` succeeding as
  "resolved" when what had answered was the identity.
- **So the default must be TOTAL, not typed.** `Failing.anyRow` tests
  the OPERATION's own class rather than the row's shape — through the
  kernel's `over`, which casts once — and is correct for every
  nesting.
- **And then the anchored ROW instances are decoration — delete
  them.** They were written and they work; once the default is total
  they answer nothing it does not answer the same way, at the same
  cost (the anchored road also runs a class test, inside `<|>`). What
  is worth keeping from that road is the SINGLE-EFFECT instance —
  `Failing[Async]` — because one effect is a shape the compiler pins,
  it is what most call sites pass, and it needs no cast. Two
  instances, not four (failing-simplify, the operator's call).
- **The total default's cast is the kernel's, not the typeclass's
  (failing-over).** Asked to avoid the cast, or at least to move it
  under an implicit, the implicit road was probed first: a
  `RowLift.In[Async, F]` witness plus a `NotGiven` identity. `In`
  walks the left spine only, so `(S + P) + Async` and a right-nested
  row resolve no witness and would take the identity; and on an
  ABSTRACT `F` — a polymorphic `Resource.run` caller — `NotGiven`
  reads "unknown" as "absent". Refuted twice, before a line was
  written. What moves is the cast: `over[F, R](using TypeableK[F])[A]` in
  Effects.scala is the prism over the row — test, rewrite, back under
  the row's type — and `Failing.anyRow` is
  `over[Async, F](e)(Failing.async.guard(_, onFailure))`: the typed
  instance lifted over any row, the logic written once, no cast in
  the typeclass.

The rule that survives all of it: **a typeclass over rows gets a TOTAL
default that reads the value — the single-effect instance lifted by
`over` — plus typed instances only for the shapes the compiler can pin
and that callers actually pass.** An identity
default is the one thing to refuse — it turns a type-level miss into a
runtime silence. And the corollary that cost this repository two
lanes: prove an instance by CALLING it, never by `summon` succeeding.

## The capability recipe: adding a door to any API (ctx-everywhere)

The pair is `provide` (core: expression-scoped installation,
nearest-wins nesting) and DOORS — and together they are the
dependency-injection story: compile-time resolution, given-scopes
as the object graph, zero framework. The composable form
(`providing[A](a) and providing[B](b)`, core Providing.scala)
builds installers as reusable values with no nesting and no arity
cap; the right operand of `and` is the inner layer, so it is the
override story as data. One trap: a conditional LAYER does not
typecheck (`if debug then providing[Log](v) else base` — the branch
types differ); make the VALUE conditional inside one installer
instead: `providing[Log](if debug then verbose else quiet)`.
The consumer one-liner is `wire[A]` (Reader's ask): `wire[Db].q`
pulls the nearest given — E10's eagerness working FOR us — and a
missing given is still a compile error. The full story —
vocabulary, theory, boundaries — is [capabilities](capabilities.md). And the generic
combinators run over context functions: `sequence(Seq[Env ?=> A]):
Env ?=> Seq[A]` via `ctxMonad` (core), F inferred. Adding a door is two lines:

```scala
// a wrapper-taking API:                      // a factory:
def granted(...)(route: Principal ?=> R): R  def wired(...): Http ?=> Engine =
  = explicit(...)(p => route(using p))         explicit(summon[Http], ...)
```

Rules that keep it honest: the door goes where the parameter is an
ENVIRONMENT type (Http, Secrets, Crypto, ChatModel, Store, Tracer,
Principal, Prompt) — a per-instance RESOURCE (a Connection, a
socket) stays an argument, because ambient resources are how leaks
happen; no newtypes are invented for string params; the explicit
form always stays. One trap (E10): a context function EAGERLY
auto-applies at ascription sites — bridges into other worlds must
be FUNCTIONS with `?=>` parameters, never bare Conversions.
