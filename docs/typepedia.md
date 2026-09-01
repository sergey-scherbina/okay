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
  splits unions (`<|>`); identity-style signatures are split by the
  runtime class of their values, so keep them class-distinct.
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
  type-changing (typestate) variant on the paramonad, ~1.7x the price.
- **`Throws % E`** — typed aborts; `runEither/runThrows`; the `throws`
  union type for direct style.
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
- **`Retry`** — policies as delay streams; `retry`, `supervised`,
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
  it is named and `TestRowIdentity` demonstrates it.
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
- **`<|>`** — the union split, sound by the excluded middle of `F[A] |
  G[A]`, documented as the trusted kernel.
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
