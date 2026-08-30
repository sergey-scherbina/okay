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
- **`Handler[F]`** — the comonadic (per-operation) handler;
  `runWith` runs with it. **`TypeableK[F]`** — the runtime test that
  splits unions (`<|>`); identity-style signatures are split by the
  runtime class of their values, so keep them class-distinct.
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
- **`State % S`** — bespoke tailrec handler; **`PState`** — the
  type-changing (typestate) variant on the paramonad, ~1.7x the price.
- **`Throws % E`** — typed aborts; `runEither/runThrows`; the `throws`
  union type for direct style.
- **`Choose`** — nondeterminism; the handler is genuinely multi-shot;
  the canonical `MonadPlus`.
- **`Async`** — `Run(thunk)` (blocking = a JVM/Native ability) and
  `Await(register)` (the universal callback form). **`Fiber`**
  (onComplete/cancel everywhere; join/joinEither only under
  **`CanBlock`** evidence — absent on JS, so a blocking join is a
  compile error, not a frozen loop), **`Scheduler`** (takes the
  program: loom/forkJoin/threads in `Schedulers` on the JVM, the
  event loop on JS, one OS thread per fiber on Native; interop
  modules add cats-effect and ZIO instances), `runAsync` (the
  universal `Future` terminal — drives the tree through callbacks),
  `spawn/par/race/timeout/sleep` (`sleep` rides the platform
  **`Timer`**), **`bracket`** (any Handler-able row).
- **`Resource`** — the region: acquires release at the scope's end in
  reverse order, surviving handled aborts and mid-step exceptions;
  run it OUTERMOST.

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
- **`Aggregator[-In, Acc, +Out]`** — init/add/**merge**/present; the
  merge is `(zero, seqOp, combOp)` — the distributed contract; `zip`
  is one-pass composition; `Serializable` so it ships as Spark tasks.
  **`Sketch`** — HyperLogLog, Count-Min, t-digest: approximate
  monoids with stated error.
- **`Chunk[A]`**/**`Chunks[A]`** — array batches / a producer of
  them; generators fill chunks in while-loops (no tree node per
  element); transformers are chunk-in, chunk-out; `mergeChunks` rides
  Channel. **`Pipeline[A]`** — the reified operator tree with
  `optimize` (fusion, pushdown) and `chunks` (compile).
- **`Take % V`** / **`pipe`** — the consumer dual of Writer and the
  coroutine pairing. **`Stage[I, O, A]`** — a transducer as a program;
  `through` composes demand-driven; `Stage.id/chunked/unchunk`.
- **`Channel[A]`** — the bounded queue between fibers (park-based
  backpressure); a LINEAR async stream; `merge` (readiness), `buffer`.
- **`Retry`** — policies as delay streams; `retry`, `supervised`,
  `retryChunks` (per-chunk lineage recompute), `parMap` (a fiber per
  chunk).

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
