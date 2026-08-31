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
