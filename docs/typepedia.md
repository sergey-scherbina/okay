# Typepedia

Every public type and typeclass of the core, with its meaning and its
gotchas. Source files are authoritative — their doc comments carry the
same material with the measurements attached.

## Control (Cont.scala)

- **`Cont[A, S, R]`** — the parameterised continuation monad,
  `(A => S) => R` defunctionalized — as the freer tree itself: an
  opaque `Free[Shift, A]` inside `object Cont`, a shift being a leaf
  whose payload is a function of the continuation, `S` and `R`
  phantom to the tree and carried by the facade's signatures
  (theory ch. 11). Running is `Cont.step`, tail-recursive with
  left-nested binds rebalanced and `Delay` forced; a fresh leaf
  absorbs its first `flatMap` (`Leaf.Absorbed`/`Mapped`), exactly
  once. The erased leaf type is written once, behind `Shift.of`
  (forget, an upcast) and `Shift.at` (remember, THE cast).
  `Cont.delay` is the tail call. The foundation everything stands on.
- **`Control[M[_,_,_]]`** — final-tagless delimited control (`shift`,
  `reset`, `/`); instances `Cont` and `Func`. `transparent inline def
  Control[M]` is the staging entry: resolved statically, the ops
  inline away.
- **`A /> S`** — `Cont[A, S, S]`, the diagonal. **`Loop[A, R]`**
  (alias `<<`) — `Cont[A, R, A => R]`: open recursion; `take` is the
  loop's input, `loop` ties the knot. Its `seed(body)` spelling is an
  `apply` extension on EVERY type, and it carries
  `NotGiven[A <:< NamedTuple.AnyNamedTuple]` for a measured reason: a
  named tuple's field access desugars to an apply BY INDEX, so without
  the guard `import okay.*` turned `t.route` into "Found: (0 : Int)"
  and disabled named tuples for anyone importing the package
  (named-tuple-unblock, BUGS.md). Guarding on `Tuple` does not work —
  a named tuple is not `<:<` one. The general lesson is in the entry:
  a universal `extension [A](a: A)` competes with whatever the
  compiler desugars into an apply on an arbitrary type.

## Effects (Effects.scala, Free.scala)

- **`A ! F`** — a computation of `A` over the signature `F` (a freer
  tree: `Return | Inject | Bind | Delay`, the node an operation sits in
  being `Inject` — there is no `Effect` alias any more, that word is
  the `derives` marker). `Free.resume` is the one rotation, a member;
  `Delay` is the trampoline (`!.tailcall`, a capturing handler, the
  codecs past `NativeThreshold`); `Free.defer(t)(f)` is
  `Bind(Delay(t), f)`. **`%`** fixes a binary signature's parameter (`State % Int`);
  **`+`** unions signatures; **`Pure`** (= `Nothing`) is the empty
  signature (the tree's answer node is `Return`, since
  free-return-rename — nothing shadows this name any more).
- **`!.loop(s)(f: S => Either[S, A] ! F): A ! F`** — `tailRecM` for
  programs (specs/fold-until.md, stage 2): continue from a `Left`,
  answer a `Right`; stack-safe because the recursive call sits inside
  the `flatMap`'s continuation and is made when the interpreter
  resumes that `Bind`. In `object !` beside `tailcall`, not top-level:
  Generate.scala's Cont fixpoint is called as `loop(f)(a)`, the same
  two-list shape. okay-ui's `Toolkit` dialogs are its first callers.
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
  `runWith` runs with it. **`Handler.flat[R]`** composes one handler
  per effect into a row handler as ONE dispatch expression (a macro
  over the row's members; 1.24x over the nested form at position 4 of
  a four-row, handler-fusion-flat); **`Handler.union[F, G]`** is the
  two-member combinator it generalises, kept for a row built one
  member at a time. Both are explicit, not givens: a given over a
  union type lambda crashes the 3.7.1 type comparer. **`TypeableK[F]`** — the runtime test that
  splits unions (`split`, and `<|>` as its `Either` form): `test`, a
  plain boolean, is its whole interface (the extractor form went with
  core-cleanup — nobody matched with it);
  identity-style signatures are split by the runtime class of their
  values, so keep them class-distinct.
- **the direct marks: `.reflect` / `.!?` / `.?` / `!prog`** — one
  mark, four spellings, one dispatch-by-type inside `direct { }`
  blocks (docs/direct-style.md): an `F[T]` of the block reflects, a
  row operation injects then reflects. `.reflect` is the word, `.!?`
  the postfix symbol, prefix `!` the one-glyph gesture (`unary_!`
  under the hood), and `.?` came BACK in unwrap-glyph (2026-09-17):
  it had been retired because `Throws.?` answered it on any value at
  all through the `into` conversion, silently doing nothing, and
  because the row peek held it too. The Throws glyphs now live in
  their type's companion where a converted receiver cannot reach
  them, and the peek is spelled `peek` — a word, for a method that
  RUNS operations through a Handler. Distinct from Monadic's `reflect`/`reify` pair below and
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
- **`Effects[M]`** — the interface; instances **`Free`** (initial)
  and **`Eager`** (opt-in, companion-scoped
  given: pure binds run at construction; the type is opaque so the
  encoding cannot leak into inference).
- **`!.relay`** — tail-resumptive handling; **`Effects.handle`** —
  abortive/multi-shot/forwarding; **`!.widen`** — effect-row
  subsumption (Free is invariant in its ROW, so it walks the tree;
  it is covariant in its ANSWER, so `Int ! F` is an `Any ! F` for
  free — free-answer-variance).

- **`Handled[Row, R, A]`** and **`Stager[Row, R]`** (Staged.scala;
  specs/direct-staged.md, direct-stagers.md) — a staged block's
  program: `Func`, the program as a function of its continuation at
  answer type `R`, with the row in the type so `Direct.staged` can read
  it. A `Stager` is the row's interpreter as an `inline match` over its
  constructors, applied by the macro to each operation as written, so
  the compiler picks the arm — no `split`. `Stager.All[E, S, W, Err, A]`
  stages `Reader % E + State % S + Writer % W + Throws % Err` in one
  layout (`E => (S, Vector[W]) => ((S, Vector[W]), Either[Err, A])`);
  a subrow puts `Unit`/`Nothing` in the slots it does not use. The
  singles `Reading`, `Stateful`, `Logging`, `Failing` carry one effect
  with the tuple removed; `StateWriter` is the canonical pair the
  numbers were taken on. Not stack-safe on a left-nested chain: a
  loop of millions is a Free block.

## The standard effects

- **`Reader % R`** — `Ask`, handled at relay speed. **Reading by the
  TYPE read** is `Reader.read[E, T]` (reader-read): `ask` with a
  projection through `Reader.Has[E, T]`, the accessor as a typeclass,
  derived for a tuple, for a product's fields and for the environment
  itself. The row still holds ONE Reader, `Reader.run` still handles
  it, nothing casts, and a type the environment does not hold does not
  compile. A component then declares exactly what it reads —
  `def banner[E](using Reader.Has[E, Users]): String ! Reader % E` runs
  in any environment holding `Users`. **Inside a `direct` block
  `!Reader.ask` needs no type argument at all** (reader-env): the
  environment comes from the block's ROW, so a block names it once and
  never again — `val (users, feeds) = !Reader.ask`. It is an overload,
  not a second name; `ask[R]` keeps working everywhere. Two witnesses
  make it work, both resolved at typer time while the row is still the
  alias the user wrote (`RowOf[F]` recovers the row from the block's
  program type, `EnvOf[R]` finds the Reader inside it), and `ask` is
  `inline` because the `DirectCtx` that pins the row is a value
  parameter of a lambda the macro strips. Four routes to the same need,
  and they answer different questions: one record (simplest), `read[E,
  T]` (by type), `HMap` (by key, so two values of one type), and
  `wire`/`providing` (context functions, not an effect). What does NOT
  work is two Readers in one row — `Reader % A + Reader % B` misroutes,
  `Distinct` says so, and `Tag.Of` is the answer when they must be
  separate members.
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
- **`Once`** — call-by-need for programs: `!.once(p)` runs `p` at
  the first demand and answers from a cell after, `Once.run` holds the
  cells as threaded state. `lazy val x = !p` in a `direct` block is
  this word. Multi-shot is handler order: `runChoice(Once.run(p))`
  backtracks the cells, `Once.run(runChoice(p))` shares one store
  (docs/direct-style.md, "Call-by-need").
- **`Logic`** — backtracking search over Choose (LogicT): `msplit`
  (first answer + the rest as a program — the one primitive), `cut`, `ifte` (soft cut), `gnot` (negation as failure),
  `interleave` (fair or), `fairBind`/`>>-` (fair bind), `observe(n)`
  (first n of an infinite search). A library over the effect, not a
  new effect. See specs/backtracking.md.
- **`Delim`** — delimited control AS AN EFFECT, multi-prompt **The typed door** is `Delim.Prompted[R]`
  (delim-prompted): evidence that a delimiter is installed, made only
  by `Delim.delimited` (or `Delim.scope`, its nested half), so a
  capture through the evidence-taking `shift` cannot name a prompt
  that is not on the stack — of the machine that installed it: ONE
  `Delim.run` per program, and the nested forms (`scope`,
  `collecting`, `pausing`) put a delimiter on the machine already
  running instead of starting a second one (delim-nesting).
  `Delim.collectUntil(using fo: FoldUntil[A, S, R])(body): R ! F`
  (collect-early-stop) runs the same producer `collect` runs and stops
  it where `done` first holds — the state passed on the way DOWN as
  the prompt's answer-function, the rest of the producer never run;
  `collectingUntil` nests. `Emitting[A]` is sealed with the two
  evidences behind it (list on the way back / state on the way down)
  and `emit` is one inline door over both. Inside a
  `direct` block it is `shift[A]` with ONE type argument
  (delim-one-type): the answer type comes from the evidence and the row
  from the block, and `A` stays because a mark gives its argument no
  expected type — `NoPrompt` moved to compile time
  for that path. A portable function reads `Prompted[Int] ?=> Int !
  (Delim + W)`: written apart, stored, passed, and callable only where
  a `delimited` put the evidence in scope. The obligation is NOT a row
  member: rows are unions and `Free` is invariant in them, so a body
  that does not capture to the prompt being installed could not be
  widened into the handler's row (measured; `push(inner) {
  shift(outer)(…) }` is ordinary and is in `TestDelim`). What stays
  runtime: evidence that escapes its own `delimited`.
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
  Monad: both branches visible, at most one runs. Their handlers are
  BY NAME, and in Scala that is the whole point — "at most one runs"
  is free in a lazy language, but here a branch is an ordinary
  argument and by value it does its work whether or not it is chosen
  (measured: a validator recorded the skipped check as having run).
- **`Validated[E, A]`** (Validated.scala) — `Valid | Invalid`, whose
  Applicative COMBINES two failures where `Either`'s keeps the first,
  and whose `Selective` is a real one rather than `selectA`: a `Right`
  scrutinee is already the answer so the handler is skipped, and a
  FAILED scrutinee does not run it either (the reference reading —
  which branch would have been taken is not known yet).
  `E` is a `Semigroup`, not a fixed `Seq`, so the caller decides what
  accumulation means (a vector for a form, a count for a sampler, a
  map keyed by field for an API). There is deliberately NO
  `Monad[Validated]`: the consistency law would force `app` to agree
  with the `flatMap` derivation, which stops at the first error, so
  the instance would quietly undo the collecting — `andThen` is that
  step under a name that says the branch is deliberate, and a test
  pins the absence as a compile error. First consumer: `okay-conf`'s
  `fromEnv`, which used to report one bad environment variable per
  run.
- **`Semigroup[A]`** (Fold.scala) — `combine`, and nothing about an
  empty. Split out of `Monoid` (which now extends it) for `Validated`:
  a list of problems being built has no "no problem", and asking for
  one turns `NonEmptyList` away for nothing. `Semigroup.fromMonoid`
  bridges given search, because the instances live in `object Monoid`
  and that is not in `Semigroup`'s implicit scope.
- **`Par[A]`** (Par.scala) — `A ! Async` read as ONE LEAF of an
  applicative spine: an opaque carrier inside `object Par`, whose
  `app` joins two leaves with `Async.par`. Choosing this instance is
  what makes generic applicative code concurrent — `Par.traverse` /
  `Par.sequence` are the named doors, `Par.map2` joins two leaves of
  different types, `Par(prog)` and `.seq` are the two ends. Do NOT
  write `.map` on one: `given Comonad[Id]` is lexically visible and
  beats an extension in `Par`'s own object, so it means the identity
  comonad's map and the next `.app` stops compiling (pinned as a
  compile error in TestPar). `fmap` deliberately does NOT fork (one leaf, nothing to run
  beside it), and there is deliberately no `Monad`: a `flatMap` would
  sequence the spine while the type still claimed independence.
  Cancellation is inherited from `Async.par`, symmetric since
  par-fail-fast (BUGS.md, `par-right-failure-waits` — found by this
  carrier's own test). Not to be confused with
  `parAll`/`parTraverse` (Parallel.scala): those are JVM/Native, flat,
  one fiber per leaf, joined in order — cheaper for a flat sequence,
  and measured so (theory ch. 12).
- **`direct` at an applicative-only carrier** (Direct.scala,
  specs/applicative-do.md) — the entry asks for `Applicative[F]`, not
  `Monad[F]`, and the macro summons the monad only where it emits a
  bind. A carrier that HAS one is unaffected, tree for tree. A carrier
  that refuses one on purpose — `Validated`, whose `Monad` would undo
  its collecting — can now be written in direct style, and a run of
  independent binds becomes the idiom bracket
  `fmap(m1, a => b => body).app(m2)`. Marks in the block's RESULT are
  leaves too, so `direct[V](f(a.reflect, b.reflect))` works. Refused
  by name, not by a missing-instance error at the call site: a
  dependent bind, a statement that is not a marked val, a mark inside
  a mark. THE SPELLINGS ARE ALL OPTIONAL: the type argument only where
  there is no expected type, and an effect is said by a mark, by a
  type annotation, or by nothing at all (a colourless val, whose
  inferred type is a program of the carrier).
- **`Direct.Binds`** / **`Direct.parallelBinds`** (Direct.scala) — a
  `direct` block's bind mode, taken the way `Deferral` is (a `using`
  parameter, default given in the companion, opt-in by importing an
  object's given). Under `import Direct.parallelBinds.given` a maximal
  run of two or more consecutive `val x = m.reflect` binds whose
  right-hand sides mention no name bound earlier in the run is emitted
  as N spawns then N joins — `parAll`'s FLAT shape, not `Par`'s
  pairwise one, because a macro holds the whole group and so never has
  to be pairwise. Needs a `Scheduler` at the call site (a clear macro
  error otherwise). A leaf must be exactly `X ! Async`, read BEFORE
  the mark narrows it into the row, so a block over a wider row
  parallelises its Async leaves too and anything else ends the run
  (it got nothing, quietly, until direct-parallel-wider-rows). What
  you take on: the leaves interleave, and a failure is seen at its own
  join rather than cancelling the siblings, which is `parAll`'s
  bargain and not a new one.
- **`Static[F, A]`** (Static.scala) — the FREE SELECTIVE: `Pure | Op |
  Ap | Select`, a program with no `Bind` in it, so its structure can
  be read before it runs. **`leaves`** lists every operation it MAY
  perform (both sides of every `Select` — an upper bound, exact
  without branches; an explicit stack, so a traverse-built spine of
  50 000 walks), **`toFree`** converts to `A ! F` for the ordinary
  runners (`Free.defer`, so no host stack at any depth) and makes the
  approximation good at run time (a `Select` performs at most one
  side), **`foldMap`** interprets the spine into any other
  `Selective` — the batching door: N leaves, one round trip. All three
  are stack-safe: `foldMap` walks a TYPE-ALIGNED `Args` in two
  tail-recursive loops, which is the reassembly other libraries do
  with an internal cast, done without one (50 000 leaves fold; it
  overflowed at 10 000 until static-foldmap-stack-safe).

## Optics (okay-optics: Optic.scala)

- **`Optic[C[_[_, _]], S, T, A, B]`** — a path into a value, as a
  value. It is a function polymorphic in a profunctor `P`, and its
  CONSTRAINT on `P` is a type parameter: composition takes the
  INTERSECTION of the two constraints, so `lens andThen prism` asks
  for `Strong[P] & Choice[P]` and an interpretation satisfies the
  meet by subtyping. Nobody writes a table of family pairs. Nominal
  on purpose — a transparent alias for the polymorphic function
  cannot have its constraint inferred by an extension method, which
  the prototype found by failing.
- The families, each one constraint: **`Iso`** (`Profunctor`),
  **`Lens`** (`Strong`), **`Prism`** (`Choice`), **`Affine`** (both,
  and usually not written — compose a lens with a prism and the type
  appears), **`Traversal`** (`Traversing`), and the two that
  AGGREGATE rather than iterate: **`Kaleidoscope`** (`Reflecting` —
  lift through any Applicative, collapse many focuses into one) and
  **`AlgebraicLens`** (`Classifying` — put by an algebra over all the
  wholes, which is what "decide what this is, given everything seen"
  needs).
- The lattice: `Profunctor` → `Strong` (`first`) and `Choice`
  (`right`); `Traversing` extends both and adds `wander` over a
  `Walk` (Purescript's, applicative-polymorphic). Each class DERIVES
  its operation from its structure map with the textbook formula as
  the default — `Strong.lens` from `first`, `Choice.prism` from
  `right`, `Traversing.eachVector` from `wander` — and an
  interpretation may override with a direct road. TestOptics holds
  every override to the default it replaces; that is the house rule,
  not a courtesy.
- The interpretations (they ride `import okay.given`):
  **`Function1`** (`Traversing`: `modify`, `set`), **`Forget[R]`**
  (the read side: `get`, and with a `Monoid` `preview`, `foldMap`,
  `toVector`), **`Star[F]`** (`traverseOf` for any `Applicative` —
  where `Validated`, `Par`, `Static` and an effect row all drop in
  with no code in the optics), **`Aggregating`** (`Reflecting` AND
  `Classifying` in one instance, because the intersection is
  satisfied by one value), and the concrete pairs **`Shop`** (a
  lens's) and **`Market`** (an affine's).
- **Fusion, and the two roads.** `set`/`modify`/`get`/`preview`/
  `foldMap`/`toVector`/`traverseOf` are inline and planned by `Fuse`:
  where the optic's shape is readable at compile time — written
  literally, named by an `inline def`, `Lens[S](_.f)`, chains of them
  — the emitted code is what a person would write, allocation
  identical to the byte. Where it is not — an optic behind a `val`,
  chosen at run time, a traversal — it falls back to the
  interpretation, which is an object per level. Both are correct;
  only the price differs. Measured pairs are in docs/optics.md and
  specs/optics.md.
- **The two field constructors.** `Lens[S](_.f)` is a macro that
  READS the selector and writes `Lens(get, set)` (the policy is "a
  macro only reads; it never writes"), so the field is ordinary code
  the IDE completes and renames. `Lens.field[S]("name")` is by name,
  the label checked against the `Mirror` at compile time with no
  macro at all — and it is the one constructor `Fuse` cannot read, so
  it pays the interpretation.
- **`Iso.non(d)`** — Kmett's: an absent value reads as `d`, and
  writing `d` back makes it absent again. This is what turns
  "create the missing parent" from an unlawful lens into a lawful
  composition; it is an iso modulo one normalisation, and the test
  names it (`Some(d)` and `None` are one point, so writing the
  default PRUNES the spine).
- **`PState.Zooming[X, R]`** (`[A, B] =>> Cont[X, B => R, A => R]`) —
  a typestate transition read as a profunctor in its STATE, with
  `opticZooming` its `Strong`. `PState.zoom` is `l[Zooming[X, R]](m)`
  and has no body of its own, so every `Strong` optic zooms a
  parameterised-state program: an iso, a `first` over a pair state, a
  composed chain. **There is no `Choice` here and cannot be** — on
  the absent case the zoomed program must still answer the inner
  program's `X`, which is universally quantified, and the only source
  of an `X` is the program that case says not to run. Parametricity,
  not a type error. The door that exists prices itself in its type:
  `PState.zoomCase(prism)(m)` answers `Option[X]`.
- Gotchas, each one paid for:
  - `.compiled` (the optic run at its own `Market` pair) is
    MEASURED SLOWER than the optic — 8.0 ns against 3.0 for a
    one-field set, because the `Either` a market carries costs more
    than it saves and a `val`-held optic is a monomorphic call site
    the JIT inlines through. It is kept for the PAIR (to hand across
    a boundary, to store an optic as data), never for speed.
  - a traversal cannot be `.compiled` at all, and the missing given
    is the honest reason: a pair holds ONE focus, so there is no
    `wander` for it.
  - `Aggregating` is deliberately NOT `Strong`: `first` would have to
    answer a `C` from a `Vector[C]` and there is no honest choice, so
    an ordinary lens does not compose into that road — the
    classifying lens stands in its place.
  - `Optic.idApplicative` and `Optic.zipLazy` are NOT givens.
    `Applicative[Id]` would be ambiguous with the package's
    `Comonad[Id]`, and a lawful zip applicative on a finite sequence
    does not exist (`pure` for zipping must be the infinite repeat),
    which is why the zip one is a `LazyList`.
  - a bottom-up rewrite is NOT a traversal — applying `f` to a
    REBUILT node binds the effect, which is a monad, so that is a
    catamorphism. `Ui.everywhere` is top-down and its test names an
    `f` for which the two differ.

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
- **`FoldUntil[-A, S, R]`** — the fold that can STOP
  (specs/fold-until.md): `init`/`add` as `Fold`, plus `done(s)` (asked
  before the first element and after every one; a consumer pulls
  nothing once it answers) and `end(s)` (the result from wherever the
  walk stopped). Instances `find`, `headOption`, `exists`, `forall`,
  `take(n)`; `FoldUntil.until(z)(step)(finish)` adapts a step
  answering `Either[S, R]` — an adapter, not the primitive, so that
  no consumer allocates a `Left` per element. Deliberately NOT a
  `Fold`: a `Fold` consumer walks to the end and would lose the stop
  silently. Consumers: `Stream.foldUntil`, `Chunks.foldUntil`,
  `Writer.foldUntil` (answers `R` alone — an early stop never sees the
  program's answer), `Source.runFoldUntil`, `Producer.foldUntil` (the
  `Produce + G` road, same early `pure`), `.foldUntil(using fo)` on
  a writer program — pure, or effectful with the `Handler[G]` in scope —
  `xs.foldUntilTo` on any `Foldable` (`Foldable.foldUntil` is on the
  trait; an `Iterator` is left after the satisfying element), and
  `Take.foldUntil(using fo): R ! Take % W`, the fold as an iteratee
  (over `!.loop`). The explicit `(using fo)` forms sit in extension
  blocks of their own: a call's explicit `using` binds to the
  EXTENSION's clause when the extension has one. Two laws consumers
  rely on, pinned by `TestFoldUntilStreams`: `take(0)` pulls nothing;
  the continuation after the satisfying tell is never called.
  **`FoldUntil.OfLong` / `OfInt` / `OfDouble` / `OfBoolean`** — the
  same four unboxed shapes `Fold` has, for the same measured reason
  (fold-until-unboxed: the accumulator box is the whole cost, 25x on
  the bare loop); `FoldUntil.long(z)(f)(stop)(finish)` and siblings
  build one, `exists`/`forall` are `OfBoolean`, and `Chunks.foldUntil`,
  `Stream.foldUntil` and `Foldable.foldUntil` dispatch on the shape.
  `Writer.foldUntil`/`Producer.foldUntil` do not, and that is measured
  (writer-fold-until-unboxed): on the tree walk the box is 24 B/elem
  and no time — the specialised walk reads 73 ± 11 against 80 ± 3 µs
  per 10k, inside the bars, because a `Bind`, a `Say`, a continuation
  and a `split` per element dwarf it.
- **`Gen[W]`** (Gen.scala; specs/generators.md, gen-chain-fusion.md) —
  a Python-style generator. A VALUE CLASS whose one field is a
  `Gen.Chain[W]`: a source program `Unit ! (Writer % W + Stop)` — the
  program that tells, `Stop` the early end — and the stages to read it
  through. `map`/`filter`/`withFilter`/`take`/`takeWhile`/`drop`/
  `flatMap`/`zipWithIndex` append a stage and `++` a `Cat` node,
  nothing walked; a for-comprehension over a `Gen` is a generator with
  no macro. The readers (`toList`, `first`, `find`, `exists`,
  `forall`, `foreach`, `foldUntil`) are `FoldUntil`: one walk of the
  source, the stages applied per element inside `add` (a transducer
  with the state it adds as `St[S]`), stopping the body where they
  have read enough — a fused `take` is done at its n-th kept element;
  an inner generator's `Stop` ends the whole generation. `iterator`
  holds the continuation and applies it on the NEXT `next()` (the
  Python law) over `program`, the chain materialised as walks; a
  `generator[W] { … }` block (okay-direct) reads `program` too.
  `Gen.emit`/`stop`/`from`/`unfold`/`of` build one. Non-memoising
  (`toLazyList` memoises). A value class because an extension on the
  row alias cannot infer `W` and an extension on an opaque lost to
  the package's `map` in lexical scope; a `Chain` because the source's
  element type is an existential a value class cannot name.
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
  lambda is typed); `Stage.transduceUntil(z)(step: (S, I) => Stage[I,
  O, Either[S, R]], end: S => R)` — the same with a step that may END
  the stage on `Right`, so `through` stops pulling upstream (a prefix
  parser); `Stage.mapAccumulate` — the 1:1 special case.
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
- **`Gate`** (okay-x402) — an okay-http route behind x402: 402 with
  what it accepts, then match, claim (no replay), verify, run, and settle
  only a 2xx answer. `Paying(http, policy, payer)` is the client half;
  keys live behind `Payer`, never in the module.
- **`CardanoFlinkSource`** (okay-scalus-flink) — the chain as a FLIP-27
  source with ONE split (a chain is one sequence), whose checkpoint is
  the last block emitted; rows from `CardanoTables` through
  `FlinkSchema`, the same tables Spark reads.
- **`format("cardano")`** (okay-scalus-spark) — a Spark DataSource V2
  that decides nothing: the driver follows a relay to confirmed blocks,
  partitions carry their bytes, executors decode them into
  `CardanoTables` rows through `Columns`. An offset is a confirmed
  block, so a re-run micro-batch is the same rows.
- **`CardanoTables`** (okay-scalus) — a Cardano block as TYPED ROWS
  (transactions, inputs, outputs, assets, mints, certificates,
  withdrawals, redeemers), the explode written once so an engine-free
  consumer and Spark read the same tables. Sums stay sums in the rows;
  `Columns` gives them their tabular shape. An input is a reference, and
  resolving it is a join — the rows do not guess.
- **`CardanoFollower`** (okay-scalus) — a Cardano relay followed to
  confirmed blocks with no node and no API key: Ouroboros node-to-node
  written here (mux, handshake, chain-sync, block-fetch in batches,
  keep-alive), scalus's ledger model for the bodies, okay-chain's
  `Tracker` for finality. A block's id is its HEADER hash — scalus's
  `Block.hash` is the body's.
- **`Tracker`** (okay-chain) — a blockchain follower as a PURE state
  machine: what a source observed (`Forward`, `Backward`, `AtTip`) in,
  what a consumer may act on (`Confirmed`, `RolledBack`) out, no I/O
  and no clock. Depth is counted over blocks linked to the chain
  followed — never off the source's head, which after a reorg sits on
  a fork the follower has not seen yet (the defect its first cut had).
  `Poller` turns `head`/`block(n)` APIs into the same observations.
- **`Schema.SBigInt`** (okay-codec) — an unbounded integer as a
  PRIMITIVE of the algebra, so a schema, a SQL column and a Spark
  column all see a number rather than the text it travels as. CBOR
  writes RFC 8949's preferred serialization (a plain integer through
  2⁶⁴−1, a tag 2/3 bignum past it — the Cardano ledger's `big_int`,
  byte for byte); JSON writes a digit string, because a JSON number is
  exact only to 2⁵³ and ours is a `Double`. Found on the way: CBOR's
  `SLong` decoded a uint64 past 2⁶³ as a negative number; it now
  refuses.
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
  intermediate and genuinely existential. `case Inject(c)` needs
  nothing: GADT refinement gives the type back.
- **`ChunkBuf.update` / `.chunk`** — the array assertion, once, with
  four measured alternatives recorded against it.
- **`split`** and **`<|>`** — the union split, sound by the excluded
  middle of `F[A] | G[A]`, documented as the trusted kernel. `split`
  takes the two branches as `inline` continuations and answers
  nothing but their result — no Either, no Option per operation —
  and holds the union's two casts; `<|>` is `split` at
  `Left`/`Right` (either-via-split), the `Either` form for drains and
  tests. `split` is what every walker in this library uses (`State.handle`,
  `Writer.foldWith`, `relay`, `Effects.handle`, `Handler.union`,
  `Resource.run` in the core, and the stream walkers in okay-stream;
  split-without-either,
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
  - `Json` TAKES it, but only behind an import and only for literals
    (json-literals, 2026-09-11 — the first reading of this entry
    refused it outright, and the operator's shape is what resolved
    it). The hazard was never `into`: it was an AMBIENT
    `Conversion[String, Json]`, which would turn an already-serialized
    document into a JSON string literal with no error anywhere, since
    `Json.parse` answers a `Json` and a conversion cannot read intent
    when both meanings are `String`. So the conversions live in
    `Json.literals`, a file sees them only by importing them, and the
    string one accepts CONSTANT types only:
    `inline given [L <: String & Singleton]` whose body matches
    `constValueOpt[L]` and calls `compiletime.error` for anything
    else. A literal converts, a `String` value is a compile error
    naming `JStr` and `Json.parse`. `Int`/`Double`/`Boolean` convert
    plainly; `Long` is left out because `JNum` is a `Double` and the
    loss past 2^53 would be silent. The singleton bound ALONE does not
    work — every `val` has a singleton type — and that refuted step is
    kept as a test.
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
- **`Schemas.field`** (okay-scala2-codec) — `SProduct` hands `make`
  the decoded fields as a `Seq[Any]` in field order, each decoded by
  the schema at that position, so position `i` holds an `Fi`. Scala 3's
  derivation performs the same cast.
- **`Choose.residual`** (okay-scala2) — not a cast, but a claim of the
  same kind: a `TypeableK[Top]` that answers "not a `Choose`". That is
  right for a two-part row `Choose + rest`, which is the only row the
  facade builds, and `Logic`'s fair combinators never call it (their
  split is `msplit`'s, on the `Choose` side).
- **`Rows.coerce` and `Effect.narrow`** (okay-scala2, Scala 2.13
  facade). On the Scala 2 side a row is a PHANTOM intersection of
  capabilities (`Eff[State[Int] with Writer[String], A]`) that no
  Scala 3 type follows, so every program is stored at one row,
  `Top[+X] = Any`, and `coerce` re-types it at the concrete union
  each handler handles. This is sound because handlers split by the
  operation's class and never by the row type, and because `Eff.run`
  accepts only `Eff[Any, A]`. `narrow` types a Scala 2 user's
  operation as its effect's `F` right after the `ClassTag` test that
  proves it. The argument is theory ch. 13.

What is not on this list is deliberate: GADT refinement removes casts
outright wherever the ADT records the type (`Schema`, `Context`,
`Model`), and 35 were removed that way rather than named.

## Scala 2.13: `okay.scala2` (okay-scala2)

The same programs, behind types that scalac 2.13 reads through
`-Ytasty-reader`. The walkthrough is [scala2.md](scala2.md), the theory
is [theory ch. 13](theory/13-rows-without-unions.md), and the API page
is [modules/okay-scala2.md](modules/okay-scala2.md).

- **`Eff[-R, A]`** — a program over an open row. `R` is an
  intersection of capabilities, and `Eff` is CONTRAVARIANT in it, so a
  program needing less fits wherever more is allowed. `flatMap[R1 <: R,
  B]` finds the shared row. `Eff.run` takes `Eff[Any, A]`, meaning
  nothing is left to handle. `Eff.runAsync` takes `Eff[Async, A]`.
- **`State[S]`, `Reader[E]`, `Writer[W]`, `Throws[E]`, `Async`** — each
  is a phantom capability trait, and its companion holds the operations
  and the handler. `X.run(...)` removes `X` from the row and leaves the
  rest.
- **`Op[+A]`, `Effect[F]`, `Handler[F, R, B]`** — a Scala 2 user's own
  effect. The operations extend `Op`, and `object KV extends
  Effect[KV]` stands in for `derives Effect`; the capability is
  `Effect[KV]`. A handler receives each operation and its continuation.
  Use `KV.handle` when other effects remain and `KV.run` for the last
  one.
- **`Cont[A, S, R]`** — okay's `Cont` as a class: `shift`, `reset`,
  `pure`, `map`, `flatMap`, `run(k)`.
- **`Source[A]`** — the core's `Source` (`Unit ! (Writer % A +
  Async)`) as a class. `fromEff`/`toEff` convert to and from
  `Eff[Writer[A] with Async, Unit]`.
- **`Fiber[A]`, `Channel[A]`** — `Async.fork` returns a `Fiber`; a
  channel's `send`/`receive` are `Eff[Async, _]`.
- **`Schemas`, `Json`, `JsonSchema`** (okay-scala2-codec) — `derives
  Schema` for Scala 2 (`productN`, `sum`/`variant`, `constant`) and
  JSON as text. `okay.codec.Json` itself is unreadable from 2.13
  (its TASTy crashes the reader), while `okay.codec.Schema`, `Cbor`,
  `Yaml`, `Validate` and the given instances are usable directly.
- **`Response`, `Routes`, `Server`, `Client`** (okay-scala2-http) —
  okay-http from Scala 2. `okay.http.Response` cannot be read (its
  streamed body names the row), and `Route` cannot be read either
  (Scala 3 generic tuples). Routing is pattern matching over
  `GET(Path(...))`. An internal class of the facade must not take a
  common name: `Body` in okay.scala2 once made scalac 2.13 read it
  while resolving `okay.http.Body`, and refuse it (now `ProgBody`).
- **`Db`** (okay-scala2-sql) — okay-sql's operations as `Eff` and
  `Source`. The data types (`SqlValue`, `Bad`, `Drift`, `Isolation`,
  `JdbcSql`) are okay-sql's own and readable from 2.13. `all` turns
  the first undecodable row into a typed `Throws[Bad]`.
- **`Chat`, `Model`, `Tools`, `Policy`** (okay-scala2-agent) —
  okay-agent's loop, with the model, tool and context handlers
  assembled once inside `Chat`. A tool call reaches Scala 2 as
  `Call(id, name, argsJson)`, because `ToolCall.args` is a `Json` and
  Scala 2 cannot read it.
- **`UiApp`, `UiHost`, `ScriptedHost`** (okay-scala2-ui) — okay-ui's
  loop as an `Eff`, and its hosts. Named `UiApp` so that it cannot
  capture `scala.App`. From Scala 2 a Scala 3 enum case constructor is
  typed as the case, not the enum, so `Source[Event](...)` needs its
  type argument.
- **`WebSocket`, `WsSession`, `WsServer`** (okay-scala2-ws) — a server
  session is a fold `(S, Frame) => (S, Seq[Frame])` over
  `Stage.transduce`, and `replay` runs it without a socket. okay's
  `Chunk` alias is invisible from Scala 2, but its expansion
  `ArraySeq` is visible, so a Scala 2 caller passes an `ArraySeq[Byte]`.
- **`Choose`, `Search`** (okay-scala2) — okay's `Choose`/`Logic` as a
  capability. The fair combinators get the rest of the row's
  `TypeableK` as the COMPLEMENT of `Choose` ("not a `Choose`"), and
  okay's `Logic` turns out never to consult it. A combinator that KEEPS
  the capability takes the row as `R <: Choose`, not as `Choose & R`,
  because the second form makes scalac 2.13 infer `Any`.
- **`Dialog`, `Screens`** (okay-scala2-ui) — okay-ui's scenario effect as
  a capability, with `replay` (no host) for tests. `okay.ui.Screen` is
  implemented directly in Scala 2, and a `Nav` stack runs in `UiApp.run`
  unchanged. Only `Nav.screen`, whose type is a union, needed a
  replacement.
- **`Prog[A]`** — `Eff[Async with Throws[Throwable], A]` under a
  one-parameter name, with `run()`/`runEither()`. `Eff.fromProg` and
  `Eff.toProg` convert between the two.

Three facts about scalac 2.13 decided this shape, and each one is a
gotcha for anyone extending the facade:

- A union type in a CONSTRUCTOR parameter makes scalac refuse the whole
  class, while one in a method is read only when the method is called.
  So each class keeps its program in a value class (`ProgBody`,
  `EffBody`, ...).
- A curried handler method that names `R` in several argument lists
  infers `R = Any` at the last position, which `-Xlint` reports. That
  is why `Effect` has `run`.
- The 2.13 probe must call every public method: a method nobody calls
  is never read, so a broken signature would go unnoticed.

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
(`providing[A](a) and providing[B](b)`, core Provide.scala)
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
