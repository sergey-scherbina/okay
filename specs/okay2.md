# okay2 — the core, written a second time in Scala 2.13

## Overview
The operator asked (2026-09-24): not a facade from Scala 3 into
Scala 2 (that is `okay-scala2`, specs/scala2-facade.md), but a
SEPARATE, fully compatible implementation of the same core — the
freer monad, continuations over it, and the effects — in Scala 2.13.
This spec records what was measured before it was built, the row
encoding that came out of that, what the module holds at each stage,
and the traps scalac 2 set on the way. Module `okay2`, package
`okay2`, directory `okay2/` — A SEPARATE sbt BUILD (`okay2/build.sbt`,
operator 2026-09-24), not a project of the root build: it depends on
nothing there, and the root's Scala 3 settings mean nothing to
scalac 2. It is gated from its own directory with the same script:
`cd okay2 && ../scripts/gate.sh test`. The root's `affected`/nightly
do NOT cover it; a lane touching `okay2/` runs that gate itself.

The question it answers is whether okay's design is a property of
Scala 3 or of the design. Stage 0 said: of the design. The RUNTIME
of okay never depended on the union — dispatch is by class
(`TypeableK`), widening is one cast (`RowLift`), the row exists only
in types — so what Scala 2 needs is a new spelling of the row, not a
new machine.

## Stage 0 — the mechanism, by hand (DONE 2026-09-24)
A 200-line probe under scala-cli, Scala 2.13.18, `-Xlint -Werror`, no
plugin, no library: the tree (`Return | Inject | Bind | Delay`), the
four rotation cases, a phantom row, membership as an implicit, split
by class, State and Console relay-style handlers, and Cont as a
facade over `Free[Shift, *]` with shift and answer-type modification.
Right answer; the operation of a union row printed its own class
(`State$Get`, no wrapper); 100 000 left-nested binds without a stack
frame per bind; a program landing in a row without its effect is a
compile error.

### The row encoding, and the one refuted first
- REFUTED: a higher-kinded phantom alias. `type +[F[_], G[_]] =
  Or[F, G]#Row` (with `trait Or[F[_], G[_]] { type Row[A] }`) is
  refused by scalac 2.13.18 — "type Row takes type parameters" —
  with and without `-Xsource:3`, and the type-lambda spelling
  `({ type L[A] = Or[F, G]#Row[A] })#L` fails the same way: an alias
  cannot be given the kind `* -> *` by partial application.
  `(F + G)#Row` compiles only written out at every use site.
- TAKEN: a ROW is a type of kind `*` with a higher-kinded member,
  `trait Row { type Op[+A] }`. A signature is a Row whose `Op` is
  concrete; `sealed trait +[F <: Row, G <: Row] extends Row` leaves
  `Op` ABSTRACT, so it erases to Object exactly as the union does,
  and `Inject(a: R#Op[A])` over a union holds the raw operation.
  `Free[R <: Row, +A]`, `type ![A, R <: Row] = Free[R, A]`.
- A single-parameter signature is written
  `sealed trait Console extends Row { type Op[+A] = Console.Op[A] }`
  with the operations in the companion — the Scala 2 spelling of
  `enum Console[+A] derives Effect`, plus one implicit,
  `implicit val effect: Effect[Console] = Effect.of[Console]`, which
  reads the class off the `ClassTag` of `Console#Op[Any]`. A ROW has
  no ClassTag for its abstract `Op`, which is the right refusal.
- A parameterised signature is a Row CLASS: `State[S]`, `Writer[W]`,
  `Throws[E]`, `Reader[R]`, each with `Op[S, +A]` in its companion.
  `State % S` is `State[S]` by an alias. The applied form is the one
  to write in a row, because of the precedence trap below.

## Stage 1 — the module (DONE 2026-09-24)
`okay2/src/main/scala/okay2`: `Free` (tree, `resume`, `fold`,
`defer`/`delay`), `Row` (`Row`, `+`, `Pure`, `Member`, `Sub`,
`Remove`, `TypeableK`/`Effect`, `Split`, `Handler`, `Interpr`,
`Interpret`, `Relay`), `Effects` (`run`, `runFree`, `next`, `peek`,
`tailcall`, `loop`, `widen`, `relay`, `translate`, `interpret`,
`handle`), `Cont` (`Control` with the `Cont.Rep` and `Func`
instances; `ContModule` with `Pure`, `shift`, `defer`, `delay`,
`bind`, `mapped`, `run`, `isAnswer`/`answerOf`; `ContImpl` with the
one-step `Leaf` absorption and the `step` loop), `State` (+ `PState`),
`Writer`, `Throws` (+ `Abort`, `recover`/`orElse`), `Reader`. The
package object carries `!`, `pure`, `effect`, `Cont`, `/>`, `^`,
`shift`, `reset`, `Func`, `!>`, `%`, and the extension classes.

### Behavior (stage 1)
- [x] the rotation law: Cont's runner agrees with Func on the answer
      AND the effect trace over twelve bind-tree shapes (TestFree)
- [x] `resume` leaves a head form, idempotently, on seven shapes
- [x] State: modify/update/swap/index, 1M-element index, a lone
      operation, forwarding beside another effect, PState
      type-changing state
- [x] Cont: answer-type modification, 1M left-nested chain,
      tagless Cont/Func agreement, absorption bounded at ONE step
      (structural probes on `Any`), 1M binds after a shift, mutual
      tail recursion by `defer`, abort and multi-shot
- [x] Effects: runWith, peek, 1M bind chain, `!.tailcall` 1M,
      `!.loop` 1M, `handle` abort + forwarding, multi-shot handler,
      1M relay with forwarding, 1M handled operations under
      `handle`, translate (answer in another row, forwarding),
      `Handler.union` + `tracing`, `next`
- [x] rows: `at` left/right/deeper, handlers in EITHER order via
      `Remove`, a reordered row, Pure into any row, membership
      REFUSED at compile time, the operation of a union row held raw
      (class `State$Get`), `bind`/`andThen`, `split`/`<|>`,
      `Effect.of` refusing a row
- [x] Writer run/collect/foldWith/map (1M tells), Throws
      runEither/runOption/runUnsafe/recover/orElse, an abort not
      running what follows, Reader run/local

### Decisions
- HANDLERS TAKE THE ROW ANYWHERE: `State.handle(s)(p)` for any `p`
  whose row mentions `State[S]` once, at any nesting. `Remove[F, R]`
  is the type-level function "R without F" (`Out`), resolved by
  implicits (`head`, `last`, then `deeper`/`deeperRight`, then
  `self`), and its `split`/`join` are the one cast pair it licenses.
  WHY: in Scala 3 a union commutes and associates, so any program
  mentioning F unifies with the handler's `F + G`; in Scala 2
  `A + B + C` is `(A + B) + C`, a different type from `A + (B + C)`,
  and NO handler written for `F + G` would unify with a row written
  the natural way. Each handler keeps its `…At` twin at the explicit
  shape (`handleAt`, `runAt`, `runEitherAt`, `mapAt`) for callers
  that already hold it. The kernel interpreters (`relay`,
  `translate`, `handle`) stay at `F + G`: F is explicit there anyway.
- `at` takes `Sub[R1, R2]` (every signature of R1 is a `Member` of
  R2), so a program at a WHOLE row lands in a wider or reordered one.
  `Member` has `left`, `right`, `deeper`, `deeperRight`, `self`,
  `pure`; a union does not commute, so `right` is its own rule.
- A LONE OPERATION IS A BIND WITH A PURE CONTINUATION, in every
  handler: scalac 2 types the `Bind(Inject(e), k)` arm cast-free (the
  continuation's argument is the existential the operation carries),
  and cannot refine a bare `Inject(e)`'s answer type by the
  constructor. One extra node on the last operation of a program;
  the Scala 3 core's `zoomWith` makes the same move for a different
  reason.
- CONSTRUCT AT THE SIGNATURE, WIDEN WITH `at`. `effect[F + G, A](op)`
  does not type: the union's `Op` is abstract, so no operation IS one.
  `produce(2).at[F]`, `Throws.raise("x").at[F]`. The named
  constructors every signature has are the API; `effect` is for a
  signature's own row.
- NO `inline`: `Free.flatMap`, `split` and the rest are ordinary
  methods for the JIT. NOT MEASURED YET against the Scala 3 core
  (backlog `okay2-bench`); the two handler loops that the Scala 3
  core writes with inline branches (`split`, `onAnswer`) here answer
  through an `Either` per operation (`Left(next)` back to the
  `@tailrec` loop) — the shape a JIT scalar-replaces, the shape a
  measurement decides.
- Scala 2 has no polymorphic function types: `Interpr[F, S]`
  (`F !> S`), `Interpret[F, G]` (translate's handler) and `Relay[F]`
  are traits with one polymorphic method.
- `Cont` is `val Cont: ContModule = ContImpl` with `type Rep[A, S, R]`
  ABSTRACT in the module: Scala 2's opaque type. The structural
  probes in TestCont match `Free.Inject(_)` on `Any`, as the Scala 3
  tests do.
- Two `Any`-typed helpers replaced Scala 3's inline `onAnswer`:
  `Cont.isAnswer`/`answerOf`. A branch passed as a closure was a JVM
  frame per handled operation — a stack overflow at 1M, measured,
  fixed by the `Left(next)` shape.

### Scala 2 traps, each measured
- INFIX TYPE PRECEDENCE: Scala 2 gives every infix type operator one
  precedence, left-associative. `S ! State % S` is `(S ! State) % S`
  (32 signatures broken in one compile), and `A + B % C` is
  `(A + B) % C`. Hence rows are parenthesised after `!` —
  `Int ! (State[Int] + Produce)` — and parameterised signatures are
  applied (`State[Int]`), never `%`-ed, inside a row.
- A NEWLINE BEFORE `(` ENDS THE CALL: `split(e)\n(f => …)\n(g => …)`
  is three statements. Argument lists go on one line or in `{ }`
  blocks (a newline before `{` continues).
- `@tailrec` REFUSED on a polymorphic member under a GADT match ("it
  changes type of 'this' on a polymorphic recursive call"): the
  rotation is the static `Free.resume(p)`; the member delegates.
- AN UNCONSTRAINED TYPE PARAMETER OF A RECURSIVE CALL IS `Nothing`,
  and the call gets a `checkcast Nothing$`: `Cont.step`'s
  forwarding arm threw `ClassCastException: String cannot be cast to
  Nothing$` until the inner `run[A, S, Any]` was pinned. The Scala 2
  face of the Scala 3 core's own "lambda typed Nothing throws".
- NO TOP-LEVEL ALIASES: `!`, `Cont`, `/>` live in the package object.
- A method type parameter is NOT refined by a constructor pattern
  (`case Ask() =>` against `Op[Int, X]` is a "GADT skolem" error): a
  handler written as an `Interpret`/`Interpr` object matches with a
  type test (`case _: Ask[_] =>`) and asserts the answer, as the
  Scala 3 core's TestEffects does with `21.asInstanceOf[X]`.
- `implicit val effect` in a companion SHADOWS the package `effect`:
  inside a companion write `Free.inject`.
- A `Return((s, a))` with no expected type infers its row as
  `Nothing`: pin it, `Return[F, (S, A)]`.

### Not in stage 1 (backlog: okay2/backlog.d, okay2's own board since 2026-09-24)
- `Distinct`: two `State[_]` of different parameters in one row are
  two TYPES to `Remove`, one CLASS to the split. The Scala 3 core
  refuses this with a macro; here it misroutes at the first wrong
  answer (loud, a ClassCastException), as the Scala 3 core did before
  its macro. A Scala 2 blackbox macro could compare erasures.
- Context functions (`?=>`, 79 uses in six Scala 3 core files) have
  no Scala 2 counterpart: `Delim`'s `Prompted ?=>` scopes, `Provide`,
  `Reader.lift/unlift`, `Resource`, `Throws.CanTry.ctxFn` will take an
  explicit scope parameter or not come.
- The rest of the core: Delim, Choice/Logic, Resource, Once, Gen,
  Stream/Fold, Prob, Sim, Validated/Static, Eager, Refs, HMap, Tag —
  each a stage of its own, in that order of use.
- The benchmark: one State lane beside the Scala 3 core's, so that
  "no inline" gets a number before anything else is written
  (`okay2-bench`).
- Scala.js / Scala Native cross-build: nothing here is JVM-specific.

## Stage 2 — interop: cats, fs2, zio (DONE 2026-09-24)
The operator's order (2026-09-24): the interop modules before the rest
of the core, and kyo too — but kyo publishes for Scala 3 only, so
there is no `okay2-kyo`. Three subprojects INSIDE the okay2 build
(`okay2/build.sbt`; the interop projects name the root by
`LocalProject("okay2")`, because a root that aggregates a project that
depends on the root is a lazy-val cycle scalac overflows on at load):

- `okay2-cats` (`okay2.cats`): `Into[R, M]`, the natural
  transformation from a row's operations into M, composed along `+`
  by the F test as `Handler.union` is; `CatsInterop.foldTo` interprets
  a program into any cats `Monad` through `tailRecM` (stack-safe
  wherever M's `tailRecM` is); the `Io` row, whose operations ARE
  `IO` values (`type Op[+A] = IO[A]`, tested by the class `IO`), with
  `Io.lift` and `Io.run`; `toCats`/`fromCats` over `cats.free.Free`;
  and `okay2.cats.instances._`: every program row a `StackSafeMonad`,
  a row with `Throws[E]` at its head a `MonadError` (more specific,
  so a `Monad` query for such a row finds it — measured: cats'
  `handleError` syntax reaches it).
- `okay2-fs2` (`okay2.fs2`): a Writer program as an `fs2.Stream[F, W]`
  (`toFs2`, the residual row run in F by an `Into`; lazy `++` and
  `flatMap`, so a million tells cost no stack and `take(1)` runs
  nothing past the first element — asserted), and an
  `fs2.Stream[IO, W]` as a Writer program that pulls ONE element per
  `Io` operation (`fromFs2`, by `uncons1` compiled to its first step).
- `okay2-zio` (`okay2.zio`): `IntoZ[R, Rz, E]` with an environment and
  an error type; `foldTo` into any ZIO (the walk inside `flatMap`);
  the `Zio` row (`Op[+A] = Task[A]`); a Writer program as a `ZStream`
  by `unfoldZIO`, one told value per step; `fromZStream` by
  `runCollect` — a pull that survives across a program's operations
  is a scoped resource, which needs the Resource effect (stage 3).

Where the Scala 3 core's interop runs an `Async` program under
`IO.blocking`/`attemptBlocking` and moves `Chunks`, okay2 has neither
yet: the shape here is the freer-monad one — the tree interpreted in
the target monad, nothing blocked, elements one at a time.

### Behavior (stage 2)
- [x] a program row is a cats `Monad`; `tailRecM` through it, 1M
- [x] `Throws[E] + F` is a `MonadError`: raiseError, handleErrorWith,
      cats' `handleError` syntax
- [x] `foldTo` into Option, Either and IO; 1M operations into Option
- [x] the `Io` row beside State (State handled first, the rest one
      IO); beside Produce by a union `Into`; an IO's effect happens
      when the IO runs, not when the tree is built
- [x] `cats.free.Free` both ways, 100k round trip
- [x] a Writer program as a pure fs2 stream and as an IO stream with
      the IO between the elements; `take(1)` runs nothing past it;
      another effect run by its own Into; 1M tells
- [x] an IO stream as a Writer program, one pull per element (counted);
      round trip
- [x] the `Zio` row beside State; `foldTo` with an environment
      (a service in the environment answers an effect); 1M operations;
      a Writer program as a ZStream (side effects between elements,
      `take(1)`); 100k tells; a ZStream as a program, round trip

Two scalac-2 traps more: a dependent implicit (`h: Into[rm.Out, F]`)
cannot sit in the same implicit section as `rm` — Scala 2 has one
implicit section — so `toFs2`/`toZStream` take the residual as a type
parameter through `Remove.Aux`; and a `MonadError` instance for a
row alias needs the instance in lexical scope (`import
okay2.cats.instances._`), since this module cannot reach the core's
`Free` companion.

## Stage 3 — okay2-stream: the pure stream layer (DONE 2026-09-24)
The operator's order the same day: "port okay-stream too", then
okay-async and okay-platform. okay-stream is two layers, and this is
the first: everything that reads no clock and parks no thread.

INTO THE CORE (`okay2/src`), mirroring where the Scala 3 core keeps
them: `Stream[S[_], F <: Row]` (uncons, a specializable `iterator`),
instances for List/LazyList/Vector, `feedStream[A]` (a writer program
as a stream, the specialized walk: no Option, no Either, no program
per step) and `writerStreamIn[A, G]` (the G-effectful twin, a
forwarded operation answered by the Handler); `Stream.fold`/
`foldUntil` dispatched on the accumulator; `StreamOps`, `FeedOps`,
`FeedInOps`; `Fold` and `FoldUntil` with the four primitive shapes
(`OfLong`/`OfInt`/`OfDouble`/`OfBoolean`), `count`/`exists`/`forall`/
`sum*`/`max`/`min`/`first`/`last`, `find`/`take`/`until`/`headOption`;
`Aggregator` (init/add/merge/present, `sum`, `count`, `contramap`);
`Pull` (of/told/toldIn/withFilter/loop); `Writer.uncons`, `unconsIn`,
`fold`/`foldAt`, `foldUntil`/`foldUntilAt`, `of`, `widen`.

THE MODULE (`okay2/okay2-stream`, package `okay2.stream`): `Chunk`
(= ArraySeq), `Feed`, `Chunks` (generate/range/ofChars/fromIterator/
nats/fibs; map/mapTagged/filter/take/drop/takeWhile/dropWhile; fold/
foldUntil/foldLeft/count; zip; rechunk; pipe; `elements`/`toLazyList`),
`ChunkBuf`, `Take` (await, `each` as a Pull, `foldUntil` as an
iteratee), `Pipe` (`pipe`/`pipeIn`, `through`/`throughIn`,
`into`/`intoIn`, the 256-deep pull budget), `Stage` (await/tell/id/
transduce/transduceUntil/mapAccumulate/phased/chunked/unchunk),
`Lines`, `Pipeline` (the operator tree, `optimize`, `chunks`, `fold`,
`depth`), `Pane`/`Windows` (tumbling/sliding/stage).

### Behavior (stage 3)
- [x] Stream: List/LazyList carriers, a writer program as a stream
      (specialized iterator agrees with uncons on an infinite teller),
      a writer program in G (the Handler runs the forwarded ops),
      `Stream.fold`/`foldUntil` on every accumulator shape, the
      stopping fold pulls exactly enough, `Writer.of`, `Pull`
- [x] Chunks: laziness (only pulled chunks built), a short tail chunk,
      `range` is a `long[]`, transformers agree with the LazyList
      reference at boundaries, an infinite chain stays lazy, zip
      realigns boundaries, rechunk, the chunked pipe, foldLeft/fold/
      the primitive shapes, `mapTagged` unboxed vs `map` boxed, ofChars
- [x] Pipe: the consumer drives, None after the end, stages
      demand-driven and associative, transduce with a flush,
      mapAccumulate, chunked/unchunk, a 4096-chunk stage past the pull
      budget, an effectful producer into a consumer (the effect
      happens at RUN), effectful stages composing lazily, a built
      program starts its stage once per run (all five doors), Lines
      with a multi-byte character split across chunks, Take.each
- [x] Pipeline: optimize preserves semantics on random pipelines
      (scalacheck), fusion shrinks the tree, take pushes into a range,
      rechunk collapses into the source, the compiled pipeline agrees
      with the hand-written one and a mapped chunk is an `int[]`
- [x] Windows: tumbling and sliding equal a brute-force recompute,
      closing by watermark, late elements dropped and counted, the
      lateness bound, the stage form and its re-runnability, one built
      program run twice loses no pane, composition under `through`
- [x] FoldUntil on the carriers: Chunks/Writer/FeedInOps agree with
      the pure road on nine instances, unboxed arms stop at the chunk,
      no chunk pulled after the satisfying one, the effectful writer
      performs the op before the stop and not after, transduceUntil
      stops the upstream, the coroutine road pulls as many as the walk,
      100k tells tail-recursive, phased CSV with both honest ends

### Decisions
- FOUR NAMES WHERE SCALA 3 HAS TWO: the Scala 3 core overloads
  `through` and `pipe` four ways by `targetName`; Scala 2 cannot
  overload methods whose parameters all erase to `Free`, so the
  pairings are `pipe`/`pipeIn`, `through`/`throughIn`, `into`/`intoIn`.
- NO CASTS ON THE WRITER SIDE: the Scala 3 core's `Erased.resumeWith`/
  `reinject` exist because its `Writer` was an identity signature with
  no constructor to match; here `Say` is a case class, so the
  continuation's argument is refined by the pattern, and a forwarded
  operation is re-injected at its own row and the PROGRAM widened by
  `at`. The remaining `asInstanceOf` are the lone-`Inject` answers
  (`pure(om.asInstanceOf[B])`, the answer type the tree cannot say —
  the Scala 3 core's `@unchecked` at the same places) and the
  accumulator dispatch in `fold` (the type test cannot tell the
  compiler `S`).
- `ChunkBuf` is a plain class over `Array[AnyRef]`: no `summonFrom`, so
  an unboxed chunk comes only from the places that know their type —
  `range` (a `long[]`), `ofChars`, `mapTagged`/`Pipeline.Mapped` (a
  `ClassTag`). `Chunks.map` boxes; `mapTagged` does not; TestChunks
  asserts both classes.
- `Fold.long(z)(f)` and friends store the step as a `Function2`: the
  accumulator stays a `long` across the loop, the step's result boxes
  on its way out. A fold written as a direct `new OfLong { }` pays
  nothing; the builders are the convenient road. Backlog `okay2-bench`
  is where this gets a number.
- `Stage.chunked` is not inline: the buffer is boxed whatever T is.
- A DEFERRED TEST EFFECT: `Later.Run(f: () => A)` in the core's test
  sources, the shape of the Scala 3 core's `Async.Run`, because
  `Produce.Emit(a)` takes a VALUE and a test asserting WHEN a side
  effect happens read the construction, not the run (three tests
  failed that way, the library was right).
- NOT PORTED, waiting for okay2-async: Channel, Source (+ merge,
  chunked, flushAfter, Flush), Fifo, Ring, Buffer, Queues,
  AdaptiveFifo, SentinelChannel, AbruptChannel, Growing, Segments,
  Interop, ParallelChunks, Bulk/Tables, `Chunks.foldWriter`, the
  `Staged` inline pipeline. The order after this: okay2-async,
  okay2-platform, then that layer.

## Stage 4 — okay2-async and okay2-platform (DONE 2026-09-24)
The operator's order: "port okay-async too", "and okay-platform". Two
subprojects, one lane, because the effect's three capabilities
(`CanBlock`, `Timer`, `Scheduler`) are traits in okay-async whose only
instances are the platform's.

`okay2-async` (package `okay2.async`): the `Async` row with `Run` and
`Await` (an error channel in, a canceller out); `Accepted`; `CanBlock`,
`Timer`, `Handoff`, `Fiber` (onComplete/cancel/joinAsync/join/
joinEither), `Scheduler`; `Async(a)` — THE SUSPEND CONSTRUCTOR — and
`await` (the simple form); `Async.await` (the full form), `run` (by
`Remove`, over `relay`), `runAt`, `runAsync` (a Future, by the
callback `Drive`), `spawn`, `Nursery`/`supervised` (the body takes
the nursery as a PARAMETER where Scala 3 gives a context function),
`par` (either side's failure fails the pair and cancels the sibling,
watched on both sides up front), `attempt`, `sleep`, `timeout` (the
program's own failure settles it at once), `race`; `Retry` (policies
as streams of delays, `Retry.async`); `Par` (`map2`, `traverse`,
`sequence` — the `Applicative` instance waits for the typeclass).

`okay2-platform` (package `okay2.platform`, JVM): `Platform.canBlock`
(park a virtual thread; the interrupt read FIRST in the fast path and
at the top of the loop, the Scala 3 core's park-interrupt-order),
`Platform.timer` (one scheduled thread holds every pending delay, the
callback on a fresh virtual thread when it fires), `Schedulers`
(`loom`, `forkJoin`, `drive` with `DriveTask` — fiber, pool task and
promise in one object —, `own`/`adaptive` with the Chase-Lev `Deque`,
the helper rule and the stuck-check, `threads`, `auto`, `platform`),
`Threads`, `Interruptible`, `Scoped`, `Net`/`NetConn`/`SocketConn`,
`parAll`/`parTraverse`/`retry`/`supervised`.

### Behavior (stage 4)
- [x] run is a relay; spawn on a virtual thread parks; par runs both
      sides at once (a handshake, no clock) and sees EITHER side fail
      without waiting out the healthy one; race answers the faster and
      cancels the loser; an async stream consumed lazily; every member
      of the scheduler family (loom, forkJoin, drive, threads, own,
      adaptive) runs par, spawn/join, a failure as a value and a fork
      from inside a fiber; timeout; joinEither; 1M mutual tail calls
      through the Drive; Async beside Writer; Interruptible's cancel
      interrupts the lifted action under `drive`; supervised: a child's
      failure cancels nine siblings, no failure waits for every child,
      the body's failure cancels the children, joinAsync
- [x] the callback surface under `runAsync`, no CanBlock: a 10 000-op
      chain in constant stack, a callback firing during registration,
      sleep, runAsync returns before the program can finish, race
      without waiting for a never-firing loser, cancel before the next
      operation, onComplete, par by callbacks and a child failure,
      joinAsync, a failure under timeout at once, Retry.async (policy,
      last error, delays honoured, zero delays sleep nothing), a race
      of two failures, an Await's Left, attempt
- [x] parTraverse in order, retry per policy and exhausted, policies as
      streams, supervised restarts, Par.sequence (eight leaves meet by
      a latch, order, empty, a failing leaf fails the spine at once in
      either order), Scoped

### Decisions
- `Async(a)`, NOT `async(a)`: a function named `async` in the package
  object of `okay2.async` is ambiguous with the PACKAGE `async` under
  `import okay2._` + `import okay2.async._` — every call site refused
  ("reference to async is ambiguous"). The signature's companion is
  the constructor, as `Writer(w)` is in the Scala 3 core.
- DEFAULTS THROUGH IMPLICIT SCOPE: `PlatformDefaults` (canBlock,
  timer, scheduler) is ONE trait in okay2-async; the companions of
  `CanBlock`/`Timer`/`Scheduler` derive their implicit from it, and the
  platform's package object provides the one `implicit val jvm`. Three
  plain implicit vals there were tried first: a test's local
  `implicit val S: Scheduler` was "ambiguous" against them — Scala 2
  treats a local implicit of another NAME as a second candidate, not a
  shadow. Through implicit scope the lexical one wins outright, which
  is what a Scala 3 nested `given` does for free.
- The JDK: okay2-platform names `Thread.startVirtualThread`/`ofVirtual`,
  so it COMPILES on 21+, and `okay2/.sdkmanrc` pins the gate's sbt to
  the root's 25 (`scripts/gate.sh` reads the .sdkmanrc of the directory
  it runs in — the box's PATH JDK is 17, and the first compile of this
  lane failed on exactly that). It RUNS on 17+: the Loom road is taken
  only where `Schedulers.hasVirtualThreads`.
- NOT PORTED: `Blocking[A] = CanBlock ?=> A` (a context function);
  `Failing`/`AsyncFailing` (needs `Resource`, stage 5); `SharedOnce`
  (needs `Once`); `Operations` (the Clojure/Frege interop values); the
  JS and Native platforms; `TestSchedulerLaws`'s soak tests
  (conservation under thieves, the lost wakeup, the stuck-check) — the
  family test covers each scheduler's basic laws, the soaks are
  backlog `okay2-scheduler-laws`.

## Stage 5 — okay-stream's asynchronous layer (DONE 2026-09-24)
"Port the async stream next" (operator). Into `okay2-stream`, which
now depends on okay2-async (and on okay2-platform for its tests):

- `Cell[S]`: one immutable value behind one CAS — `modify(f: S => (S,
  () => R))` installs the state and runs the action after the CAS won.
  The Scala 3 core's `TRef.modify` at one operation; the STM over
  several cells is a later stage.
- `Channel[A]`: the interface (sendAsync/receiveAsync/offer/close/
  fail/failed/isClosed, the derived `send`/`receive` programs,
  `sendBlocking` offer-first, `receiveBlocking` through a `Handoff`,
  the batched `receiveMany`), with the close contract stated;
  `StmChannel` (the whole state in a `Cell`, `Fifo` list/array, the
  batched receive in ONE transaction with the chunk filled in the
  action); `Drain` and its `Stream` in Async; `Stream[Channel, Async]`;
  `drained`/`drainedChunks`; `Channel.apply` (= StmChannel), `merge`,
  `mergeChunked` (a flusher fiber per source that never touches the
  pull), `mergeSources`/`mergeSourcesChunked` (the writer instance in
  Async under CanBlock), `buffer`, `bufferChunked`; the feeds offer
  first and park on the one refused element, inside a program step.
- `Source[W] = Unit ! (Writer[W] + Async)`: `of`, `apply`, `unfold`,
  `range`, `concat`; `runCollect`, `runForeach`, `runFoldUntil` (one
  walk each); `merge` (readiness; bounded 64 by default; `chunked`
  and `flushAfter`), `either`, `chunked`, `unchunked`, `widen`,
  `toLazyList`; `Chunks.merge`/`either` (a channel of chunks).
- Into the core: `Writer.expand` (one told value becomes many — the
  road `unchunked` takes); `Member.pure` moved to the top with a
  `NotPure` side condition on `deeper`/`deeperRight`.

### Behavior (stage 5)
- [x] a channel is a linear async stream; send after close refused,
      not thrown; a thousand parked receives hold no thread; a
      bounded send suspends as a program and resumes on a take; close
      wakes a parked receiver and drains a parked sender's element;
      8 producers x 4 consumers through 16 slots, every element once;
      the send/close race exact over 100 rounds (accepted = received,
      in order); close does not discard the buffer and the batched
      read agrees with the single one at the batch's every edge; a
      failing producer's elements arrive before the failure
- [x] merge by readiness with each side's order kept and `either`
      keeping the side; fibers start at the first pull; eight sources
      overlap; a silent source holds up nobody; bounded by default;
      buffer/bufferChunked; a failed producer fails the consumer's
      program by callbacks alone; every merge path ends under a
      deadline (elementwise, chunked at 16/256/1024, timed flush,
      Channel.merge); chunked/unchunked leave no trace; a partial
      final chunk is flushed; the chunked-stream merge; runCollect/
      runForeach/runFoldUntil/concat/unfold; the callback bridge

### Decisions
- `Source[A | B]` is `Source[B >: A]`: no union types in Scala 2, so
  `merge[B >: A](t: Source[B])` tells the common supertype (the
  caller says `merge[Any]` for Int and String) and `either` keeps the
  side as data — the typed alternative. `Source.widen[B >: A]` is the
  cast a `Source[String]` needs to meet a `Source[Any]`.
- `Channel.apply` is `StmChannel`. The Scala 3 default is the
  ring-buffered `SentinelChannel` over a `Growing` buffer, chosen by
  capacity, with `Ring`, `Segments`, `AdaptiveFifo`, `Queues` (~2 600
  lines, every one a measured lane); the reference implementation is
  what the contract is defined by, and the fast mechanisms are backlog
  `okay2-fast-channels`. `Flush`/`Flushing`/`mergeFlushing` go with
  them.
- The `Member.pure` ambiguity (measured on `Source.of`): with `pure`
  at the lowest priority, `Member[Pure, A + B]` had two derivations at
  one level (`deeper` and `deeperRight` through `pure`) and the first
  cut's test passed only because `deeper`'s inner search happened to
  be ambiguous and dropped it. Now `pure` is the top rule and
  `NotPure[F]` (two instances for `Pure`, one for everything else)
  keeps the deeper rules off it.
- A `case class State` INSIDE a generic class trips `-Xlint`'s "outer
  reference cannot be checked" on the type test `Cell.modify`'s tuple
  makes: the state is `StmChannel.State[A]` in the companion.

## Stage 6 — Resource, Once, Delim, Provide (DONE 2026-09-24)
"Port Resource, Once, Delim, Provide" (operator). Into the core:

- `Resource`: `Acquire`, `acquire`, `open` (the scope kept open, an
  idempotent closer), `scoped`, `run` (the row anywhere via `Remove`,
  the residual's `Failing`), `runAt` (the region loop: `guarded` on
  every call the walk did not write, finalizers carried into the
  residual); `Failing[F]` with `pure` in the core and `never[F]`
  EXPLICIT for an effect that cannot fail; `bracket` in the package
  object. okay2-async: `AsyncFailing.async` (Run/Await rebuilt with
  the hook) and `anyRowFailing` (the typed one lifted over the row by
  `Split.over`), both from `import okay2.async._`.
- `Once`: `Force`/`Store`, `Handle`, `once`/`!.once`, `at`, `run`/
  `runAt` (the cells threaded like `State.handleAt`; the knot is a
  loud `IllegalStateException`).
- `Delim`: `Prompt`, `NoPrompt` (names the installed delimiters),
  `At` (a default `<unknown>`, a lexical override), `Push`/`Capture`,
  `prompt`/`push`/`shift`/`shift0`/`control`/`control0`/`abort`/
  `reset`, `OneMachine` (a second machine in a row is a compile
  error; `NoDelim` is the absence witness), `Prompted.Aux[R, F]` and
  `scope`/`delimited` with the evidence doors `shift`/`shift0`/
  `control`/`control0`/`abort`/`exit`/`onReturn`, `Emitting.Aux` with
  `collect`/`collecting`/`collectUntil`/`collectingUntil`/`emit`,
  `Paused`/`Dialogue`/`Asking.Aux` with `resumable`/`pausing`/`pause`/
  `drive`/`answer`/`replay`/`Journal`, `run`, `runNested` (forwards a
  capture it cannot place, by `Member[Delim, F]` — no cast), the
  machine (`Segs`/`Cut`/`Next`, `reify`, `cut`, one `@tailrec` loop);
  `Replayable` INDUCTIVE over the row; `Same` in the core.
- `Provide`: `provide` at arities 1–8 (curried bodies), `Providing`
  with `and`, `providing`, `wire`, `Fact`/`Facts`, `Module` (`and` in
  both forms, `installing`, `apply`, `use`, `declare`, `declaring`,
  `ready`/`value`/`nothing`/`contributing`), `module`/`moduleAs`,
  `New`/`fresh`/`prototype`.

### Behavior (stage 6)
- [x] Resource: reverse release order; an abort handled inside still
      releases; a JVM throw during a step releases; finalizers travel
      with the residual and a throw AFTER a forwarded effect releases;
      `Remove` finds the effect anywhere; `open` idempotent closer and
      a throw during acquisition; `bracket` over Later and Produce;
      a row with no `Failing` is REFUSED; a forwarded `Async.Run` that
      throws, an `Await` answering Left, and a nested row with Async
      on the right all release
- [x] Once: three demands one run; never demanded never run; a bare
      p beside it runs every time; two calls two handles; the knot;
      the same program twice replays; forwarding; the row anywhere
- [x] Delim: the shift/reset laws, abort, multi-shot; multi-prompt
      past an intervening delimiter; two answer types in one row;
      re-installed prompt; other effects pass through; abandoned
      effects do not run; shift vs shift0, shift0 vs control0,
      control; yield in user code; NoPrompt names the installed
      stack; At default and override; 1000 nested captures and 100k
      pushes; Prompted apart from the delimiter, not forgeable, nested
      scopes, which delimiter a capture names; the second machine is
      a compile error naming `collecting`; forwarding's three claims;
      exit out of two loops, collect/emit, collectUntil stops the
      walk at the third leaf, pause/resumable as a value, Done
      already, onReturn twice; a pause crosses a collecting; replay
      through a producer; exit crosses a nested producer; pausing
      inside delimited; the journal round trip, empty and full;
      the discipline is a TYPE (Writer refused), the limit measured
      under `unchecked`, pause performed once; Replayable inductive
- [x] Provide: provide, nearest by NAME, missing dependency does not
      compile, three at once; providing and-composition, reuse, the
      right operand inner, 9 layers; wire; Module acquire/release
      order with the right built in the left's context, plain `and`,
      `use`, a ready test double, facts as a multibinder with the
      preview, `declaring` reads its own installer, prototype/fresh

### Decisions
- The evidence doors take the evidence FIRST, as a value:
  `Delim.shift[Int, Int](in)(k => k(5))`, `Delim.emit(e)(a)`,
  `Delim.pause(s)(q)`, `Delim.exit(in)(v)`. The Scala 3 doors read
  the row and answer type off a `direct` block's context function;
  Scala 2 has neither, and the alternative — an implicit evidence in
  the LAST list — cannot type `f`, whose parameter mentions the
  evidence's row. The evidence carries that row as a type member
  (`Rest`, with `Row = Delim + Rest`), so `Prompted.Aux[R, F]`,
  `Emitting.Aux[A, F]` and `Asking.Aux[Q, A, R, F]` are a body's
  parameter types and no door needs the cast the inline Scala 3 doors
  make (`Stopping.atRow`, `pause`'s `k` cast): a `Stopping`'s row IS
  the row `emit` captures at, by the member.
- The machine runs at the answer type `Any`, as every handler here
  does (State's `k(s)`): scalac 2 instantiates a `Bind`'s unconstrained
  answer to `Any`, and the two claims stay the Scala 3 core's two
  (`Push.body`, `Capture.f`). The typed `Segs` chain KEEPS its types
  with one change: `Done` carries `A =:= Z` as a value, because scalac
  2 refines a pattern's own type parameters from the scrutinee but not
  the method's from a pattern (`Refl() extends Eq[A, A]` proves
  nothing in a Scala 2 match), and `reify`/`loop` apply the witness.
  `Next` holds its head type as an abstract member, since a local
  `@tailrec` loop cannot change type arguments across calls.
- `runNested` forwards through `Member[Delim, F]` and `at` — the
  Scala 3 core casts there; the witness makes it typed.
- `Replayable` is the inductive instance the Scala 3 core could not
  write: a row is nominal here, so `union(f, g)` resolves, and a
  `Writer` in the row is refused with the message.
- `OneMachine` is the Scala 2 absence witness (`NoDelim`: one
  instance always, two more when `Member[Delim, F]` holds — ambiguous
  exactly when Delim is in the row). An abstract F reads as absent,
  as in the Scala 3 core.
- `At` defaults to `<unknown>`: a Scala 2 def macro cannot expand in
  the run that defines it and okay2 is one build; a caller installs a
  lexical `implicit val at: At = At("File.scala:12")`. A macro module
  is backlog `okay2-at-macro`.
- `Failing.never[F]` is a METHOD, not an implicit: the Scala 3 core's
  finding that an identity default silently unguards a row stands;
  the core's only implicit is `pure`, and a row with nothing is a
  compile error (tested).
- `provide` bodies are CURRIED (`implicit db => implicit log => app`):
  an `implicit` lambda parameter is one identifier in Scala 2, and the
  curried chain is what `Providing.and` composes anyway, so the flat
  and composable forms agree on what a body is. Nearest-wins is by
  NAME: Scala 2 shadows implicits by name and reports two of one type
  in nested scopes as ambiguous, so an override takes the same name.
  Arities 1–8 rather than 22: the curried chain has no cap.
- `Module.and` is overloaded: a plain `Module[G]` and the dependent
  `F[Module[G]]` (a function of what the left installs); the identity
  module is the one shape where they coincide, and `contributing` is
  its door. `prototype(acquire, release)` is one list (two by-name
  overloads with different list counts do not resolve).
- Not ported: `plan`/`exports`/`shadowed` (macros over the chain's
  type; backlog `okay2-module-plan`), `Delim.Stacked` (the prompt
  stack as a tuple type; Scala 2 has no `*:`), the inline `shift[A]`/
  `exit`/`emit`/`pause`/`onReturn` (direct-block doors — the evidence
  forms above are their Scala 2 spelling), `SharedOnce` (okay-async;
  backlog with `okay2-stage2`), `ctxMonad` (no `Monad` here).
- Scala 2 traps this stage: a `case class` inside a test suite trips
  -Xlint's outer-reference check on every type test (hoist to an
  object); `implicit val at` after a use of `at` in the same block is
  a forward reference (wrap in a def); a shadowed implicit lambda
  parameter is "never used" (the outer layers are `_ =>`).

## Stage 7 — Delim.Stacked, Choice/Logic, SharedOnce (DONE 2026-09-24)
"Do Delim.Stacked, SharedOnce and the Once handler-order tests, which
wait for Choice/Logic" (operator).

- `Delim.Stacked`: the prompt stack as a TYPE — an HList `Cons[P, S]`/
  `Empty` of prompt singleton types (`p.type`), `Has[S, P]` with
  `here`/`there`, `Stack[S]` a value whose METHODS are the doors
  (`shift`/`control`/`abort` asking `Has[S, p.type]`, `reset` pushing
  one prompt for its body), `In[R, S]` (the prompt and the stack under
  it), `delimited` (the root on `Empty`).
- `Choose`/`Logic` in the core: `Op(as)`, `choose`/`fail`/`guard`,
  `runChoice` (the row anywhere) / `runChoiceAt` (a Cont-valued handler
  resuming once per alternative), `Logic.msplit`/`cut`/`ifte`/`gnot`/
  `interleave`/`fairBind`/`observe`; `choose`/`runChoice` in the
  package object.
- `SharedOnce` in okay2-async: one store for many fibres, a demand
  met in flight WAITS (an `Async.await` resumed by the store),
  `run`/`runIn`; the walk is a bespoke suspending loop at `Any`.
- The two Once handler-order tests: `Once.run` inside the search
  backtracks the cells (two runs), outside it shares one store.

### Behavior (stage 7)
- [x] Stacked: the five positive shapes and abort/multi-shot/control
      through the real machine; the three negative shapes are compile
      errors naming the stack (no reset: no stack to call; a foreign
      prompt of the same answer type; an escaped prompt after its
      reset returned); a `Stack` cannot be constructed outside; the
      residual hole is PINNED (a program built under an inner stack
      and run after its reset is a run-time NoPrompt)
- [x] Choose: cartesian, pruning, guard, pythagorean triples, effects
      forward with the row anywhere; Logic: msplit, cut, ifte, gnot,
      fair interleave over infinite branches, fair bind, observe
- [x] SharedOnce: one handle two fibres one run (the second waits);
      Once.run per fibre runs twice; a demand after the store answers
      from it; runIn forwards the rest of the row
- [x] Once handler order: inside the search two runs and two tells,
      outside one run and one tell

### Decisions
- The stack is a VALUE the body receives and the doors are its
  METHODS. The Scala 3 core carries the stack as a lexical given and
  a body is a DEPENDENT function `(s: In[R, S]) => Under[F, R,
  s.p.type *: S]` over the indexed `Prog` facade; Scala 2 has no
  dependent function types and no `*:`, and a door as a free function
  would need the stack's `S` as an explicit type argument (all-or-
  nothing type args, `Has[S, p.type]` cannot be in the same implicit
  section as the `Stack[S]` it depends on). With `S` fixed by the
  receiver, `Has[S, p.type]` resolves as a dependent implicit on the
  earlier parameter `p`. Programs stay ordinary `A ! (Delim + F)`.
- The hole that leaves, tested rather than hidden: a program BUILT
  under an inner stack, leaked and run after its reset returned is
  still a run-time `NoPrompt`. The `Prog` index closes it in Scala 3
  and needs the dependent body type; a rank-2 body (`trait Body { def
  apply[P](in: In[R, P, S]): … }`) would close it here at the cost of
  an anonymous class per reset — not taken.
- `shift0`/`control0` stay unstacked (their body's stack is `S` below
  `p`, a type-level function), as in the Scala 3 core.
- `Choose` has no `runSeq` (a collection cannot be a kind-`*` Row) and
  no `MonadPlus`/`withFilter` (no `Monad` here): `Choose.guard(p)` is
  the step an `if` guard would desugar to.
- `SharedOnce` is not `translate`: its handler is polymorphic in X and
  a `Force[a]` answers `Option[a]`, which scalac 2 cannot relate to X
  without a cast; the bespoke loop at `Any` needs none. A type
  VARIABLE in the pattern (`case s: Store[a]`), not a wildcard: against
  a covariant `Op[Any]` scrutinee `Store[_]` is instantiated to
  `Store[Any]` and its two fields no longer agree.
- The Once handler-order tests build `lazy val x` by hand as `!.once`
  of a telling program (no `direct`): the same two readings, the same
  counts.
## Stage 8 — the row as an intersection: `+` is `with` (2026-09-24)
Operator: "начнем с исправления в окей2 алиаса +" (backlog
`okay2-intersection-row`, measured the same day). Until now `+` was
`sealed trait +[F, G] extends Row`: it neither commutes nor associates,
so the row layer carried `Member` (six rules), `Sub`, `NotPure`,
`Remove` + `Aux`, the `.at`/`.plus`/`.bind` coercions, and an `…At` twin
of every handler. This stage makes the row a CONTRAVARIANT INTERSECTION,
the encoding the facade `okay-scala2` already uses:

```
sealed abstract class Free[-R <: Row, +A]
type +[F <: Row, G <: Row] = F with G
type Pure = Row                       // the empty requirement: the TOP row
```

`Free[State[Int], A] <: Free[State[Int] + Writer[String], A]` by
variance, so widening is subtyping; `A with B` and `B with A` are mutual
subtypes, so the order a row is written in stops mattering; and a
handler names the rest of the row as a type parameter scalac infers:

```
def handle[S, R <: Row, A](s: S)(a: Free[State[S] with R, A]): Free[R, (S, A)]
```

### Measured before (2026-09-24, scalac 2.13.18 -Xlint -Werror, a 150-line model)
- all SIX handler orders over a three-effect program infer the residual
  with no annotation; a helper polymorphic in the rest
  (`Int ! State[Int] with R`) works; 1M left-nested binds run.
- THE PARAMETER TRAP: a handler's parameter spelled through the alias,
  `a: A ! (State[S] + R)`, solves `R` as the WHOLE row in 4 of 6 orders
  (the facade's stage 19 measured the same on its side). Every
  row-generic PARAMETER in okay2 is spelled `Free[F with R, A]`;
  results and concrete rows keep `!` and `+`.
- AN INTERSECTION'S `#Op` IS A LIE: `(Writer with State)#Op` resolves to
  the LAST parent's `Op` (`=:= State.Op`), and reading an operation at
  that type inserts a `checkcast` — `ClassCastException: Writer$Tell
  cannot be cast to State$Op`, measured. So `Inject` holds its
  operation as `Any`, and a typed `F#Op` exists only at a SINGLE
  signature F, handed out by the class-test kernel (`Split`) — the one
  cast it always made.

### What changes
- `Free[-R <: Row, +A]`; `Inject(op: Any)`; `Step`/`fold` hand the
  operation over as `Any`.
- `Member`, `Sub`, `NotPure`, `Remove` are DELETED. "F is in R" is
  `R <:< F`; a negative (`NoDelim`) is the same ambiguity trick over
  `<:<`.
- `.at[R2 <: R]` and `.plus[G]` stay as NO-OP widenings (the type is
  already a subtype), so call sites keep compiling; `widen` likewise.
- Every handler takes `Free[F with R, A]` and answers `… ! R`; the
  `…At` twins become the same method (kept as aliases where a caller
  names them).
- `Handler[+F]` handles an operation given as `Any` (`handleOp`); a
  single signature's handler is written `new Handler.Of[F] { def
  handle[A](a: F#Op[A]): A }` — typed, and sound because the row
  admits only F's operations to it. `Handler.union` splits by F's test.
- `Split.split[F, G, A, X](e: Any)(onF: F#Op[A] => X)(onG: Any => X)`:
  the G side is `Any`, never `G#Op`.
- `Effect.of[F]` must not be given a ROW: an intersection has a ClassTag
  for its last parent's `Op`. No implicit ever asks for one
  (`TypeableK` is invariant, and a companion's `effect` answers its own
  signature only); the doc says so.

### Behavior (stage 8)
- [x] every okay2 test green on the new row, the interop modules included
      (276, cold)
- [x] handlers in all six orders over a three-effect program, no
      annotation (TestRow)
- [x] widening by subtyping: a one-effect program IS a program in a
      wider row, in either order; `.at` compiles as the identity
- [x] a program with an effect left unhandled is refused by `run`
- [x] the `#Op` trap pinned: `(Writer[String] + State[Int])#Op =:=
      State.Op` (TestRow), so the kernel never reads at it

### Found while building it (each measured)
- AN INDUCTIVE IMPLICIT OVER AN INTERSECTION IS NOT AVAILABLE in scalac
  2: a rule `Rep[A with B]` matches every type (`S =:= S with S`) and
  the search diverges — on two parts, three and four, at normal and at
  LOW priority. Three places relied on one: `Replayable` (now DERIVED by
  a blackbox macro that flattens the row's parents against the
  whitelist State/Reader/Throws/Delim/Pure — so a user's own
  outside-reaching effect is still refused; scala-reflect is the
  core's one dependency), and `Into.union`/`IntoZ.union` (now explicit
  combinators, as `Handler.union` always was).
- A COVARIANT HANDLER RESOLVES ITSELF: with `Handler[+F]`, the
  documented `implicit val h: Handler[F + G] = Handler.union[F, G]`
  took `h` for its own `Handler[G]` ("Implicit resolves to enclosing
  value"). `Handler`, `Into`, `IntoZ` stay invariant.
- A LONE `Say`'s answer and a Writer-only loop's told value lost the
  type the constructor used to give (`Inject` holds `Any`): one cast
  each, in `Writer.loneAnswer` and the name-based extractor
  `Writer.said[W]` (allocation-free), instead of one per loop.
- Handlers whose rest is itself ONE signature keep typed patterns on
  both sides through `Split.splitBoth` (Take's two-signature rows); a
  single-signature tree reads its operation through `Split.only`
  (Cont's `Shift`, cats' `toCats`). Every cast of the row discipline is
  in `Split`, `Writer.said`/`loneAnswer` and `Cont.shiftOp`.
- A handler called with EXPLICIT type arguments changes meaning: the
  row argument used to be the WHOLE row and is now the REST. 42 test
  call sites named it; all now let scalac infer it, which is what the
  stage is for.
## Results
- Stage 0: see above. The probe is kept beside the repository
  (`../okay2-probe-Probe2.scala` on the operator's box), not in it;
  TestRow's "the row erases" test carries its finding.
- Stage 1: 59 test results, 9 suites, under `-Xlint -Werror`, GREEN
  2026-09-24 as `cd okay2 && ../scripts/gate.sh test` (its own build;
  the first cut was a root project `okay2/test`, and the operator moved
  it out the same day).
- Stage 2: 78 test results (59 + 19 interop), 12 suites, GREEN
  2026-09-24, same gate; every interop suite passed on its first full
  run after the two traps above were paid at compile time.
- Stage 3: 132 test results (+8 core Stream, +46 stream), 19 suites,
  GREEN 2026-09-24 (`cd okay2 && ../scripts/gate.sh test`).
- Stage 4: 166 test results (+34 async/platform), 24 suites, GREEN
  2026-09-24, same gate, on JDK 25.
- Stage 5: 186 test results (+20 channels/sources), 25 suites, GREEN
  2026-09-24, same gate.
- Stage 6: 274 test results (+88 Resource/Once/Delim/Provide/Failing),
  37 suites, GREEN 2026-09-24, same gate.
- Stage 7: 301 test results (+27 Stacked/Choice/Logic/SharedOnce/
  handler order), 41 suites, GREEN 2026-09-24, same gate.
- STAGE 8 LANDED (2026-09-24, okay2-intersection-row). The row is a
  contravariant intersection: `Member`, `Sub`, `NotPure`, `Remove`
  deleted; `+` is `with`, `Pure` is `Row`; every okay2 module and its
  tests moved; 276 tests green cold under `-Xlint -Werror`.

