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

### Not in stage 1 (backlog, section okay2)
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
