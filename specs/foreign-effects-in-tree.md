# foreign-effects-in-tree — another library's value as an effect in the tree; ZIO's `for`; row variance

Status: probes done, design written, 2026-10-04. Owner lane:
`foreign-effects-in-tree`. Nothing in main code changes in this lane;
the stages below are what the next lanes build.

The operator's questions (2026-10-04, after interop-compose):

1. Instead of turning a cats/ZIO/kyo value into `Async` at once
   (`asOkay`), keep it in the tree as an effect of its own, so a handler
   decides later how to run it.
2. Name those effects as what they wrap (`IO`, or `Cats.IO`, …) where
   nothing clashes.
3. For ZIO, a system natural to ZIO: steps join in one `for` the way
   they join in ZIO's own `for`.
4. Should `Free` be covariant in its row at all? Does that help, or hurt?
   (Merge speed is a separate question.)

## Answers in one paragraph

The effect IS the foreign type: `Int ! IO`, `Int ! ZIO[Db, DbErr, *]`,
`Int ! Kyo[S]`. An `F[+_]` row member is any covariant type
constructor, and an operation is any `F[A]`, so `IO(1).perform` already
builds one (`perform`, Effects.scala). A handler that runs a ZIO row
infers its `R` and `E` from the row BY SUBTYPING, and answers exactly
the type ZIO's own `for` answers: `ZIO[Db & Log, AppErr, Int]` for a
`Db`/`DbErr` step and a `Log`/`LogErr` step (probe D). Covariance of
`Free` is NOT needed and would HURT: what makes a `for` over different
effects work is a bind whose result row is WRITTEN as a union
(`F + G`), and that works under today's invariance; covariance instead
makes a handler's inferred rest widen to a join (`Object & Enum`) as
soon as two members remain (probe B). Wiring the union bind into
`flatMap` itself is the open problem: two roads were refuted below.

## Interface (target)

```scala
// cats-effect (okay-cats): the row member is cats' own type
val a: Int ! IO = IO(1).perform
// ZIO (okay-zio): ZIO's own type and its aliases
val b: Int ! ZIO[Db, DbErr, *] = ZIO.service[Db].as(1).perform
val c: Int ! UIO = ZIO.succeed(2).perform          // also Task, RIO[R, *], URIO[R, *], zio.IO[E, *]
// kyo (okay-kyo): the one invented name, an alias, because `A < S` has none
val d: Int ! <[*, Async & Abort[E]] = KyoEffect.perform(v)   // `perform` cannot unify `A < S` with `F[A]`

// handlers: the runtime is chosen at handling, not at building
p.toIO        // IO + Async as ONE IO fiber (as CatsEffect.toIO)
p.toZIO       // a row of ZIO members as one ZIO; R and E read off the row: ZIO[R1 & R2, lub(E1, E2), A]
p.via[M]      // each M step lowered by ForeignEffect[M] (Async for IO and Task), the rest of the row kept
p.handle(...) // any okay handler, e.g. one answering the IO operations in a test
```

`asOkay` stays; it is `perform` then `via[IO]` (TestIOMembers checks they agree).
AS BUILT (2026-10-04): `via[M]` is ONE extension in okay-async, chosen by the
`ForeignEffect[M]` instance, not a `viaAsync` per module — per-module
extensions of one name do not overload when imported together
(interop-compose). kyo's member is `<[*, S]`, not an alias: `kyo.Kyo` is
kyo's own object, and a name okay invented would clash.
`CatsFx` stays for cats-effect's primitives that carry sub-programs
(`uncancelable(poll => …)`, `racePair`); a plain `IO` is `IO`.

Names clash only where the libraries themselves clash (`cats.effect.IO`
against `zio.IO`): the user resolves it as in plain code, by a renaming
import. okay invents no wrapper name except `Kyo[S]`.

## ZIO's `for`, and ours

ZIO joins by variance and the signature of its `flatMap`:

```scala
ZIO[-R, +E, +A]
def flatMap[R1 <: R, E1 >: E, B](k: A => ZIO[R1, E1, B]): ZIO[R1, E1, B]
```

The environment accumulates by intersection, the error widens to the
least upper bound (a sealed parent: `AppErr` for `DbErr` and `LogErr`).

Ours, MEASURED (probe D): a program whose row holds one member per ZIO
step — `ZIO[Db, DbErr, *] + ZIO[Log, LogErr, *]` — and a handler

```scala
def toZIO[F[+_], R, E, A](p: A ! F)(using F[Any] <:< ZIO[R, E, Any]): ZIO[R, E, A]
```

infers `ZIO[Db & Log, AppErr, Int]`: the union on the left of `<:<`
gives `R <: Db`, `R <: Log` and `E >: DbErr | LogErr`, and the
instantiation is ZIO's own. The same with ZIO's aliases as members
(`UIO + Task` → `ZIO[Any, Throwable, Int]`). It is `Row.Sub`'s shape
(Row.scala), so it does not meet row-membership-crash. Narrowing
handlers keep ZIO's names: `.provide(layer)` / `.provideEnvironment`
turn a member's `R` into `Any`, `.catchAll` / `.mapError` rewrite `E`.

What joins the STEPS is the bind (next section): under today's
`flatMap` a `for` over two different ZIO rows does not typecheck, as
any two different rows do not. Until the bind lands, a `direct` block
(whose macro coerces each program into the block's row) or `.at[R]`
does it.

## The union-writing bind

IT EXISTS ALREADY as a method: `Row.bind` (bind-in-row-union, 2026-09-23 — `p.bind(f) : B ! F + G`,
Row.scala, with the reasons it is not `flatMap`), found after this section was written. What is open is
`flatMap` itself, so that a `for` mixes rows; the probe below re-derived `bind` and measured that.

```scala
def bind[G[+_], B](f: A => B ! G): B ! (F + G)   // on p: A ! F; one claim: members of a union erase (Row.In's)
```

MEASURED on the real core (probe E, the main checkout's compiled
classes): a `State`, `Reader`, `Writer` chain types as
`Int ! (State % Int + Reader % Int + Writer % String)` with no widen;
the order of members does not matter (`Reader + State` is the same
type); the same effect twice stays one member; the real `Reader.run`
and `State.handle` infer their rest exactly; an abstract row
(`p: Int ! R` bound to `State.set`) compiles, no crash.

Wiring it in as `flatMap` (what `for` calls) — REFUTED twice, on the
real core in a worktree:

1. **`flatMap` as an extension** (member removed; one generic extension
   and one `Free` extension writing the union, in `object Freer`). Eight
   sites in main broke before the run was stopped (Eager.scala:61,
   Effects.scala:361 and :453, Lexical.scala:67 and :68, Logic.scala:33,
   Maybe.scala:95, Resource.scala:210), of two kinds:
   - a receiver whose row came from the EXPECTED type (`Inject(e)`,
     `pure(())`) is now typed alone;
   - worse, inside package `okay` an extension `flatMap` of a given in
     LEXICAL scope wins over the companion's: `MonadPlus[A ! Choose]`'s
     (Choice.scala) took `Maybe.prune`'s and `Logic.defer`'s binds, and
     at Eager.scala:61 `t.flatMap` inside `Monad[Eager]`'s `flatMap`
     resolved to that override ITSELF (a type error there only because
     the types differ). Every `Monad` instance written `x.flatMap(f)` is
     open to the same choice, and where the types agree it compiles: an
     infinite recursion the compiler accepts. Not observed compiled —
     the run stopped first — but the resolution rule is the one Eager
     showed, and it rules this road out.
2. **`flatMap` a member, its result row computed by a given**
   (`Join[G, H, O]`: `same` → `G`, two `Unary` → `Unary[F + H]`, else
   `G +~ H`). The core does not compile: dotty 3.9 CRASHES,
   `AssertionError: Failure to join alternatives G and H` in
   `TypeOps.orDominator` — row-membership-crash (AGENTS.md), here met by
   an implicit search over abstract signatures.

Also measured: the bind cannot be written once at `Freer`'s level as
`G +~ H`, because `Freer[Unary[F] +~ Unary[G], …]` and
`Free[F + G, …]` are NOT the same type (neither `=:=` nor `<:<` either
way): the law `Unary[F + G] = Unary[F] +~ Unary[G]` holds applied at
indexes, not between the lambdas (TestFreerPara pins it applied).

Roads still open, in the order to try them:

- (a) a `flatMap` OVERLOAD on the member for the unary case, applicable
  only to a `Free` receiver (an evidence `Freer[G, S, R, A] <:<
  Free[F, A]` reads `F` off `Unary[F]`'s prefix) — no implicit search
  over abstract rows; to check: ambiguity with the general member when
  both rows are the same, and the receivers-typed-alone sites above;
- (b) the union bind for FOREIGN programs only: a value lifted from
  IO/ZIO/kyo is a wrapper type of its own (as `CatsEffect.Program` is)
  whose `flatMap` writes the union — the core's `flatMap` untouched;
- (c) re-test road 2 when a Scala release fixes the crash
  (`ProbeRowCrash` is the reproducer to uncomment).

## Row variance: not needed, and it would hurt

Measured on models of the tree (probes A, B, C; Scala 3.9.0):

| | invariant row (today) | covariant row |
|---|---|---|
| `val p: A ! (F + G) = q` for `q: A ! F` | needs `.at`/`widen` | compiles |
| `for` over two rows, today's `flatMap` signature | refused | refused, even with an expected type |
| `for`, `flatMap[G[+x] >: F[x], B]` (ZIO's trick on a constructor) | — | compiles, row inferred `[X] =>> Object & Enum`: useless |
| `for`, the union-writing bind | compiles (one claim) | compiles (no claim) |
| a handler's rest, one member left | exact | exact |
| a handler's rest, TWO members left | exact | widened to `Object & Enum`; exact only under an expected type, an explicit argument, or `Precise` |

`Precise` (which stops the widening) needs
`import scala.language.experimental.modularity`, and a package-level
experimental import makes every caller experimental too: not usable in a
library's API.

So the useful half of covariance — a mixed `for` — comes from the bind,
not from variance, and the cost of covariance is the inference every
handler call relies on. A third reason, not measured on the real tree:
since one-bridge, `Free[F, A] = Freer[Unary[F], …]` with
`Unary[F] = Diagonal[F]#L`, a match type under a projection on an
invariant trait, so `Free` cannot be covariant in `F` without a new
bridge as well as `Freer[+G, …]`. The real-tree count of the walker sites
covariance would break was NOT taken: the model's inference regression
decides the question, and the count would not change the answer.

The 2026-09-03 decision (specs/writer-covariance.md, free-row-variance)
stands, on a better reason than the one it was taken on: its number
(`Source.merge` 5-7% slower) measured removing the `widen` walk, which
covariance allows but does not require.

## Behavior

- [x] probe A: a covariant `Free` — upcast, rest inference, the lower-bound `flatMap`
- [x] probe B: a covariant `Free` with the union-writing bind — `for` over 2 and 3 effects, the same effect twice, a handler's rest (widened at two members), `Precise`
- [x] probe C: the invariant `Free` with the union-writing bind — the same programs, the rest exact
- [x] probe D: cats `IO`, ZIO (`ZIO[R, E, *]`, `UIO`, `Task`) and kyo as row members; `toZIO` infers `ZIO[Db & Log, AppErr, Int]`, the type ZIO's own `for` gives; a lift of `perform`'s signature (`[F[+_], A](fa: F[A])`) takes a ZIO with no type lambda; kyo is refused by it and needs its own lift
- [x] probe E: the union-writing bind on the REAL core — rows, order, same effect, real handlers, an abstract row
- [x] refuted on the real core: `flatMap` as extensions (lexical givens' `flatMap` win; self-recursion), `flatMap` with a `Join` given (dotty crash)
- [x] stage 1 (foreign-effects-members, 2026-10-04): `IO`, `ZIO[R, E, *]` (and ZIO's aliases) and kyo's `<[*, S]` as row members; `perform` builds an IO/ZIO operation, `KyoEffect.perform` a kyo one; `p.toIO` (IO + Async as one IO), `p.toZIO` (R and E read off the row; TestZioMembers checks the type against ZIO's own `for`), `p.via[M]` (each `M` step lowered by `ForeignEffect[M]`, the rest kept), `KyoEffect.run`; a handler of our own answering the IO steps; `asOkay` agrees with `perform` then `via[IO]`; a thousand steps each (TestIOMembers 6, TestZioMembers 5, TestKyoMembers 1)
- [ ] stage 1, left: cats-effect laws on `toIO` over a mixed program; cancellation both ways through `toIO`/`toZIO`
- [x] stage 2: `provideEnvironment`, `mapError` on a row of ZIO members
- [ ] stage 2, left: `catchAll` (it is the whole program's, not a step's: it needs the rest of the program, a different shape)
- [ ] stage 3: the union bind as `for`'s `flatMap` — road (a), else (b)
- [ ] stage 4: `direct`'s `.?` on an IO/ZIO puts the value in the tree instead of awaiting it (a semantic change: the operator decides)

## Decisions

- **The foreign type is the effect; no wrapper.** A row member is any
  `F[+_]`; `IO`, `SyncIO`, `Eval`, `ZIO[R, E, *]` are covariant in their
  value. Kyo's `<[+A, -S]` puts the value first, so it gets an alias
  (`Kyo[S]`) and a lift of its own.
- **R and E are read at handling, by subtyping.** The steps keep one
  member each; the handler's `F[Any] <:< ZIO[R, E, Any]` does what ZIO's
  `flatMap` bounds do. Merging at every bind would need the bind to
  know ZIO, and `Row.Sub`'s shape is the one that does not crash.
- **`Free` stays invariant in its row.** Measured above.

## Results

Probes are compile-only, run with scala-cli 3.9.0 (models A-D, with
zio 2.1.14, cats-effect 3.7.1, kyo 0.16.2 as in build.sbt) and against
`.jvm/target/scala-3.9.0/classes` of the main checkout (probe E); the
two real-tree roads in a worktree through `scripts/gate.sh
"okayJVM/Test/compile"`, then reverted. The model sources are short
enough to restate here when a stage needs them; the decisive lines:

```scala
// B, covariant: the rest of a three-member row
def runRd[R[+_], A](p: Free[Rd + R, A], env: Int): Free[R, A]
val h1 = runRd(three, 1)                   // Free[[X0] =>> Object & scala.reflect.Enum, Int]
// C, invariant, the same call                // Free[St + Wr, Int]
// D
val zp = toZIO(for { a <- lift(z1); b <- lift(z2) } yield a + b)   // lift: perform's signature
                                           // zio.ZIO[Db & Log, AppErr, Int]
```
