# okay from Scala 2.13 — a facade, not a cross-build

## Overview
The operator asked (2026-09-22) for okay to "work and compile under
both Scala 3.9 and Scala 2.13". This spec records why that is done
as a FACADE MODULE consumed through Scala 2.13's TASTy reader, and
not as a cross-build of the library's own sources, and what the
facade is.

A 2.13 user adds `okay-scala2`, compiles with `-Ytasty-reader`, and
writes ordinary 2.13 code — for-comprehensions, lambdas, pattern
matches — over types whose SIGNATURES a Scala 2 compiler can read.
The facade is written in Scala 3 and calls the real library; the
2.13 side never sees an effect row, an inline method or an opaque
type. It is `okay-java`'s idea one level up: that module makes the
library usable from Java, this one from Scala 2.

## Why not a cross-build (measured 2026-09-22)
- THE ROW IS A UNION. `F + G` is `[A] =>> F[A] | G[A]` (Effects.scala)
  and every program's type is `A ! Row`. Scala 2 has no union types,
  so the core's central type has no 2.13 spelling; a cross-build
  means a new row encoding (a coproduct with membership type
  classes), which is a rewrite of the core and would undo measured
  decisions (RowLift's zero-cost widening, row-membership-crash).
- `inline` IS THE HOT PATH. 182 `inline def`s in `src/main` alone,
  including `Free.flatMap`, `State.get`, `raise`, `async`. The
  performance arcs of the last month (direct-staged, fold-until,
  handler-fusion) are paid for by them.
- The rest compounds: 19 opaque types, ~23 files of `scala.quoted`
  macros, `given`/`using` in ~200 files, braceless syntax in ~277.
  Across `*/src/main`: 465 files, 93 461 lines.
- Consuming the Scala 3 artifacts DIRECTLY from 2.13 is refused at
  the first call: scalac 2.13.18 `-Ytasty-reader` reads 3.9 TASTy
  fine but stops at `Unsupported Scala 3 inline method flatMap;
  found in class okay.Free` (and the same for `State.get`).

## Stage 0 — the round trip, by hand (DONE 2026-09-22)
A 9-line Scala 3 class wrapping `A ! State % Int` behind non-inline
`map`/`flatMap`/`run`, compiled by 3.9.0 against the core; a 2.13
`for`-comprehension over it, compiled by scalac 2.13.18
`-Ytasty-reader`; run on the JVM. Printed `(42,84)`, the right answer.
- A `private val` of the row `State % Int` inside the facade class
  did NOT stop the TASTy reader. That row has no `+`; stage 1 found
  that a constructor naming `+` does (Results).
- COMPILE classpath: the 2.13 stdlib must stand AHEAD of the 3.9 one.
  3.9's first → `Unsupported Scala 3 union in bounds of type T;
  found in method wrapRefArray in class scala.LowPriorityImplicits`
  (3.9's stdlib carries TASTy, not Scala 2 pickles — the okay-spark
  finding, from the other side). 3.9's absent → `could not find
  package scala.annotation.internal`.
- RUN classpath: 3.9's stdlib is REQUIRED (2.13's alone →
  `NoClassDefFoundError: scala/reflect/Enum`); with it present the
  order does not matter.
- So a 2.13 build that simply depends on okay gets a classpath
  NEITHER compiler nor runtime accepts until it arranges the two
  stdlibs, and that arrangement is part of this module's contract —
  stated in its docs and PROVED by a 2.13 project in this build.

## Stage 1 — `okay.scala2.Prog` and the 2.13 probe project
The one carrier every 2.13 caller needs first: a program over
`Async + Throws % Throwable` — suspended, failing, recoverable,
runnable — the IO-shaped subset of the library.

- `okay-scala2` (JVM, Scala 3.9): `Prog[A]` with `map`, `flatMap`,
  `recover`, `attempt`, `run()`, `runEither()`; `Prog.pure`,
  `Prog.delay`, `Prog.fail`, `Prog.fromEither`, `Prog.sequence`;
  and for Scala 3 callers `Bridge.lift(A ! Async)` and
  `Bridge.program(prog)`, in their own object so 2.13 code never
  loads them. Nothing in its public API is inline, a union, opaque,
  a match type or an extension method.
- `okay-scala2-probe` (JVM, Scala 2.13.18, `-Ytasty-reader`,
  `-Werror`): munit suites written in Scala 2 against `okay-scala2`,
  compiled by the Scala 2 compiler in the ordinary gate. This is the
  test that the facade stays readable from 2.13. A class the reader
  refuses breaks THIS project's compile, not a user's. The reader
  reads a method only when something calls it, so the probe calls
  EVERY public method of the facade.
- The stdlib arrangement the probe needs is the one a user needs,
  and the docs give it verbatim.

## Behavior
- [x] a 2.13 for-comprehension over `Prog` compiles and runs
- [x] `Prog.delay` suspends: nothing runs until `run()`
- [x] a thrown exception inside `delay` and a `Prog.fail` both reach
      `recover`/`attempt`/`runEither` as the same Throwable
- [x] `sequence` over 10 000 progs is stack-safe from 2.13
- [x] Scala 3 code can `lift` an `A ! Async` and read `.program` back
- [x] the probe project compiles with `-Werror` under 2.13.18

## Stage 2 — everything a 2.13 program needs, in `okay.scala2`
Operator (2026-09-23): the Scala 2 surface lives in ONE package,
`okay.scala2`, and its types carry the library's own names ("вместо
Cont2 называй тип просто Cont"). A 2.13 caller writes
`import okay.scala2._` and gets `Cont`, `Eff`, `State`, `Reader`,
`Writer`, `Throws`, `Async` — Scala 2 types, with the same names as
the Scala 3 ones they stand for.

- `Cont[A, S, R]` — the continuation paramonad (answer-type
  modification) as a class: `map`, `flatMap`, `run(k)`;
  `Cont.pure`, `Cont.shift`, `Cont.reset`. Stack-safe, because it is
  okay's own `Cont` underneath.
- `Eff[-R, A]` — a program over an OPEN row. The row is an
  intersection of phantom capability types, `State[Int] with
  Writer[String]`, which Scala 2 can spell; contravariance makes a
  single-effect program widen into a wider row for free, and
  `flatMap[R1 <: R, B]` accumulates rows the way ZIO 1's `R` did.
  Each capability's companion holds its operations and its handler:
  `State.get/put/modify/run`, `Reader.ask/run`,
  `Writer.tell/run`, `Throws.raise/run` (into Either),
  `Async.delay`; `Eff.run` for an `Eff[Any, A]`, `Eff.runAsync` for
  an `Eff[Async, A]` (JVM, blocking).
  Measured before writing (2026-09-22, by hand): scalac 2.13 infers
  the residual row through `State.run(1)(prog)` with no annotation,
  either handler order works, and running a program with an
  unhandled effect is a compile error (its message names the row,
  not the missing handler).
- THE ONE CAST. On the Scala 2 side the row is a phantom, so the
  program is stored at a single top row and each handler re-types it
  at the concrete row it handles. There is no typed route: `Free` is
  invariant in its row, and the phantom has no Scala 3 counterpart to
  carry. The cast lives in one function with this reason beside it
  (operator rule, no cast without necessity).
- `Prog` stays — `Eff[Async with Throws[Throwable], A]` is the same
  program, and `Eff.fromProg` / `Eff.toProg` cross between them.

## Behavior (stage 2), all from Scala 2.13
- [x] `Cont`: shift/reset, a continuation called twice, answer-type
      change (Int → String), 100 000 binds without a stack overflow
- [x] `Eff`: State + Writer in one for-comprehension, handled in
      both orders with the documented answers
- [x] `Eff`: Reader + State + Throws; a raise stops the program and
      `Throws.run` answers Left, state handled outside it still
      answers
- [x] `Eff`: Async + Throws run with `runAsync`
- [x] `Prog` ↔ `Eff` round trip

## Stage 3 — a Scala 2 user's own effect
Operator (2026-09-23): "свои эффекты в скала 2 нужно будет подумать
как сделать". okay declares an effect with `derives Effect`, a Scala 3
derivation, but all a row split needs from it is
`TypeableK.test(x: Any): Boolean` (Handler.scala). The facade can
build that test itself, from a `ClassTag` that scalac 2 supplies.

```scala
sealed trait Console[A] extends Op[A]
final case class PrintLn(s: String) extends Console[Unit]
case object ReadLn extends Console[String]
object Console extends Effect[Console]
```

- `Op[+A]` — ONE Scala 3 signature that every Scala 2 effect's
  operations extend, so the stored program's row stays `Top`.
- `Effect[F[_]](implicit ClassTag[F[Any]])` — `send(op)` performs an
  operation as `Eff[Effect[F], A]`, so two effects are two capabilities
  with no declaration beyond the object. `handle(e)(ret)(h)` removes
  it and leaves the rest of the row; `run(e)(ret)(h)` handles the LAST
  effect and answers (see Results for why both exist).
- `Handler[F, R, B]` — `apply[X](op: F[X], k: X => Eff[R, B])`: the
  operation AND its continuation. Resuming once is an ordinary effect;
  resuming never is abort; resuming twice is nondeterminism.
- A SECOND CAST, `narrow`, from `Op[X]` to `F[X]`. It is right because
  the class test has just passed; it lives in one function next to
  the test that proves it.

Prototype measured from 2.13 by hand (2026-09-23): Console
(resumptive, beside `State`) answered `(3,ada)` and logged `hi ada`;
Choose (multi-shot) answered all four `(Boolean, Boolean)` pairs.
scalac 2.13 types the GADT match (`case PrintLn(s) => k(())`).
OPEN, to be settled by the probe under `-Werror`: at the LAST handler
scalac inferred `R = Any` and `-Xlint` warned "a type was inferred to
be `Any`". Plain `State.run` at the last position did not warn, so the
likely cause is `R` also being inferred through the handler's type.

## Behavior (stage 3), all from Scala 2.13
- [x] a resumptive effect beside `State`, handled in the same program
- [x] a multi-shot handler: every answer of two flips
- [x] an aborting handler: the continuation dropped, the rest never runs
- [x] two user effects in one row, each handled by its own object
- [x] an effect left unhandled does not compile
- [x] the probe stays at `-Xlint -Werror`, whatever that costs the API

## Stage 4 — streams: `okay.scala2.Source`
Operator (2026-09-23): "Стримы, файберы и каналы - нужно будет
сделать их поддержку тоже". The core's `Source[W]` is
`Unit ! (Writer % W + Async)`, which is exactly
`Eff[Writer[W] with Async, Unit]`. So a 2.13 `Source[A]` wraps it, and
a source can also be WRITTEN as an Eff that tells.

- Constructors: `Source(as: A*)`, `fromIterable`, `range(from, until)`,
  `unfold`, `empty`, and `fromEff(e: Eff[Writer[A] with Async, Unit])`
  so that a source is an ordinary for-comprehension of `Writer.tell`
  and `Async.delay`; `toEff` goes back.
- Transformations: `map` (`Writer.map`), `filter` and `mapConcat`
  (`Writer.expand`), `take`/`takeWhile`/`drop` (a `Stage` driven by
  `through`, which STOPS PULLING the source once the stage ends, so
  infinite sources are fine), `++`, `zipWithIndex`.
- Concurrency: `merge(other)` over `Channel.merge` (a fiber per side,
  one channel), read back through `drained`.
- Running: `runCollect`, `runForeach`, `runFold`, each an
  `Eff[Async, _]`, run with `Eff.runAsync`.

## Behavior (stage 4), all from Scala 2.13
- [x] constructors, map/filter/take/takeWhile/drop/zipWithIndex/++
      give the expected vectors
- [x] `take` on an infinite `unfold` terminates
- [x] a source written as an Eff for-comprehension (tell + delay):
      nothing runs until the source is run
- [x] `runForeach` and `runFold` see every element in order
- [x] `merge` of two sources delivers the union of both, whatever the
      interleaving
- [x] 100 000 elements through map/filter/runFold without a stack
      overflow

## Stage 5 — fibers and channels
The same operator message as stage 4. The core's `Fiber` and `Channel`
are traits with plain methods, but everything useful on them answers
`A ! Async`, which a Scala 2 caller cannot compose. So both are
wrapped, and every operation that can wait is an `Eff[Async, _]`.

- `Async.fork(e): Eff[Async, Fiber[A]]` (the core's `Async.spawn`),
  and alongside it `Async.par`, `Async.race`, `Async.sleep` and
  `Async.timeout`, all over the platform's own Scheduler and Timer.
- `Fiber[A]`: `join` (fails if the fiber failed), `joinEither`,
  `cancel`.
- `Channel[A]`: `Channel[A](capacity)`, `send`/`receive` as
  `Eff[Async, _]` (a full channel suspends the sender, an empty one the
  receiver), `offer`, `close`, `isClosed`, and `source`, which drains
  the channel as a `Source[A]` that ends when it is closed.

## Behavior (stage 5), all from Scala 2.13
- [x] a forked fiber runs concurrently and `join` answers its result;
      a failed fiber fails `join` and answers `Left` to `joinEither`
- [x] `par` answers both results; `race` answers the faster one;
      `timeout` answers None past its deadline
- [x] producer/consumer over a bounded channel: every element arrives
      once and in order, and the producer suspends when the channel
      is full rather than failing
- [x] `source` drains a channel into a `Source` that ends on `close`

## Stages 6–10 — the rest of the library (operator, 2026-09-23)
"Остальная библиотека (HTTP, SQL, кодеки, агенты, UI) для 2.13 не
обёрнута - оберни." One stage per area, in dependency order, because
HTTP, SQL and the agent layer all speak `Schema`:

6. codecs (okay-scala2-codec) — below.
7. HTTP (okay-scala2-http): routes, the client, a server.
8. SQL (okay-scala2-sql): queries and transactions as `Eff`.
9. agents (okay-scala2-agent): a model, tools, the agent loop.
10. UI (okay-scala2-ui): the view tree, update, a host.

EACH AREA IS PROBED BEFORE IT IS WRAPPED. A facade is written only for
what scalac 2.13 cannot use directly, because a wrapper where none is
needed is a second API to keep in step. The probe for codecs is why
this rule exists: most of okay-codec turned out to be usable as it is.

Each area is its own module, in the one package `okay.scala2`, so a
2.13 build pulls only the areas it uses (split packages work:
core-modules).

## Stage 6 — codecs
Probed from scalac 2.13.18 against the published jars (2026-09-23):

- USABLE DIRECTLY: `okay.codec.Schema` (the enum, its cases, and
  `wrap`/`refine`/`enumeration`), `Cbor.write/read`, `Yaml`,
  `Validate`. Scala 2's implicit search FINDS okay-codec's Scala 3
  givens: `implicitly[Schema[Option[Vector[Long]]]]` resolves.
- NOT READABLE: `okay.codec.Json`. The 2.13 reader crashes on
  `Json.tasty` ("class file ... is broken (class scala.MatchError/49)"),
  and that takes down every signature that mentions `Json`, including
  `JsonSchema.of`.
- NOT AVAILABLE: `derives Schema` / `Schema.derived`, which is a
  Mirror macro.
- INVISIBLE: a Scala 3 TOP-LEVEL definition (a `type` alias in a
  package) cannot be seen from 2.13 at all. So the facade cannot give
  `Schema` a shorter name, and users write `okay.codec.Schema`.

So the facade is small:
- `Schemas.product1` … `product16`, in the style of circe's
  `forProductN`: field names, the companion's `apply`, and a
  projection back to a tuple. Field schemas are implicit and BY-NAME,
  so recursive types work (`implicit lazy val`).
- `Schemas.sum` / `Schemas.variant`, for a sealed hierarchy: a
  `ClassTag` per case chooses the case when encoding.
- `Json.write` / `read` / `readStrict` and `JsonSchema.of`, all over
  `String`. The `Json` value type stays out of every signature.

## Behavior (stage 6), all from Scala 2.13
- [x] a case class round-trips through Json and Cbor under a
      `productN` schema; a missing optional field reads as None
- [x] a sealed hierarchy round-trips under `sum`/`variant`
- [x] a recursive type (a tree) round-trips
- [x] `JsonSchema.of` renders the declaration as a string
- [x] a decode error is a `Left` saying what was expected and what
      came (okay-codec's own message, "expected SInt, got JStr(old)",
      the same from Scala 3; it does not name the field's path)

## Stage 7 — HTTP
Probed from scalac 2.13.18: okay-http's `Request`, `Method`, `Body`,
`Router`, `Http`, `Server` and `Transports` are readable. `Response` is
not: its body is a `Source`, and so its constructor names the union
row. `Route` is not either: "Unsupported Scala 3 generic tuple type
scala.Tuple". So okay-scala2-http supplies:

- `Response`, with the body held in `ResponseBody` for the reason
  `ProgBody` exists. It has in-memory bodies (text, html, bytes, json)
  and a streamed one (`lines`).
- routing as Scala 2 pattern matching: `Routes { case GET(Path(...)) }`,
  extractors per method, `Path` over the decoded segments, and
  `Requests` for query, text and JSON. Decoding is okay-http's own,
  exposed as `okay.http.Urls` so that it is not duplicated.
- `Server.use` (okay-http's `Server.serve` under `Resource.run`) and
  `Server.start`, which returns a handle, and `Client` over
  `Transports.http`, with `lines` streamed.

## Behavior (stage 7), all from Scala 2.13
- [x] a route matches method and path and answers JSON; a JSON body is
      decoded and a bad one is a 400; a query parameter is decoded; an
      unmatched request is a 404
- [x] (Live) `Server.use` serves while its body runs and a `Client` reads
      the answer; `Server.start`/`close` stops serving; a streamed
      response is read line by line

## Stage 8 — SQL
Probed from scalac 2.13.18: okay-sql's data is readable (`SqlValue`,
`Bad`, `Drift`, `Isolation`, the `Sql` trait, okay-jdbc's `JdbcSql`),
but every operation answers a program (`Long ! Async`, a `Source` of
chunks), which Scala 2 cannot compose. okay-scala2-sql's `Db` is those
operations, each one a call into `Typed`. `all` reports the first
undecodable row as `Throws[Bad]`, and `transaction` is `Typed.transact`
under `Resource.run`.

## Behavior (stage 8), all from Scala 2.13, on in-memory H2
- [x] insert with `SqlValue` parameters and from a case class; read
      back by column label, camelCase to snake_case
- [x] an undecodable row is a `Left(Bad)` in `rows` and a
      `Throws[Bad]` in `all`
- [x] a transaction commits on completion and rolls back on failure
- [x] `verify` answers no drift for a matching table and names a
      missing column

## Stage 9 — agents
Probed from scalac 2.13.18: okay-agent's `Turn`, `Reply`, `ToolCall`,
`ToolSpec`, `Toolbox`, `Handlers` and `Provider` are readable. Only a
`Json` FIELD would break (`ToolCall.args`, `ToolSpec.schema`), because
that is where `okay.codec.Json` gets loaded. The program
`String ! Agent` and the union of handlers it needs are not usable.
okay-scala2-agent's `Chat` assembles the handlers once, the way
okay-agent's own `TestAgent.run` does, and runs `Agent.converse` per
`say`, keeping the context handler (and so the conversation) between
calls. `Model` wraps `Handlers.scripted` and `Provider.anthropic`/
`openAi`. `Tools` wraps `Toolbox`. `Policy` wraps `Compact.all`/
`window`. A tool call is shown to Scala 2 as `Call`, with its arguments
as JSON text.

## Behavior (stage 9), all from Scala 2.13
- [x] a scripted chat answers, and the conversation persists across `say`
- [x] a tool call's arguments arrive as a case class, and the result
      goes back to the model
- [x] `approve` denies a call: the model is told, and the tool never runs
- [x] the declarations carry the arguments' JSON Schema
- [x] a window policy keeps within budget and reports the elision
- [ ] (Live) a real model answers through `Chat`. Written, and SKIPPED
      here because no API key was available.

## Stage 10 — UI
Probed from scalac 2.13.18: okay-ui's `Ui`, `Event`, `Frame`, `Form`,
`Swing` and `Terminal` are readable, so a Scala 2 view is built with
okay-ui's own constructors. `Ui.run` (a program) and `Host` (whose
methods answer programs) are not usable. okay-scala2-ui's `UiApp` is
`Ui.run` as an `Eff`, `UiHost` wraps the terminal and Swing hosts, and
`ScriptedHost` is a host for tests that keeps its frames.

## Behavior (stage 10), all from Scala 2.13
- [x] the loop folds scripted events, draws each changed view once, and
      answers the final state
- [x] a frame renders to text through okay-ui's `Frame`
- [x] an external source merges in, and its `Closed` ends the loop
      (deterministic: exactly 3 increments)

## Stage 11 — forms, and what is queued after them
okay-ui's `Form` is readable, but every function in it takes or
returns a `Json`, which 2.13 cannot read. `FormState[A]` holds that
value in `FormValue` (a value class, for the constructor reason) and
speaks `A`, `Ui` and `Event`.

- [x] a blank form does not decode and names its missing fields
- [x] edits (text, number, toggle) fold into a value that decodes
- [x] a form filled from a value decodes back to it and draws it
- [x] the form is the state of a `UiApp` loop driven by a `ScriptedHost`
- [x] labels appear in the drawn form

Queued after it, in order: WebSockets (`scala2-ws`); nondeterminism as
an `Eff` capability, with okay-agent's search strategies over it
(`scala2-choose-search`); okay-ui's `Dialog`/`Nav` scenarios
(`scala2-dialog-nav`).

## Stage 12 — WebSockets
Probed from scalac 2.13.18: okay-http's `Frame` is readable and
matchable, and so are `Socket`, `Transports.sockets()` and okay-jetty's
`Jetty`. A socket's operations answer programs, and a server session is
a `Stage` (a program). okay-scala2-ws: `WebSocket.connect` gives a
`WsClient` (`Eff` operations, a `Source` of frames); `WsSession.fold`
builds a session over `Stage.transduce`; `WsSession.replay` runs one
without a socket; `WsServer.use` serves routes and upgrades together on
okay-jetty. Its own module, so okay-scala2-http does not pull Jetty.

- [x] a fold session, replayed without a socket, answers each text frame
      and ignores the rest
- [x] binary frames to and from `Array[Byte]`
- [x] (Live, run and green) a client talks to a fold session over a real
      socket, and the same server answers an ordinary route

## Stage 13 — nondeterminism and search
`Choose` joins `Eff` as a capability: `from`/`fail`/`guard`, the handlers
`all`/`first`, `cut` and `ifte`, and the FAIR `interleave`/`fairBind`.
`Search.bestOf`/`all`/`majority` build on it and serve okay-agent's use
("sample until valid") for any `Eff`.

THE RESIDUAL TEST (backlog `residual-row-typeable`, invented here at the
operator's "выдумай"). `Logic`'s fair combinators declare
`TypeableK[F]` for the rest of the row. The facade gives the complement
of the known side: `x => !TypeableK[Choose].test(x)`. Reading
`Logic.scala` showed that `interleave`, `fairBind` and `observe` never
CONSULT that instance. Their one split is `msplit`'s
`split[Choose, F]`, which tests `Choose`. So property (2) of the
backlog item (nested splits cannot misroute) holds by construction
today, and the complement stays right for a two-part row if the core
ever starts using it. Properties (1), (3) and (4) are tests:

- [x] every answer (the Pythagorean triples up to 13)
- [x] `first(n)` on an infinite search; `cut` commits to the first
- [x] `ifte`: every answer of the condition, else only when none
- [x] fairness: an infinite branch does not starve the other (property 4)
- [x] another effect in the rest (Writer) passes through `interleave`,
      in order (property 1)
- [x] State inside the search is per branch, outside it is shared
      (property 3)
- [x] `Search.bestOf` stops at the first good sample (2 calls, not 5)

## Stage 14 — Dialog and Nav
Probed from scalac 2.13.18: `okay.ui.Screen` can be IMPLEMENTED in
Scala 2, and `Nav`'s cases, `Nav.state`, `Nav.update` and `Nav.view` are
readable. A screen stack written in Scala 2 ran through them in the
probe. `Dialog` is an effect (`show: Event ! Dialog`), so a scenario is
a program. The facade makes `Dialog` a capability of `Eff` (`show`,
`ask` over `Form.ask`, `run` on a host, `replay` with no host), and
`Screens.of` replaces `Nav.screen`, whose update answers the union
`Nav | S`.

- [x] a scenario replayed without a host: its screens and its answer,
      or none if the events stop short
- [x] `Dialog.ask`: `$ok` submits a decoded value, and `$cancel` answers None
- [x] a scenario runs on a `ScriptedHost`
- [x] a stack of Scala 2 screens runs in the ordinary `UiApp.run` loop

## Stage 15 — everything possible (operator, 2026-09-23)
"Да бери делай всё что возможно чтобы работало в скале 2." Two surveys
of every module, taken on a fresh `publishLocal` of master:

- FROM THE SOURCES: public `def`s, how many are `inline` (Scala 2 cannot
  call them), and how many RETURN A PROGRAM (`! ...`, `Source`, `Stage`,
  `Resource`, `Cont`). A program-returning method needs a facade,
  because Scala 2 can neither compose nor run the result.
- FROM SCALAC 2.13: one Scala 2 file per module that names every public
  `object`, compiled against the published jars. It shows which objects
  the reader refuses outright.

| module | public defs | inline | return a program | objects | unreadable objects |
|---|---|---|---|---|---|
| okay | 915 | 165 | 216 | 50 | 7 (Cont,Eager,Gen,HMap,Handled,Producer,throws) |
| okay-acme | 35 | 0 | 1 | 3 | 0  |
| okay-actor | 11 | 0 | 6 | 1 | 0  |
| okay-admin | 3 | 0 | 0 | 1 | 0  |
| okay-agent | 181 | 0 | 30 | 17 | 1 (Agent) |
| okay-async | 47 | 4 | 22 | 4 | 1 (Par) |
| okay-blob | 47 | 0 | 33 | 7 | 0  |
| okay-cache | 33 | 0 | 18 | 5 | 0  |
| okay-cats | 15 | 0 | 12 | 1 | 0  |
| okay-cdi | 5 | 0 | 1 | 1 | 0  |
| okay-chain | 28 | 0 | 0 | 8 | 2 (BlockId,TxId) |
| okay-chat | 14 | 0 | 1 | 1 | 0  |
| okay-clojure | 46 | 0 | 9 | 5 | 0  |
| okay-cluster | 372 | 0 | 3 | 18 | 0  |
| okay-codec | 360 | 4 | 0 | 21 | 1 (<file-level>) |
| okay-conf | 13 | 0 | 0 | 4 | 0  |
| okay-crdt | 29 | 0 | 0 | 9 | 1 (NodeId) |
| okay-crypto | 8 | 0 | 0 |  | -  |
| okay-data | 46 | 0 | 0 | 3 | 0  |
| okay-delta | 22 | 0 | 3 | 1 | 0  |
| okay-demo | 95 | 0 | 15 |  | -  |
| okay-deploy | 112 | 1 | 0 | 17 | 0  |
| okay-direct | 186 | 5 | 10 | 2 | 0  |
| okay-docs | 39 | 0 | 21 | 4 | 0  |
| okay-flink | 5 | 0 | 0 | 1 | 0  |
| okay-frame | 34 | 0 | 0 | 4 | 0  |
| okay-frege | 19 | 0 | 4 | 2 | 0  |
| okay-fs2 | 3 | 0 | 0 | 1 | 0  |
| okay-guice | 4 | 0 | 1 | 1 | 0  |
| okay-http | 228 | 1 | 57 | 19 | 2 (Query,Route) |
| okay-intent | 230 | 0 | 0 | 31 | 0  |
| okay-java | 70 | 0 | 7 | 6 | 0  |
| okay-jdbc | 47 | 0 | 17 | 7 | 0  |
| okay-jetty | 26 | 0 | 8 | 1 | 0  |
| okay-js | 40 | 1 | 0 | 4 | 0  |
| okay-kafka | 18 | 0 | 3 | 2 | 0  |
| okay-kyo | 11 | 0 | 11 | 1 | 0  |
| okay-langchain4j | 8 | 0 | 0 | 1 | 0  |
| okay-langchain4j-embed | 3 | 0 | 0 |  | -  |
| okay-lex | 45 | 0 | 2 | 4 | 0  |
| okay-live | 10 | 0 | 0 | 1 | 0  |
| okay-llm | 54 | 0 | 23 | 7 | 0  |
| okay-mail | 30 | 0 | 0 | 4 | 0  |
| okay-mcp | 120 | 0 | 40 | 7 | 0  |
| okay-netty | 23 | 0 | 7 | 1 | 0  |
| okay-obs | 32 | 0 | 7 | 7 | 0  |
| okay-onnx | 7 | 0 | 0 |  | -  |
| okay-openapi | 4 | 0 | 0 | 1 | 0  |
| okay-ops | 33 | 0 | 5 | 6 | 0  |
| okay-optics | 180 | 16 | 4 | 13 | 7 (Affine,AlgebraicLens,Iso,Kaleidoscope,Lens,Prism,Traversal) |
| okay-outbox | 16 | 0 | 11 | 1 | 0  |
| okay-parse | 21 | 1 | 0 | 3 | 0  |
| okay-persist | 266 | 0 | 35 | 32 | 2 (Dialogue,Worker) |
| okay-pg | 44 | 0 | 19 | 5 | 0  |
| okay-platform | 61 | 0 | 16 | 4 | 0  |
| okay-py | 17 | 0 | 0 | 2 | 0  |
| okay-r | 21 | 0 | 0 | 3 | 0  |
| okay-r2dbc | 22 | 0 | 7 | 1 | 0  |
| okay-rag | 111 | 0 | 23 | 13 | 0  |
| okay-reactive | 14 | 0 | 4 | 1 | 0  |
| okay-resilience | 69 | 0 | 31 | 12 | 0  |
| okay-scalus | 58 | 0 | 0 | 10 | 0  |
| okay-scalus-spark | 39 | 0 | 0 | 4 | 0  |
| okay-script | 242 | 0 | 9 | 27 | 1 (Live) |
| okay-security | 87 | 0 | 6 | 16 | 1 (Policy) |
| okay-security-argon2 | 3 | 0 | 0 | 1 | 0  |
| okay-spark | 31 | 0 | 1 | 3 | 0  |
| okay-spring | 14 | 0 | 3 | 3 | 0  |
| okay-sql | 70 | 0 | 18 | 9 | 0  |
| okay-staging | 57 | 0 | 0 | 1 | 0  |
| okay-stm | 33 | 0 | 16 | 2 | 0  |
| okay-stream | 412 | 40 | 96 | 19 | 5 (ChunkBuf,Chunks,SentinelChannel,Source,Stage) |
| okay-subscription | 7 | 0 | 0 | 1 | 0  |
| okay-tls | 21 | 0 | 0 | 1 | 0  |
| okay-ui | 235 | 1 | 52 | 20 | 0  |
| okay-ui-gtk | 61 | 0 | 3 |  | -  |
| okay-workflow | 131 | 1 | 20 | 3 | 0  |
| okay-zio | 14 | 0 | 3 | 2 | 0  |

Reading it. Unreadable objects are rare. They are optics (their types are
built with Scala 3 features), `Source`/`Stage`/`Chunks` (type aliases
over rows), a few core objects (`Cont`, `Gen`, `Producer`, `throws`), and
`okay-codec`'s `Json`. What stands in the way almost everywhere is the
program-returning API. The queue, by what it is worth to a Scala 2
SERVICE:

1. okay-resilience: retry, circuit breaker, bulkhead, limiter.
2. okay-persist: the durable log.
3. okay-stm: transactions over `TRef`.
4. okay-cache, okay-blob, okay-docs: the stores.
5. okay-mcp, okay-rag, okay-llm: the agent ecosystem around stage 9.
6. okay-optics: lenses and prisms without the macros.
7. okay-workflow: durable workflows, which also brings durable agents.
8. okay-actor, okay-kafka, okay-pg, okay-outbox, okay-obs, okay-ops.

NOT APPLICABLE: the interop modules okay-cats, okay-zio, okay-kyo and
okay-fs2. Each depends on the `_3` artifacts of its library, and a
Scala 2 build has that library at `_2.13`. Both on one classpath is a
conflict, not an interop. okay-direct and okay-staging are Scala 3
metaprogramming.

## Stage 16 — the row alias `+`, in Scala 2 code (2026-09-24)
Operator: "Алиас напиши в самом коде на скале 2", then "аналог моего
`+[F[_],G[_]]` так чтобы он в скала 2 для окей работал везде вместо
with". okay writes a row `State % Int + Writer % String`; the facade
wrote `State[Int] with Writer[String]`. The alias is

```
type +[R, S] = R with S
```

kind `*` on both sides, so it stands wherever `with` stood: two
built-ins (`State[Int] + Writer[String]`), a user effect beside a
built-in (`Effect[Console] + State[Int]`), two user effects, and a
chain (`Reader[Int] + State[Int] + Throws[String]`, left-associative,
the same flat intersection to scalac). It is declared in Scala 2
source — the probe's `package object scala2probe` — and the guide
tells a user to declare it once in their own package object. It is
not in the facade: a Scala 3 top-level alias is invisible to scalac
2.13 (`okay.Chunk`, stage 12), and the operator asked for it in
Scala 2 code.

Two other shapes were tried first (scalac 2.13.18, 2026-09-24):
- the core's `+[F[_], G[_]]`, on a model of the facade's signatures:
  a Scala 2 alias cannot ANSWER a higher kind —
  `type +[F[_], G[_]] = ({ type L[A] = F[A] with G[A] })#L` is refused
  with "type L takes type parameters", so the projection `#L` has to
  be written at every use, `Can[((Console + Log)#L + Db)#L]`. And
  even written that way, with a covariant phantom `Can[+F[_]]`,
  WIDENING works and HANDLING does not: `handle` wants
  `Can[Console] with Can[Log] with Can[Db]` and is given
  `Can[[X]Console[X] with Log[X] with Db[X]]`. Scala 3 simplifies
  `C[A] & C[B]` to `C[A & B]` for a covariant `C`; 2.13 does not. So
  the intersection has to stand OUTSIDE `Effect[...]`.
- `+[R, G[_]] = R with Effect[G]`, this stage's first draft: it spares
  the `Effect[...]` around a user effect, but its right side is kind
  `* -> *` and the built-ins (`State[S]`, `Writer[W]`, `Throws[E]`,
  `Async`, `Choose`) are kind `*`, so `State[Int] with Writer[String]`
  — nine of the twelve rows in the probe — stayed `with`. Scala 2 has
  no overloading of type aliases, one name cannot take both kinds,
  and the operator wants `+` everywhere. The alias over kind `*` is
  the one that covers every row, at the price of writing
  `Effect[Console]` rather than `Console` — which is what the row
  said already.

- [x] the alias in `package object scala2probe`, compiled under
      `-Xlint -Werror`
- [x] it IS the intersection: `=:=` both ways, for one `+` and a chain
- [x] a program typed at a `+` row is handled effect by effect, the
      residual row inferred with no annotation
- [x] every Scala 2 row in the probe and the docs is written with `+`
      (a compiler message quoted verbatim, `State[Int] with Any`, is
      scalac's spelling and stays)

## Later stages
- Nothing is queued. The operator's list (effects, continuations, a
  user's own effects, streams, fibers, channels) is covered by stages
  1–5. Direct style is NOT planned: it is built from Scala 3 macros,
  and the operator agreed it is not needed for now (2026-09-23).

## Decisions
- FACADE over cross-build (operator choice, 2026-09-22, after the
  measurements above).
- The facade is written in Scala 3, not 2.13: it must call inline
  methods and name rows, which only a Scala 3 compiler can.

## Results
- STAGE 1 LANDED (2026-09-22). `okay-scala2` (`Prog`, `Bridge`) and
  `okay-scala2-probe`: 6 suites written in Scala 2 and compiled by
  scalac 2.13.18 with `-Ytasty-reader -Xlint -Werror`, plus 1 Scala 3
  bridge suite, all green, from a cold probe target.
- A CONSTRUCTOR MAY NOT NAME THE ROW. scalac 2.13 reads a class's
  primary-constructor parameter types when it first loads the class.
  If one of them names `okay.+`, the class is refused: "Unsupported
  Scala 3 union in bounds of type +; found in object
  okay.Effects$package", reported at the user's `package` line.
  Bisected by hand: a public val, a private val, and a plain parameter
  kept in a def or in a val all failed. METHODS are read lazily, so a
  public method returning the row compiled against the cold probe (tried
  and reverted). Stage 0 never hit this because its row, `State % Int`,
  has no `+`. The fix is `Body` (renamed `ProgBody` in stage 7, see there), a value class holding the program:
  `Prog`'s constructor names a class, and naming a class does not read
  its constructor. It costs no allocation.
- REFUTED along the way, and recorded because each one looked
  plausible: (1) "the facade must not live under package `okay`".
  Moving it to `dev.okay.scala2` changed nothing, and 2.13 code in a
  package `okay.userland` compiles and runs. (2) "`private` hides a
  member from the reader". It does not, for constructor parameters.
- sbt ADDS A FOURTH CONSTRAINT to stage 0's three. For a 2.13 project
  whose dependencies bring `scala-library:3.9.0`, sbt stops: "Expected
  scalaVersion to be 3.9.0 or later" (SIP-51). `allowUnsafeScalaLibUpgrade`
  makes 3.9's jar the COMPILE stdlib, which is exactly stage 0's
  failure. What works is to exclude the transitive jar, resolve it in
  a hidden configuration, and APPEND it to Compile/Runtime/Test
  `dependencyClasspath`. The probe uses exactly the settings
  docs/modules/okay-scala2.md gives users, so the page's snippet is
  gated. The one difference: the probe excludes through
  `projectDependencies`, the page through the library dependency.
- The TASTy reader in 2.13.18 accepts 3.9.0 TASTy. That is not
  guaranteed across releases: the reader supports Scala 3 only up to
  a version tied to each 2.13 release. So when this build's Scala is
  bumped, the probe is the first thing to break, and that is the probe
  doing its job.
- STAGE 2 (2026-09-23). `okay.scala2` now holds `Cont`, `Eff` and the
  capabilities `State`/`Reader`/`Writer`/`Throws`/`Async` beside
  `Prog`. The 2.13 probe has 17 suites, all green on their first run
  under `-Xlint -Werror` against a cold target. Every public method is
  called, and a `compileErrors` check pins that an unhandled effect is
  a type error. Two traps, both avoided by construction: (1) the phantom
  capabilities are TRAITS with companions, so the same name is the
  type in a row and the object holding the operations, as in Scala 3
  okay; (2) the Scala 3 source spells the rows with `&`, because
  `with` as a type operator warns in 3.9, and scalac 2.13 reads `&`
  as its own `with`.
- STAGE 3 (2026-09-23). `Op`, `Effect[F]`, `Handler[F, R, B]` in
  okay.scala2. The 2.13 probe grows to 22 tests, green under
  `-Xlint -Werror`: a resumptive effect beside State, a multi-shot one,
  an aborting one, two user effects in one row, and two `compileErrors`
  checks (an unhandled effect; `run` on a row that still holds another
  effect).
- `handle` AT THE LAST POSITION IS A LINT ERROR, and the prototype's
  guess about why was REFUTED. With `handle` in that position, scalac
  2.13 solves `R = Any` from the first argument list, and `-Xlint`
  reports "a type was inferred to be `Any`". The guess was that the
  singleton capability (`Console.type`) was the cause. Changing the
  capability to `Effect[Console]` produced the same three errors at the
  same three places. `State.run` at the last position does not warn,
  and the one difference in shape is that `handle` names `R` in three
  argument lists, not one. What works is `run(e)(ret)(h): B`, which has
  no `R` at all. `Effect[Console]` stayed as the capability anyway: it
  reads as the declaration does and does not depend on how the object
  is imported.
- STAGE 4 (2026-09-23). `okay.scala2.Source` over the core's
  `okay.Source`; okay-scala2 now also depends on okay-stream. The 2.13
  probe has 28 tests, green on their first run under `-Xlint -Werror`,
  including `take` on an infinite `unfold`, a source written as an Eff
  (it re-runs from the start each time it is run: the second run read
  3 and 4), `merge` of two ranges (compared sorted, since the order is
  the arrival order), and 100 000 elements through map/filter/runFold.
  The stages (`take`, `takeWhile`, `drop`, `zipWithIndex`) carry
  `Async` in their own row, so `through` passes the source's Async
  operations along.
- STAGE 5 (2026-09-23). `Fiber`, `Channel`, and `Async.fork/par/race/
  sleep/timeout` over the core's `Async.spawn`, `Channel` and the
  platform's Scheduler and Timer. The 2.13 probe has 34 tests, all
  green on their first run under `-Xlint -Werror`. They cover a
  producer and a consumer over a channel of capacity 4 moving 1 000
  elements in order, cancelling a fiber that is sleeping, and `race`
  and `timeout` against a 5-second sleep. A fiber and a channel are
  held directly, not through a `ProgBody`: their constructors name the
  core's `Fiber`/`Channel` traits, which contain no union, and scalac
  2.13 reads them fine.
- DOCS AND A REAL CONSUMER (2026-09-23, scala2-docs). A user guide
  (docs/scala2.md), theory ch. 13, typepedia entries (including the two
  casts in the cast registry), and pointers from README, docs/README,
  the tutorial, your-own-effect and ROADMAP. Every snippet is verbatim
  from `TestScala2Guide`, 12 tests. The setup block was then checked
  in a SEPARATE sbt project against a `publishLocal`, and that found a
  defect the probe could not see: `sbt run` takes its classpath from
  `dependencyClasspathAsJars`, so appending the 3.9 stdlib to
  `dependencyClasspath` alone compiled and then failed on `run`,
  forked or not, with `NoClassDefFoundError: scala/reflect/Enum`. The
  probe had passed only because its tests were forked and read
  `fullClasspath`. Two hypotheses were refuted along the way: "an
  unforked run layers `scala.*` away" (a forked run failed the same
  way), and "sbt drops a jar whose module id is `scala-library`"
  (`Attributed.blank` changed nothing). What showed the cause was
  `-XshowSettings:properties` in the forked JVM: its `java.class.path`
  had no 3.9 jar at all. The fix appends to both, and the probe now
  runs UNFORKED (46 tests), the way a user's `sbt test` does.
- STAGE 6 (2026-09-23). okay-scala2-codec: `Schemas.product1..16`
  (generated), `constant`, `sum`/`variant`, and `Json`/`JsonSchema`
  over text. `TestCodecFromScala2` has 6 tests, and the probe has 52,
  green under `-Xlint -Werror`. The test that expected the decode
  error to name the field was WRONG about okay-codec, not about the
  facade: okay-codec's message names the schema and the value
  ("expected SInt, got JStr(old)"), from Scala 3 too. It is pinned as
  it is. Adding the field path is okay-codec's own change to make.
- STAGE 7 (2026-09-23). okay-scala2-http. 4 socket-free tests in the
  gate and 3 Live ones, run and green, including a real server, a
  client and a streamed body. A TRAP found on the way: the facade's
  internal value class was called `Body`, and a Scala 2 file that
  imports both `okay.http.Body` and `okay.scala2._` made scalac READ
  that class while resolving the name. Its constructor names the row,
  so compilation failed at `Body.Text(...)`, with the error pointing
  at `okay.Effects$package`. Internal classes of the facade must not
  take common names, and `Body` is now `ProgBody`.
- STAGE 8 (2026-09-23). okay-scala2-sql. `TestSqlFromScala2` has 4
  tests on H2, and the probe has 60, green under `-Xlint -Werror`. Two
  first-run failures were the TEST's assumptions, not the facade's,
  and both show okay-sql doing its job. H2 upper-cases an unquoted
  column name, so `Bad`/`Drift` say `FULL_NAME`. And `verify` reports
  a NULLABLE column behind a non-`Option` field as drift, so the test
  table's columns are `NOT NULL`.
  In the FULL matrix the suite then failed with "No suitable driver
  found for jdbc:h2:mem:...", although it had passed alone. Unforked
  tests share sbt's JVM, and okay-jdbc's suite had registered H2 in
  `DriverManager` from ITS class loader first. The suite now connects
  through `org.h2.Driver` directly. Reproduced and verified by running
  `okayJdbc/test` then `okayScala2Probe/test` in one sbt: 75 + 60
  green.
- STAGE 9 (2026-09-23). okay-scala2-agent. `TestAgentFromScala2` has 5
  tests in the gate, green under `-Xlint -Werror`. The Live test against
  Anthropic skips without `ANTHROPIC_API_KEY`, and no key was present,
  so a real provider has not been exercised through this facade. The
  providers themselves are okay-agent's, and they are tested there.
- STAGE 10 (2026-09-23). okay-scala2-ui. `TestUiFromScala2` has 3
  tests. A Scala 2 trap: a Scala 3 enum case's constructor is typed as
  the CASE from Scala 2, not widened to the enum, so
  `Source(Event.Pressed(...))` is a `Source[Event.Pressed]`, and the
  invariant `Source[Event]` refuses it. The loop's object is `UiApp`
  because `App` would capture `object Main extends App`. The first cut
  of the external-source test asserted a RANGE, because the host's own
  `Closed` raced the external events. `ScriptedHost.open` (no
  `Closed`) made it exact. All five areas the operator named (codecs,
  HTTP, SQL, agents, UI) are now covered.
- STAGE 11 (2026-09-23). `FormState` in okay-scala2-ui.
  `TestFormFromScala2` has 5 tests, all green on their first run.
- STAGE 12 (2026-09-23). okay-scala2-ws. 3 socket-free tests in the gate,
  and 1 Live test, run and green, against a real Jetty and a client
  socket. Found on the way: `okay.Chunk` (a Scala 3 top-level alias) is
  invisible from Scala 2 ("type Chunk is not a member of package
  okay"), but the type it names, `ArraySeq`, is not. So a Scala 2
  caller builds `Frame.Ping(ArraySeq[Byte](...))` directly, and the
  facade adds `binary`/`bytes` for `Array[Byte]`.
- STAGE 13 (2026-09-23). `Choose`/`Search` in okay-scala2. 7 tests,
  green. The `R = Any` lint trap came back, for the combinators that
  KEEP the capability (`cut`, `ifte`, `interleave`, `fairBind`),
  because `Choose & R` leaves `R` to infer for a program that is only
  `Choose`. The fix here is better than the `run` workaround of stage 3:
  take the whole row as `R <: Choose`. That states the same requirement
  with nothing left to infer. (Whether stage 3's `Effect.handle` can use
  the same bound is worth a look. It removes, rather than keeps, its
  capability, so it is a different shape.)
- STAGE 14 (2026-09-23). `Dialog`/`Screens` in okay-scala2-ui. 4 tests.
  The first draft submitted `Dialog.ask` with `Event.Submitted`, but
  okay-ui's own `ask` loop waits for `Pressed("$ok")`; the test now
  uses that. With this stage the sprint's Scala 2 queue is empty.
  Durable agents and `Scope` inside a dialog remain unwrapped.
- STAGE 15.1 (2026-09-23). okay-scala2-resilience. The pieces, the
  refusals and the retry policies are readable from 2.13 and used
  directly. `Guards` adds only the program transformations. 7 tests. The
  bulkhead test first used a `sleep(50)` to be sure the first call held
  the permit. It now waits on a latch the first call releases from
  inside the permit, so the refusal cannot depend on timing (the
  supervision-shapes flake is the reason not to leave a sleep there).
  Named `Guards` so that it cannot collide with `okay.resilience.Resilient`
  under two wildcard imports.
- STAGE 15.2 (2026-09-23). okay-scala2-persist. The engine (stores,
  topics, `Offsets`, `Snapshots`, `Typed`) is synchronous and readable,
  and Scala 2 uses it directly. Two new reader facts came out of it: a
  TRAIT's abstract-method defaults are invisible (`Store.topic("t")`
  asks for every argument; class and object method defaults do carry
  over), and so is an extension (`topic.of[A]`). `Persist` adds `topic`
  with the defaults, `typed`, and `stream`/`tail` as a
  `Source[Record]`, flattening the engine's chunked streams with
  `Writer.expand`. 4 tests, including a FileStore reopen and a tail
  that sees appends made after it started.
- STAGE 15.3 (2026-09-23). okay-scala2-stm. `TRef` is core and
  readable, used directly (`Stm.ref` is only a name for `TRef(init)`).
  `Tx` is a phantom capability like `Choose`, and `Stm.atomically`
  takes `Eff[Tx, A]`, so the Scala 3 rule "no I/O in a transaction"
  holds in Scala 2 by the same means: the row, not a runtime check.
  Pinned by a `compileErrors` test. 5 tests: a transfer, 1000
  increments from 8 fibers, a retry woken by another fiber, and
  `orElse` discarding the first branch's writes.
- STAGE 15.4 (2026-09-23). okay-scala2-stores: `Caches`, `Blobs`,
  `Documents`. Every store is BUILT from Scala 2 with its own
  constructor, and every OPERATION answers a program, so the facade is
  the operations only. A new reader fact: `new TopicDocs[A](topic)`
  failed with "Unsupported Scala 3 union in bounds of type +" although
  the constructor mentions no row. `new` makes the reader complete the
  whole class, and `query`'s type is a `Source`. A method that ANSWERS
  such a class is fine (`Fs(root)` works), so the fix is a factory,
  `Documents.onTopic`, found by bisecting the test file (the cache
  tests compiled, the documents test alone did not). 6 tests:
  single-flight `getOrLoad` under two fibers, write-through ordering,
  cross-node `drain`, blob put/get/list/stream/head/delete, `putFile`
  with a persist backup and restore, and conditional document writes
  with an indexed query.
- STAGE 15.5 (2026-09-23). okay-scala2-llm, -rag, -mcp. Probed first:
  okay-rag's splitting and keyword search are plain and used directly;
  okay-mcp's `Stdio` links and plain values (`Mcp.Info`, `.Resource`,
  `.Prompt`) too. `ToolCall`/`ToolSpec` carry `Json` in their
  constructors, so MCP's JSON crosses as text, and a server's tools are
  okay-scala2-agent's `Tools`. Two findings: `VectorStore[okay.Pure]`
  in a facade SIGNATURE fails at the Scala 2 call site ("can't find type
  required by method memory ...: okay.Pure") — a top-level alias is
  invisible even inside a type argument — so the store lives in a
  `VectorIndex` with a body holder; and `Handler.union[Embed, Async]`
  does not compile (`Embed` has no `TypeableK`), `union[Async, Embed]`
  does (the tested side is the one with the instance). The Async-store
  variants built on that were REMOVED before landing: the only async
  store is PgVector, a Scala 2 test of it needs a live Postgres, and
  code the default gate cannot run was not shipped. A hang in the
  first probe run was the test's own bug (an unescaped `"` in a
  scripted SSE payload, so nothing decoded and `first` read an endless
  stream); the stream is now bounded at 50 so that failure mode fails
  instead of hanging. 7 tests.
- STAGE 15.6 (2026-09-23). okay-scala2-optics. Nothing of okay-optics
  reaches Scala 2 (type-lambda constraints, alias kinds, polymorphic
  function types, `inline` extensions), so the five kinds are Scala 2
  classes, each a shell over okay's optic with a body holder. Operations
  and composition are okay's own (`import okay.given` brings the
  profunctor instances). The first draft derived a prism's `review` and
  an iso's `reverseGet` by calling `set` on a `null` source; that is
  wrong for any preview that inspects its argument, and it was replaced
  before the first test by carrying `review`/`from` beside the optic
  and composing them explicitly. 5 tests: lens laws on a composed lens,
  a subtype prism with a lens after it, lens-then-prism as an affine
  (unchanged where absent), a traversal through `each`, and an iso then a
  prism reviewing through both.
- STAGE 15.7 (2026-09-23). okay-scala2-workflow, and durable agents.
  The driver's data (`Wf.Step`, `Wait`, `SysA`, `Runtime`) is plain and
  used directly. The program is not: a body is a context function over
  `Wf.Asks`, whose doors exist only while the driver runs it. A Scala 2
  workflow is therefore an ordinary `Eff[Workflow[Q, A], R]` over a
  facade-owned GADT (`WfOp[Q, A, +X] derives Effect`, so its TypeableK
  is by class), and `!.translate` rewrites each operation into its
  `Asks` door inside `Wf.resumable`. Everything after that is okay's
  engine unchanged. It compiled first time. 5 tests: drive, replay as
  a new process, the worker loop, sleep plus signal, and `patch` on an
  old journal. Durable agents: `Chat` takes an optional
  `Durable.Journal` and wraps its gated tool handler in
  `Durable.tools`. 2 tests, one of them the control (no journal: the
  restart pays twice).
- STAGE 15.8 (2026-09-23). okay-scala2-services: `Actors`, `Outboxes`,
  `Logs`, `Tracing`, `Operations`, `Kafkas`, `Postgres`. The builders and
  values of all six libraries are plain; only the program-answering
  operations are wrapped. `Db` gained `private[scala2] def underlying`
  so the outbox runs over the SQL facade's connection (and transaction).
  6 offline tests (actors plain and supervised, outbox relay plus inbox
  once over H2 and a MemoryStore, logs streamed, a span written, ops
  routes plus a RED meter) and 2 Live ones, run once here against
  `postgres:16` and `apache/kafka:3.9.0` in throwaway containers: GREEN.
  Two mistakes of the lane's own, both caught by those tests: the ops
  endpoints are `/healthz` and `/readyz` (the facade's comment first said
  `/health`), and okay-pg's SQL takes `$1`-numbered placeholders, not
  JDBC's `?` (by design: the SQL string is the dialect's).
- STAGE 15.5b (2026-09-23, scala2-pgvector; operator: "Доделай"). The
  PgVector wrapper removed from 15.5 for want of a test is back, now that
  a Live test runs it: `Rag.pgvector(db, table, dim, embed)` answers a
  `PgIndex` whose operations are `Eff[Async, _]`. `Embed` still has no
  TypeableK, so it cannot be `!.translate`d into Async programs; the
  handler is `Handler.union[okay.Async, Embed]` inside `Async.delay`. The
  embedding handler moved into its own `Embedder`, shared by both index
  bodies. `TestRagLiveFromScala2` against `pgvector/pgvector:pg16`: GREEN,
  and the nearest segment matches the memory index's.
- LAYOUT (2026-09-23, scala2-dir). Operator: "я предлагаю перенести все
  scala2 модули в подкаталог scala2". Every `okay-scala2*` module and the
  2.13 probe now live under `scala2/`; artifact and project names are
  unchanged, so a user's build is too. Paths written above this line are
  where things were when they were written. One check depended on the old
  layout without saying so: TestDocsIndex found module roots with
  `file("okay-x")` and treats a path with a slash as a sub-project, so
  after the move the seventeen facade modules would have silently left
  "every module the build declares has a page". It now knows `scala2/`
  as a grouping directory, and a control run (one facade page removed)
  failed naming that module.
- STAGE 16 LANDED (2026-09-24, scala2-row-alias). `type +[R, S] = R with
  S` in the probe's package object; every Scala 2 row in the probe and
  the docs written with it; `TestRowAliasFromScala2` green under
  `-Xlint -Werror`. Measured on the way: a left-associative chain
  `Reader[Config] + State[Int] + Throws[String]` is `(A with B) with C`
  to the parser and a flat `A with B with C` to scalac — `=:=` holds
  both ways and the residual row is still inferred through it with
  no annotation (TestEffFromScala2 "Reader, State and Throws", the
  guide's `withdraw`, unchanged but for the spelling). The first draft
  of this stage, `+[R, G[_]]`, was replaced before it was used: it
  could not spell a row of two built-ins (see the stage).
