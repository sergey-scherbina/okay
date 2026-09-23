# okay-scala2

okay from **Scala 2.13**. It is a facade module, written in Scala 3,
whose public types a Scala 2 compiler can read through its TASTy
reader (`-Ytasty-reader`). You write ordinary 2.13 code (for-comprehensions,
lambdas, pattern matches) and the real library runs underneath.

| | |
|---|---|
| `Eff[-R, A]` | a program over an OPEN row of effects, spelled as an intersection: `Eff[State[Int] with Writer[String], A]`. The operations and handlers live on the capabilities' companions: `State`, `Reader`, `Writer`, `Throws`, `Async` |
| `Cont[A, S, R]` | delimited continuations: `Cont.shift`, `Cont.reset`, `Cont.pure`, `map`, `flatMap`, `run(k)`, with answer-type modification |
| `Op`, `Effect[F]`, `Handler[F, R, B]` | YOUR OWN effect, declared in plain Scala 2: operations extend `Op`, `object Console extends Effect[Console]` is the whole declaration, and a handler gets each operation together with its continuation |
| `Source[A]` | streams: `Source(...)`, `range`, `unfold`, `fromEff`; `map`, `filter`, `take`, `takeWhile`, `drop`, `zipWithIndex`, `++`, `merge`; `runCollect`, `runForeach`, `runFold` |
| `Fiber[A]`, `Channel[A]` | concurrency: `Async.fork`, `par`, `race`, `sleep`, `timeout`; a fiber's `join`/`joinEither`/`cancel`; a bounded channel's `send`/`receive` (programs that wait), `offer`, `close`, `source` |
| `Schemas`, `Json`, `JsonSchema` (module `okay-scala2-codec`) | okay-codec from 2.13: `Schemas.product1`…`product16`, `sum`/`variant`, `constant` in place of `derives Schema`; JSON as text. `okay.codec.Schema`, `Cbor`, `Yaml` and `Validate` are used directly |
| `Response`, `Routes`, `GET`/`POST`/…, `Path`, `Requests`, `Server`, `Client` (module `okay-scala2-http`) | okay-http from 2.13: routing as pattern matching, `Server.use`/`start`, a client; `okay.http.Request`, `Method` and `Body` are used directly |
| `Db` (module `okay-scala2-sql`) | okay-sql from 2.13: `rows`/`all`/`update`/`verify`/`transaction` as `Eff` and `Source`; `okay.sql.SqlValue`, `Bad`, `Drift`, `Isolation` are used directly |
| `Chat`, `Model`, `Tools`, `Policy`, `Call` (module `okay-scala2-agent`) | okay-agent from 2.13: the agent loop with a persistent conversation, a scripted or real model, tools decoded by `Schema`, the context policy; `okay.agent.Turn` and `Reply` are used directly |
| `UiApp`, `UiHost`, `ScriptedHost` (module `okay-scala2-ui`) | okay-ui from 2.13: the loop as an `Eff`, terminal and Swing hosts, a scripted host for tests; `okay.ui.Ui`, `Event` and `Frame` are used directly |
| `WebSocket`, `WsClient`, `WsSession`, `WsServer` (module `okay-scala2-ws`) | WebSockets from 2.13: a client as `Eff`/`Source`, a server session as a fold, replayable without a socket; `okay.http.Frame` is used directly |
| `Choose`, `Search` | nondeterminism as a capability of `Eff`: `from`/`fail`/`guard`, `all`/`first`/`cut`/`ifte`, fair `interleave`/`fairBind`; `Search.bestOf`/`all`/`majority` over samples |
| `Guards` (module `okay-scala2-resilience`) | okay-resilience's breaker, bulkhead, limiter, hedge, deadline and retry around an `Eff`; the pieces themselves are used directly |
| `Prog[A]` | a program over `Async + Throws % Throwable`: suspended, failing, recoverable, runnable. `map`, `flatMap`, `attempt`, `recover`, `run()`, `runEither()`; `Prog.pure`, `delay`, `fail`, `fromEither`, `sequence` |
| `Bridge` | the Scala 3 side of `Prog`: `Bridge.lift(p: A ! Async)` and `Bridge.program(prog)`. 2.13 code never names it |

The walkthrough, from the build to a worker pool, is
**[okay from Scala 2.13](../scala2.md)**. Why the row becomes an
intersection is [theory ch. 13](../theory/13-rows-without-unions.md).
This page is the module's reference: the setup, one example per area,
the API with signatures, and the design.

## Setting up a 2.13 build

These are the settings `okay-scala2-probe` uses in this repository's
build.sbt, where the gate compiles a Scala 2.13 test suite with them.
The only difference is that the probe reaches the facade through
`dependsOn` + `projectDependencies`, while you reach it through a
library dependency. They were also checked in a separate consumer
project against a `publishLocal`, since okay is not on Maven Central
yet ([okay from Scala 2.13, section 1](../scala2.md#1-setting-up-the-build)
explains how to publish it and why each line is there):

```scala
scalaVersion := "2.13.18"
scalacOptions += "-Ytasty-reader"

libraryDependencies +=
  ("dev.okay" %% "okay-scala2" % "0.2.0-SNAPSHOT")
    .cross(CrossVersion.for2_13Use3)
    .exclude("org.scala-lang", "scala-library")

// Scala 3's standard library, BEHIND 2.13's
lazy val Scala3Stdlib = config("scala3Stdlib").hide
ivyConfigurations += Scala3Stdlib
libraryDependencies += "org.scala-lang" % "scala-library" % "3.9.0" % Scala3Stdlib
Seq(Compile, Runtime, Test).flatMap(c => Seq(
  c / dependencyClasspath ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value),
  c / dependencyClasspathAsJars ++= Classpaths.managedJars(Scala3Stdlib, Set("jar"), update.value)))
```

Why two standard libraries, and why in that order (all three measured
2026-09-22):

- Since 3.8, Scala 3's stdlib is published as `scala-library:3.x`. Its
  classes carry TASTy instead of Scala 2 pickles. If it comes FIRST,
  scalac 2.13 cannot read its own `Predef`: *Unsupported Scala 3 union
  in bounds of type T; found in method wrapRefArray in class
  scala.LowPriorityImplicits*. sbt also refuses outright to put a
  2.13 compiler under a newer stdlib (SIP-51). So the transitive one
  is excluded.
- If it is MISSING, compilation still fails (*could not find package
  scala.annotation.internal*), and at run time `scala.reflect.Enum`
  and the other Scala-3-only classes cannot be found. So it goes
  back in, at the end of the classpath.
- The jar is appended to BOTH `dependencyClasspath` (compilation,
  `test`) and `dependencyClasspathAsJars` (`sbt run` builds its
  classpath from that one). With only the first, a consumer project
  compiled and then failed on `run` with
  `NoClassDefFoundError: scala/reflect/Enum`.
- This is the same pair of jars as okay-spark's test classpath, only
  in the opposite order. Both are the same library compiled twice.

## Writing it

This code is copied from `okay-scala2/probe/src/test/scala/TestFromScala2.scala`,
which the gate compiles with scalac 2.13.18 under `-Xlint -Werror`:

```scala
import okay.scala2.Prog

val prog = for {
  a <- Prog.pure(20)
  b <- Prog.delay(a + 1)
} yield a + b + 1
assertEquals(prog.run(), 42)
```

`delay` is the door for code that may throw. A throw inside it
becomes the program's failure, the same failure as `Prog.fail`, so
`attempt`, `recover` and `runEither()` all see it. A throw from a
function passed to `map` or `flatMap` is not caught by anything, and
`run()` rethrows it.

## Several effects in one program: `Eff`

Everything a 2.13 program needs is in one package: `import okay.scala2._`.
The types there have the same names as the Scala 3 types they wrap.
Scala 2 cannot spell a union type, so the effect row is written as an
intersection of capabilities. This is the same shape as the
environment `R` in ZIO 1. The code below is copied from
`okay-scala2/probe/src/test/scala/TestEffFromScala2.scala`:

```scala
val prog: Eff[State[Int] with Writer[String], Int] = for {
  n <- State.get[Int]
  _ <- Writer.tell("saw " + n)
  _ <- State.put(n + 1)
  m <- State.get[Int]
  _ <- Writer.tell("now " + m)
} yield m * 10

assertEquals(Eff.run(Writer.run(State.run(1)(prog))), (Vector("saw 1", "now 2"), (2, 20)))
assertEquals(Eff.run(State.run(1)(Writer.run(prog))), (2, (Vector("saw 1", "now 2"), 20)))
```

- Each handler removes one capability from the row: `State.run(1)`
  turns `Eff[State[Int] with R, A]` into `Eff[R, (Int, A)]`. scalac
  2.13 infers `R` by itself.
- The handler order decides the shape of the answer, exactly as in
  okay's Scala 3 API.
- `Eff.run` accepts only `Eff[Any, A]`, so a program with an unhandled
  effect does not compile. The probe checks this with `compileErrors`.
  The message says `type mismatch` and does not name the missing
  handler.
- `Eff.runAsync` runs a program whose only remaining effect is `Async`.
  `Async.attempt` turns a throw into a `Throws[Throwable]` failure.
- `Eff.fromProg` and `Eff.toProg` convert between `Prog` and
  `Eff[Async with Throws[Throwable], A]`; they are the same program.

Underneath are okay's own `Free` and okay's own handlers. On the Scala 2
side the row is only a phantom type, so the facade needs ONE cast: it
stores the program at a single top row, and each handler re-types it
at the concrete row it handles. The reason is written beside that one
function (`Rows.coerce`).

## Continuations: `Cont`

`Cont[A, S, R]` is okay's continuation paramonad, and it is
stack-safe. `shift` captures the continuation up to the nearest
`reset` and may change the answer type (Danvy & Filinski, *Abstracting
Control*, LFP 1990, doi:10.1145/91556.91622; answer-type modification
and its typing: Asai, *On typing delimited continuations: three new
solutions to the printf problem*, HOSC 2009,
doi:10.1007/s10990-009-9049-5). The code below is copied from
`okay-scala2/probe/src/test/scala/TestContFromScala2.scala`:

```scala
val c: Cont[Int, Int, Int] = for {
  a <- Cont.shift[Int, Int, Int](k => k(k(10)))
  b <- Cont.pure[Int, Int](1)
} yield a + b
assertEquals(Cont.reset(c), 12)

val s: String = Cont.reset(Cont.shift[Int, Int, String](k => "k(5)=" + k(5)).map(_ * 2))
assertEquals(s, "k(5)=10")
```

Scala 2 cannot infer the type arguments of `shift` and `pure` from
where they are used, so write them out.

## Your own effect

In Scala 3, okay declares an effect with `derives Effect`. A Scala 2
build cannot run that derivation, but a handler only needs to test
whether a value is one of the effect's operations, and a `ClassTag`
answers that. So the whole declaration is ordinary Scala 2. The code
below is copied from
`okay-scala2/probe/src/test/scala/TestOwnEffectFromScala2.scala`:

```scala
sealed trait Console[A] extends Op[A]
final case class PrintLn(s: String) extends Console[Unit]
case object ReadLn extends Console[String]
object Console extends Effect[Console]
```

```scala
def console[R, B](out: ListBuffer[String], input: String): Handler[Console, R, B] =
  new Handler[Console, R, B] {
    def apply[X](op: Console[X], k: X => Eff[R, B]): Eff[R, B] = op match {
      case PrintLn(s) => out += s; k(())
      case ReadLn => k(input)
    }
  }

val prog: Eff[Effect[Console] with State[Int], String] = for {
  name <- Console.send(ReadLn)
  _ <- State.put(name.length)
  _ <- Console.send(PrintLn("hi " + name))
} yield name
val out = ListBuffer.empty[String]
val handled = Console.handle(prog)(a => Eff.pure(a))(console(out, "ada"))
assertEquals(Eff.run(State.run(0)(handled)), (3, "ada"))
```

- The capability in the row is `Effect[Console]`.
- A handler receives each operation together with its continuation
  `k` (Plotkin & Pretnar, *Handlers of Algebraic Effects*, ESOP 2009,
  doi:10.1007/978-3-642-00590-9_7). Calling `k` once resumes the
  program. Not calling it aborts the rest. Calling it more than once
  gives several answers; the probe's `Choose` handler collects all
  four outcomes of two flips.
- **For the LAST effect, use `Console.run(prog)(ret)(h)`, not
  `handle`.** With `handle` in that position, scalac 2.13 infers
  `R = Any`, and `-Xlint` warns "a type was inferred to be `Any`",
  which is an error under `-Werror`. `run` has no `R` to infer, and it
  returns the answer directly.
- Pattern matching `op match { case PrintLn(s) => k(()) }` typechecks
  in Scala 2, because matching on the case class tells scalac what `X`
  is in that branch.

## Streams: `Source`

okay's core `Source[A]` is a program that tells its elements and may
perform `Async` between them. In Scala 2 terms that is
`Eff[Writer[A] with Async, Unit]`. So there are two ways to get a
`Source`: build it from the constructors, or write it as an ordinary
for-comprehension and wrap it with `Source.fromEff`. The code below is
copied from `okay-scala2/probe/src/test/scala/TestSourceFromScala2.scala`:

```scala
val nats = Source.unfold(0)(n => Some((n, n + 1)))
assertEquals(collect(nats.map(_ * 2).take(4)), Vector(0, 2, 4, 6))
```

```scala
val lines: Eff[Writer[String] with Async, Unit] = for {
  a <- read()
  _ <- Writer.tell("line " + a)
  b <- read()
  _ <- Writer.tell("line " + b)
} yield ()
val src = Source.fromEff(lines).map(_.toUpperCase)
```

Here `collect(s)` is `Eff.runAsync(s.runCollect)`. Each terminal
operation (`runCollect`, `runForeach`, `runFold`) is an
`Eff[Async, _]`, so it composes with other programs until you run it.

- Once `take` or `takeWhile` has what it needs, it stops pulling from
  the source, so they work on infinite sources.
- `merge` runs both sources at once, one fiber each, feeding one
  channel. Elements come out in the order they arrive, not in turns.

## Fibers and channels

`Async.fork(e)` starts `e` on its own fiber, which is a virtual thread
on the JVM. Anything that waits (`join`, `send` on a full channel,
`receive` on an empty one, `sleep`) is an `Eff[Async, _]`, so it
composes like the rest. The code below is copied from
`okay-scala2/probe/src/test/scala/TestFibersChannelsFromScala2.scala`:

```scala
val ch = Channel[Int](4)
def produce(i: Int): Eff[Async, Unit] =
  if (i > 1000) Async.delay(ch.close())
  else ch.send(i).flatMap(_ => produce(i + 1))
def consume(acc: Vector[Int]): Eff[Async, Vector[Int]] =
  ch.receive.flatMap {
    case Some(n) => consume(acc :+ n)
    case None => Eff.pure(acc)
  }
val prog = for {
  p <- Async.fork(produce(1))
  got <- consume(Vector.empty)
  _ <- p.join
} yield got
assertEquals(Eff.runAsync(prog), (1 to 1000).toVector)
```

- `join` fails the same way the fiber failed. `joinEither` returns
  the failure as a `Left` instead.
- `race` returns the first answer and cancels the other fiber.
  `timeout` returns `None` if the deadline passes, and cancels the
  fiber.
- `channel.source` reads the channel as a `Source` that ends when the
  channel is closed.

## Not planned

- **Direct style** is built from Scala 3 macros, so Scala 2 cannot use
  it. Write for-comprehensions instead.

## API reference

The signatures are written as Scala 2 sees them, with `with` for an
intersection; the Scala 3 source writes `&`.

**`Eff[-R, A]`** — `map[B](f: A => B): Eff[R, B]`,
`flatMap[R1 <: R, B](f: A => Eff[R1, B]): Eff[R1, B]`.
`object Eff`: `pure[A](a: A): Eff[Any, A]`, `run[A](e: Eff[Any, A]): A`,
`runAsync[A](e: Eff[Async, A]): A`,
`fromProg[A](p: Prog[A]): Eff[Async with Throws[Throwable], A]`,
`toProg[A](e: Eff[Async with Throws[Throwable], A]): Prog[A]`.

| capability | operations | handler |
|---|---|---|
| `State[S]` | `get[S]: Eff[State[S], S]`, `put[S](s: S): Eff[State[S], Unit]`, `modify[S](f: S => S): Eff[State[S], Unit]` | `run[S, R, A](s: S)(e: Eff[State[S] with R, A]): Eff[R, (S, A)]` |
| `Reader[E]` | `ask[E]: Eff[Reader[E], E]` | `run[E, R, A](env: E)(e: Eff[Reader[E] with R, A]): Eff[R, A]` |
| `Writer[W]` | `tell[W](w: W): Eff[Writer[W], Unit]` | `run[W, R, A](e: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)]` |
| `Throws[E]` | `raise[E, A](e: E): Eff[Throws[E], A]` | `run[E, R, A](e: Eff[Throws[E] with R, A]): Eff[R, Either[E, A]]` |
| `Async` | `delay[A](a: => A): Eff[Async, A]`, `attempt[A](a: => A): Eff[Async with Throws[Throwable], A]`, `fork[A](e: Eff[Async, A]): Eff[Async, Fiber[A]]`, `par[A, B](a, b): Eff[Async, (A, B)]`, `race[A](a, b): Eff[Async, A]`, `sleep(millis: Long): Eff[Async, Unit]`, `timeout[A](millis: Long)(e): Eff[Async, Option[A]]` | `Eff.runAsync` |

**Your own effect** — `trait Op[+A]`;
`abstract class Effect[F[_]](implicit tag: ClassTag[F[Any]])` with
`send[A](op: F[A] with Op[A]): Eff[Effect[F], A]`,
`handle[R, A, B](e: Eff[Effect[F] with R, A])(ret: A => Eff[R, B])(h: Handler[F, R, B]): Eff[R, B]`,
`run[A, B](e: Eff[Effect[F], A])(ret: A => Eff[Any, B])(h: Handler[F, Any, B]): B`;
`trait Handler[F[_], R, B] { def apply[X](op: F[X], k: X => Eff[R, B]): Eff[R, B] }`.

**`Cont[A, S, R]`** — `map[B](f: A => B): Cont[B, S, R]`,
`flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R]`,
`run(k: A => S): R`. `object Cont`: `pure[A, R](a: A): Cont[A, R, R]`,
`shift[A, S, R](f: (A => S) => R): Cont[A, S, R]`,
`reset[A, R](c: Cont[A, A, R]): R`.

**`Source[A]`** — `map`, `filter`, `mapConcat[B](f: A => Iterable[B])`,
`take(n: Int)`, `takeWhile`, `drop(n: Int)`,
`zipWithIndex: Source[(A, Long)]`, `++(that: => Source[A])`,
`merge(that: Source[A])`; `runCollect: Eff[Async, Vector[A]]`,
`runForeach(f: A => Eff[Async, Unit]): Eff[Async, Unit]`,
`runFold[S](z: S)(f: (S, A) => S): Eff[Async, S]`,
`toEff: Eff[Writer[A] with Async, Unit]`. `object Source`:
`apply[A](as: A*)`, `fromIterable[A](as: Iterable[A])`, `empty[A]`,
`range(from: Long, until: Long): Source[Long]`,
`unfold[S, A](s: S)(f: S => Option[(A, S)])`,
`fromEff[A](e: Eff[Writer[A] with Async, Unit])`.

**`Fiber[A]`** — `join: Eff[Async, A]`,
`joinEither: Eff[Async, Either[Throwable, A]]`,
`cancel: Eff[Async, Unit]`.

**`Channel[A]`** — `Channel[A](capacity: Int)`;
`send(a: A): Eff[Async, Boolean]` (false once closed),
`receive: Eff[Async, Option[A]]` (None once closed and drained),
`offer(a: A): Boolean`, `close(): Unit`, `isClosed: Boolean`,
`source: Source[A]`.

**Codecs** (module `okay-scala2-codec`) — `Schemas.productN[A, F1..FN](name: String, n1..nN: String)(make: (F1..FN) => A)(parts: A => (F1..FN))(implicit s1..sN: => Schema[Fi]): Schema[A]` for N in 1..16 (for N = 1, `make: F1 => A` and `parts: A => F1`);
`Schemas.constant[A](name: String, value: A): Schema[A]`;
`Schemas.variant[A, C <: A](name: String)(implicit s: => Schema[C], tag: ClassTag[C]): Schemas.Variant[A]`;
`Schemas.sum[A](name: String)(variants: Schemas.Variant[A]*): Schema[A]`;
`Json.write[A](a: A)(implicit s: Schema[A]): String`,
`Json.read[A](text: String)(implicit s: Schema[A]): Either[String, A]`,
`Json.readStrict[A]` (the same, refusing repairable damage);
`JsonSchema.of[A](s: Schema[A]): String`.

**HTTP** (module `okay-scala2-http`) — `Response.text(body, status = 200)`, `.html`, `.bytes(body, contentType, status)`, `.json[A](a, status)(implicit Schema[A])`, `.status(code)`, `.notFound`, `.lines(src: Source[String], contentType, status)`; on a response `status`, `headers`, `header(name)`, `text`, `bytes`, `ok`, `withHeader`.
`Routes(pf: PartialFunction[Request, Eff[Async, Response]]): Request => Eff[Async, Response]` (404 when no case matches);
extractors `GET`, `POST`, `PUT`, `PATCH`, `DELETE` (`unapply(r: Request): Option[Request]`) and `Path` (`unapplySeq(r: Request): Option[Seq[String]]`);
`Requests.path(r)`, `.query(r, name)`, `.queryAll(r, name)`, `.text(r)`, `.json[A](r)`.
`Server.use[A](port: Int)(handler)(body: Int => Eff[Async, A]): Eff[Async, A]`, `Server.start(port)(handler): RunningServer` (`port`, `close()`).
`Client()`: `send(r: Request)`, `get(url)`, `post(url, body, contentType)`, `postJson[A](url, a)`, each an `Eff[Async, Response]`; `lines(r: Request): Source[String]`.

**SQL** (module `okay-scala2-sql`) — `Db.jdbc(connection: java.sql.Connection, fetchSize: Int = 64): Db`, `Db(sql: okay.sql.Sql): Db`;
`rows[A](query: String, params: SqlValue*)(implicit Schema[A]): Source[Either[Bad, A]]`, `rowsOf[A, P](query, p: P)`;
`all[A](query, params: SqlValue*): Eff[Async with Throws[Bad], Vector[A]]`, `allOf[A, P](query, p)`;
`update(query, params: SqlValue*): Eff[Async, Long]`, `updateOf[P](query, p)`;
`verify[A](query): Eff[Async, Vector[Drift]]`;
`transaction[A](isolation: Isolation = ReadCommitted, readOnly: Boolean = false)(body: Db => Eff[Async, A]): Eff[Async, A]`.

**Agents** (module `okay-scala2-agent`) — `Model.scripted(replies: String*)`, `Model.scriptedCalls(replies: (String, Seq[(String, String)])*)` (tool calls as `(name, JSON arguments)`), `Model.anthropic(apiKey, model, maxTokens = 1024)`, `Model.openAi(apiKey, model, url)`;
`Tools.empty.on[A](name, description)(run: A => String)(implicit Schema[A]): Tools`, `tools.declarations: Seq[(String, String, String)]` (name, description, JSON Schema text);
`Policy.all`, `Policy.window(budget: Int)`;
`Chat(model, tools = Tools.empty, policy = Policy.window(4000), maxSteps = 8, approve: Call => Boolean = _ => true)`, `chat.say(message): Eff[Async, String]`, `chat.transcript: Seq[okay.agent.Turn]`;
`Call(id: String, name: String, argsJson: String)`.

**UI** (module `okay-scala2-ui`) — `UiApp.run[S](init: S)(view: S => Ui)(update: (S, Event) => S)(host: UiHost): Eff[Async, S]`, `UiApp.runWith[S](...)(host, external: Source[Event])`, `UiApp.window[S](title)(init)(view)(update)`;
`UiHost.terminal()`, `UiHost.swing(root: java.awt.Container)`;
`ScriptedHost(events: Event*)` / `ScriptedHost.open(events: Event*)` (without the closing `Closed`), `.host: UiHost`, `.frames: Vector[Ui]`.
`Dialog.show(ui: Ui): Eff[Dialog, Event]`, `Dialog.ask[A](message)(implicit Schema[A]): Eff[Dialog, Option[A]]`, `Dialog.run[A](host: UiHost)(prog: Eff[Dialog, A]): Eff[Async, Option[A]]`, `Dialog.replay[A](prog, events: Seq[Event]): (Vector[Ui], Option[A])`; `Screens.of[S](init: S)(view: S => Ui)(update: (S, Event) => Either[Nav, S]): Screen`.
`FormState.blank[A](implicit Schema[A])`, `FormState.of[A](a: A)`; `form.view: Ui`, `form.edit(e: Event): FormState[A]`, `form.errors: Vector[(String, String)]`, `form.decoded: Either[String, A]`, `form.json: String`, `form.withLabels(labels: Map[String, String])`.

**WebSockets** (module `okay-scala2-ws`) — `WebSocket.connect(url): Eff[Async, WsClient]`, `WebSocket.binary(bytes: Array[Byte]): Frame`, `WebSocket.bytes(f: Frame): Option[Array[Byte]]`;
`WsClient`: `send(f: Frame)`, `sendText(text)`, `close()` (each `Eff[Async, Unit]`), `frames: Source[Frame]`, `texts: Source[String]`;
`WsSession.fold[S](init: S)(step: (S, Frame) => (S, Seq[Frame])): WsSession`, `WsSession.echo`, `WsSession.replay(s, incoming: Seq[Frame]): Vector[Frame]`;
`WsServer.use[A](port)(routes: Request => Eff[Async, Response])(sessions: PartialFunction[Request, WsSession])(body: Int => Eff[Async, A]): Eff[Async, A]`.

**Nondeterminism** — `Choose.from[A](as: A*): Eff[Choose, A]`, `Choose.fail[A]`, `Choose.guard(ok: Boolean)`;
`Choose.all[R, A](e: Eff[Choose with R, A]): Eff[R, Seq[A]]`, `Choose.first[R, A](n)(e): Eff[R, Seq[A]]`;
`Choose.cut[R <: Choose, A](e: Eff[R, A]): Eff[R, A]`, `Choose.ifte[R <: Choose, A, B](cond)(th: A => Eff[R, B])(el: => Eff[R, B])`, `Choose.interleave[R <: Choose, A](a, b)`, `Choose.fairBind[R <: Choose, A, B](m)(f)`;
`Search.bestOf[R, A](n)(gen: Eff[R, A])(ok: A => Boolean): Eff[R, Option[A]]`, `Search.all[R, A](n)(gen)(ok): Eff[R, Seq[A]]`, `Search.majority[A](answers: Seq[A]): Option[A]`.

**Resilience** (module `okay-scala2-resilience`) — `Guards.breaker[A](b: Breaker)(prog: Eff[Async, A], failing: Either[Throwable, A] => Boolean = _.isLeft)`, `Guards.bulkhead[A](b: Bulkhead)(prog)`, `Guards.limiter[A](l: Limiter, key: String = "")(prog)`, `Guards.hedge[A](afterMillis: Long, max: Int = 2)(prog)`, `Guards.deadline[A](d: Deadline)(prog)`, `Guards.retry[A](policy: LazyList[Long])(prog)`, each an `Eff[Async, A]`.

**Persist** (module `okay-scala2-persist`) — `Persist.topic(store: Store, name, partitions: Int = 1): Topic`, `Persist.typed[A](topic, version: Int = 1)(implicit Schema[A]): Typed[A]`, `Persist.stream(topic, partition, from: Long, chunk: Int = 256): Source[Record]`, `Persist.tail(topic, partition, from, chunk = 256, pollMillis = 25L): Source[Record]`.

**Transactions** (module `okay-scala2-stm`) — `Stm.ref[A](init: A): TRef[A]`, `Stm.atomically[A](tx: Eff[Tx, A]): Eff[Async, A]`; `Tx.read[A](r: TRef[A]): Eff[Tx, A]`, `Tx.write(r, a): Eff[Tx, Unit]`, `Tx.modify[A, B](r)(f: A => (A, B)): Eff[Tx, B]`, `Tx.update(r)(f: A => A): Eff[Tx, Unit]`, `Tx.retry[A]`, `Tx.check(cond: Boolean): Eff[Tx, Unit]`, `Tx.orElse[A](a, b): Eff[Tx, A]`.

**Stores** (module `okay-scala2-stores`) — `Caches.get(c: Cache[K, V], k): Eff[Async, Option[V]]`, `Caches.put(c, k, v)`, `Caches.invalidate(c, k)`, `Caches.getOrLoad(c, k)(load: K => Eff[Async, V]): Eff[Async, V]`, `Caches.writeThrough(c, k)(commit: Eff[Async, A]): Eff[Async, A]`, `Caches.drain(topic, c, keyOf: String => K, from: Long, max: Int = 512): Eff[Async, Long]`, `Caches.latest(v: View[K, V], k)`, `Caches.refresh(v)`;
`Blobs.put(b: Blob, key, bytes: Source[ArraySeq[Byte]]): Eff[Async, Etag]`, `Blobs.putBytes(b, key, bytes: Array[Byte])`, `Blobs.putFile(b, key, path: Path, chunk: Int = 65536)`, `Blobs.getBytes(b, key, range: Option[(Long, Long)] = None): Eff[Async, Either[String, Array[Byte]]]`, `Blobs.stream(b, key, range = None): Source[ArraySeq[Byte]]`, `Blobs.head(b, key): Eff[Async, Option[Meta]]`, `Blobs.list(b, prefix): Source[Meta]`, `Blobs.delete(b, key)`, `Blobs.backup(root: Path, b, prefix = "persist", active = true): Eff[Async, Vector[String]]`, `Blobs.restore(b, root, prefix = "persist")`;
`Documents.onTopic[A](topic, indexes: Map[String, A => String] = Map.empty)(implicit Schema[A]): Docs[A]`, `Documents.get(d: Docs[A], id): Eff[Async, Option[Docs.Versioned[A]]]`, `Documents.put(d, id, a, cond: Cond = Cond.Always): Eff[Async, PutResult]`, `Documents.delete(d, id, cond = Cond.Always)`, `Documents.query(d, field, equals, max: Int = 256): Source[(String, A)]`.

**Models** (module `okay-scala2-llm`) — `Llm.http: Transport`, `Llm.transport(post: (String, Map[String, String], String) => Source[String]): Transport`, `Llm.anthropic(transport, apiKey, model, messages: Seq[(String, String)], maxTokens: Int = 1024, url = ...): Source[String]`, `Llm.openAi(transport, apiKey, model, messages, maxTokens: Option[Int] = None, url = OpenAi.chatUrl): Source[String]`, `Llm.first[A](tokens: Source[String])(implicit Schema[A]): Eff[Async, Option[A]]`, `Llm.cut[A](tokens)(implicit Schema[A]): Eff[Async, Structured.Cut[A]]`.

**Retrieval** (module `okay-scala2-rag`) — `Rag.memory(embed: Seq[String] => Seq[Array[Float]]): VectorIndex`, `Rag.hashing(dim: Int = 64)`; `VectorIndex`: `add(sources: Seq[okay.rag.Source], budget: Int = 400, batch: Int = 32): Ingest.Progress`, `search(query, k): Seq[Scored]`, `hybrid(keywords: Postings, query, k): Seq[Scored]`, `size: Int`.

**MCP** (module `okay-scala2-mcp`) — `McpClient.connect(link, name, version): Eff[Async, McpClient]`, `McpClient.spawn(command: Seq[String], name, version)`; `McpClient`: `server: Option[(String, String)]`, `tools: Eff[Async, Seq[McpTool]]`, `call(name, argsJson: String): Eff[Async, String]`, `resources`, `read(uri): Eff[Async, Option[String]]`, `prompts`, `prompt(name, args: Map[String, String] = Map.empty): Eff[Async, Seq[Turn]]`; `McpTool(name, description, schema: String)`; `McpServer.run(link, name, version, tools: Tools, resources: Map[String, String] = Map.empty): Eff[Async, Unit]`; `McpLink.pair(): (Link, Link)`, `McpLink.of(in, out): Link`.

**Optics** (module `okay-scala2-optics`) — `Lens[S, A](get: S => A, set: (S, A) => S)`: `get`, `set(a): S => S`, `modify(f): S => S`; `Prism[S, A](preview: S => Option[A], review: A => S)`, `Prism.subtype[S, A <: S](implicit ClassTag[A])`, `Prism.some[A]`: `preview`, `review`, `set`, `modify`; `Affine[S, A](preview, set: (S, A) => S)`: `preview`, `set`, `modify`; `Traversal[S, A](parts: S => Vector[A], rebuild: (S, Vector[A]) => S)`, `Traversal.each[A]`, `Traversal.eachList[A]`: `toVector`, `set`, `modify`; `Iso[S, A](to: S => A, from: A => S)`: `get`, `reverseGet`, `modify`. Every kind has `andThen` with every kind, answering the kind the lattice gives.

**Workflows** (module `okay-scala2-workflow`) — `Workflow[Q, A]` (`Workflow.apply[Q, A]`): `ask(q: Q): Eff[Workflow[Q, A], A]`, `now: Eff[.., Long]`, `uuid`, `random`, `patch(id): Eff[.., Boolean]`, `sleep(millis): Eff[.., Unit]`, `awaitSignal(name): Eff[.., String]`, `awaitChild(id)`, `cancelled: Eff[.., Option[String]]`; `Workflows.drive[Q, A, R](wf, journal: List[Either[Wf.SysA, A]], runtime: Wf.Runtime = Wf.Runtime.live)(oracle: Q => A): (Wf.Step[Q, R], List[Either[Wf.SysA, A]])`, `Workflows.advance(wf, journal, runtime = live)` (same answer, no oracle), `Workflows.replay(wf, journal): Option[R]`, `Workflows.elapsed`, `Workflows.got(payload: String)`.
**Durable agent** — `Chat(model, tools, policy, maxSteps, approve, journal: Option[okay.agent.Durable.Journal] = None, onRepeat: String => Durable.OnRepeat = _ => Durable.OnRepeat.Fail)`.

**`Prog[A]`** — `map`, `flatMap`, `attempt: Prog[Either[Throwable, A]]`,
`recover(h: Throwable => Prog[A])`, `run(): A`,
`runEither(): Either[Throwable, A]`. `object Prog`: `pure`, `delay`,
`fail(e: Throwable)`, `fromEither`, `sequence(ps: List[Prog[A]])`.
`object Bridge` (Scala 3 only): `lift[A](p: A ! Async): Prog[A]`,
`program[A](p: Prog[A]): A ! (Async + Throws % Throwable)`.

## Why a facade and not a cross-build

okay's central type is `A ! Row`, and a row is a union:
`F + G = [A] =>> F[A] | G[A]`. This is okay's encoding of an
extensible effect row (Leijen, *Koka: Programming with Row Polymorphic
Effect Types*, MSFP 2014, doi:10.4204/EPTCS.153.8), over the freer
monad of Kiselyov & Ishii (*Freer Monads, More Extensible Effects*,
Haskell 2015, doi:10.1145/2804302.2804319). Scala 2 cannot spell a
union type. The library's combinators are also `inline`, and the
Scala 2 TASTy reader refuses to call those: *Unsupported Scala 3
inline method flatMap; found in class okay.Free*. So a 2.13 build
cannot use okay's own API directly. It gets a small, fixed-row
surface instead, the way `okay-java` gives Java one. The measurements
and the stages still to come (State/Reader/Writer carriers, streams)
are in [`specs/scala2-facade.md`](../../specs/scala2-facade.md).

References on the mechanism:
[the Scala 2 TASTy reader](https://docs.scala-lang.org/scala3/guides/migration/compatibility-classpath.html),
[SIP-51, dropping forward binary compatibility](https://docs.scala-lang.org/sips/drop-stdlib-forwards-bin-compat.html).
