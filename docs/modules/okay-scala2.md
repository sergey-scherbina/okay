# okay-scala2

okay from **Scala 2.13**. It is a facade module, written in Scala 3,
whose public types a Scala 2 compiler can read through its TASTy
reader (`-Ytasty-reader`). You write ordinary 2.13 code (for-comprehensions,
lambdas, pattern matches) and the real library runs underneath.

| | |
|---|---|
| `Eff[-R, +A]` | a program over an OPEN row of effects, written `A ! R` with `R` an intersection of capabilities joined by `+` — the aliases come from [`okay-scala2-prelude`](okay-scala2-prelude.md) with `import okay.scala2._` ([scala2.md](../scala2.md), section 3): `A ! (State[Int] + Writer[String])`, parentheses included. The operations and handlers live on the capabilities' companions under okay's names: `State.get`/`set`/`modify`/`handle`/`run`, `Reader.ask`/`run`, `Writer.tell`/`run`/`collect`, `Throws.raise`/`runEither`, `Async(a)`/`attempt`/`catching` |
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
| `Choose`, `Logic`, `Search` | nondeterminism as a capability of `Eff`, under okay's names: `Choose.choose`/`fail`/`guard`/`runChoice`; `Logic.observe`/`msplit`/`cut`/`ifte`/`gnot`, fair `interleave`/`fairBind`; `Search.bestOf`/`all`/`majority` over samples |
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

This code is copied from `scala2/okay-scala2/probe/src/test/scala/TestFromScala2.scala`,
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
environment `R` in ZIO 1. `+` is `with` and `A ! R` is `Eff[R, A]`,
two aliases declared once in the user's own package object
(`type +[R, S] = R with S`, `type ![A, R] = Eff[R, A]`; section 3 of
[scala2.md](../scala2.md)), so the type reads as okay's own. The code
below is copied from
`scala2/okay-scala2/probe/src/test/scala/TestEffFromScala2.scala`:

```scala
val prog: Int ! (State[Int] + Writer[String]) = for {
  n <- State.get[Int]
  _ <- Writer.tell("saw " + n)
  _ <- State.set(n + 1)
  m <- State.get[Int]
  _ <- Writer.tell("now " + m)
} yield m * 10

assertEquals(!.run(Writer.run(State.handle(1)(prog))), (Vector("saw 1", "now 2"), (2, 20)))
assertEquals(!.run(State.handle(1)(Writer.run(prog))), (2, (Vector("saw 1", "now 2"), 20)))
```

- Each handler removes one capability from the row: `State.handle(1)`
  turns `A ! (State[Int] with R)` into `(Int, A) ! R`. scalac
  2.13 infers `R` by itself.
- The handler order decides the shape of the answer, exactly as in
  okay's Scala 3 API.
- `Eff.run` accepts only `A ! Pure`, so a program with an unhandled
  effect does not compile. The probe checks this with `compileErrors`.
  The message says `type mismatch` and does not name the missing
  handler.
- `Eff.runAsync` runs a program whose only remaining effect is `Async`.
  `Async.attempt` turns a throw into a `Throws[Throwable]` failure.
- `Eff.fromProg` and `Eff.toProg` convert between `Prog` and
  `A ! (Async + Throws[Throwable])`; they are the same program.

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
`scala2/okay-scala2/probe/src/test/scala/TestContFromScala2.scala`:

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
`scala2/okay-scala2/probe/src/test/scala/TestOwnEffectFromScala2.scala`:

```scala
sealed trait Console[A] extends Op[A]
final case class PrintLn(s: String) extends Console[Unit]
case object ReadLn extends Console[String]
object Console extends Effect[Console]
```

```scala
def console[R, B](out: ListBuffer[String], input: String): Handler[Console, R, B] =
  new Handler[Console, R, B] {
    def apply[X](op: Console[X], k: X => B ! R): B ! R = op match {
      case PrintLn(s) => out += s; k(())
      case ReadLn => k(input)
    }
  }

val prog: String ! (Effect[Console] + State[Int]) = for {
  name <- Console.send(ReadLn)
  _ <- State.set(name.length)
  _ <- Console.send(PrintLn("hi " + name))
} yield name
val out = ListBuffer.empty[String]
val handled = Console.handle(prog)(a => pure(a))(console(out, "ada"))
assertEquals(!.run(State.handle(0)(handled)), (3, "ada"))
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
`Unit ! (Writer[A] + Async)`. So there are two ways to get a
`Source`: build it from the constructors, or write it as an ordinary
for-comprehension and wrap it with `Source.fromEff`. The code below is
copied from `scala2/okay-scala2/probe/src/test/scala/TestSourceFromScala2.scala`:

```scala
val nats = Source.unfold(0)(n => Some((n, n + 1)))
assertEquals(collect(nats.map(_ * 2).take(4)), Vector(0, 2, 4, 6))
```

```scala
val lines: Unit ! (Writer[String] + Async) = for {
  a <- read()
  _ <- Writer.tell("line " + a)
  b <- read()
  _ <- Writer.tell("line " + b)
} yield ()
val src = Source.fromEff(lines).map(_.toUpperCase)
```

Here `collect(s)` is `s.runCollect.runWith`. Each terminal
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
`scala2/okay-scala2/probe/src/test/scala/TestFibersChannelsFromScala2.scala`:

```scala
val ch = Channel[Int](4)
def produce(i: Int): Unit ! Async =
  if (i > 1000) Async(ch.close())
  else ch.send(i).flatMap(_ => produce(i + 1))
def consume(acc: Vector[Int]): Vector[Int] ! Async =
  ch.receive.flatMap {
    case Some(n) => consume(acc :+ n)
    case None => pure(acc)
  }
val prog = for {
  p <- Async.fork(produce(1))
  got <- consume(Vector.empty)
  _ <- p.join
} yield got
assertEquals(prog.runWith, (1 to 1000).toVector)
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

**`Eff[-R, A]`** (written `A ! R` below) — `map[B](f: A => B): B ! R`,
`flatMap[R1 <: R, B](f: A => B ! R1): B ! R1`.
`object Eff`: `pure[A](a: A): A ! Pure`, `run[A](e: A ! Pure): A`,
`runAsync[A](e: A ! Async): A`,
`fromProg[A](p: Prog[A]): A ! (Async + Throws[Throwable])`,
`toProg[A](e: A ! (Async + Throws[Throwable])): Prog[A]`.

| capability | operations | handler |
|---|---|---|
| `State[S]` | `get[S]: S ! State[S]`, `put[S](s: S): Unit ! State[S]`, `modify[S](f: S => S): Unit ! State[S]` | `run[S, R, A](s: S)(e: A ! (State[S] with R)): (S, A) ! R` |
| `Reader[E]` | `ask[E]: E ! Reader[E]` | `run[E, R, A](env: E)(e: A ! (Reader[E] with R)): A ! R` |
| `Writer[W]` | `tell[W](w: W): Unit ! Writer[W]` | `run[W, R, A](e: A ! (Writer[W] with R)): (Vector[W], A) ! R` |
| `Throws[E]` | `raise[E, A](e: E): A ! Throws[E]` | `run[E, R, A](e: A ! (Throws[E] with R)): Either[E, A] ! R` |
| `Async` | `delay[A](a: => A): A ! Async`, `attempt[A](a: => A): A ! (Async + Throws[Throwable])`, `fork[A](e: A ! Async): Fiber[A] ! Async`, `par[A, B](a, b): (A, B) ! Async`, `race[A](a, b): A ! Async`, `sleep(millis: Long): Unit ! Async`, `timeout[A](millis: Long)(e): Option[A] ! Async` | `Eff.runAsync` |

**Your own effect** — `trait Op[+A]`;
`abstract class Effect[F[_]](implicit tag: ClassTag[F[Any]])` with
`send[A](op: F[A] with Op[A]): A ! Effect[F]`,
`handle[R, A, B](e: A ! (Effect[F] with R))(ret: A => B ! R)(h: Handler[F, R, B]): B ! R`,
`run[A, B](e: A ! Effect[F])(ret: A => B ! Pure)(h: Handler[F, Pure, B]): B`;
`trait Handler[F[_], R, B] { def apply[X](op: F[X], k: X => B ! R): B ! R }`.

**`Cont[A, S, R]`** — `map[B](f: A => B): Cont[B, S, R]`,
`flatMap[B, S2](f: A => Cont[B, S2, S]): Cont[B, S2, R]`,
`run(k: A => S): R`. `object Cont`: `pure[A, R](a: A): Cont[A, R, R]`,
`shift[A, S, R](f: (A => S) => R): Cont[A, S, R]`,
`reset[A, R](c: Cont[A, A, R]): R`.

**`Source[A]`** — `map`, `filter`, `mapConcat[B](f: A => Iterable[B])`,
`take(n: Int)`, `takeWhile`, `drop(n: Int)`,
`zipWithIndex: Source[(A, Long)]`, `++(that: => Source[A])`,
`merge(that: Source[A])`; `runCollect: Vector[A] ! Async`,
`runForeach(f: A => Unit ! Async): Unit ! Async`,
`runFold[S](z: S)(f: (S, A) => S): S ! Async`,
`toEff: Unit ! (Writer[A] + Async)`. `object Source`:
`apply[A](as: A*)`, `fromIterable[A](as: Iterable[A])`, `empty[A]`,
`range(from: Long, until: Long): Source[Long]`,
`unfold[S, A](s: S)(f: S => Option[(A, S)])`,
`fromEff[A](e: Unit ! (Writer[A] + Async))`.

**`Fiber[A]`** — `join: A ! Async`,
`joinEither: Either[Throwable, A] ! Async`,
`cancel: Unit ! Async`.

**`Channel[A]`** — `Channel[A](capacity: Int)`;
`send(a: A): Boolean ! Async` (false once closed),
`receive: Option[A] ! Async` (None once closed and drained),
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
`Routes(pf: PartialFunction[Request, Response ! Async]): Request => Response ! Async` (404 when no case matches);
extractors `GET`, `POST`, `PUT`, `PATCH`, `DELETE` (`unapply(r: Request): Option[Request]`) and `Path` (`unapplySeq(r: Request): Option[Seq[String]]`);
`Requests.path(r)`, `.query(r, name)`, `.queryAll(r, name)`, `.text(r)`, `.json[A](r)`.
`Server.use[A](port: Int)(handler)(body: Int => A ! Async): A ! Async`, `Server.start(port)(handler): RunningServer` (`port`, `close()`).
`Client()`: `send(r: Request)`, `get(url)`, `post(url, body, contentType)`, `postJson[A](url, a)`, each an `Response ! Async`; `lines(r: Request): Source[String]`.

**SQL** (module `okay-scala2-sql`) — `Db.jdbc(connection: java.sql.Connection, fetchSize: Int = 64): Db`, `Db(sql: okay.sql.Sql): Db`;
`rows[A](query: String, params: SqlValue*)(implicit Schema[A]): Source[Either[Bad, A]]`, `rowsOf[A, P](query, p: P)`;
`all[A](query, params: SqlValue*): Vector[A] ! (Async + Throws[Bad])`, `allOf[A, P](query, p)`;
`update(query, params: SqlValue*): Long ! Async`, `updateOf[P](query, p)`;
`verify[A](query): Vector[Drift] ! Async`;
`transaction[A](isolation: Isolation = ReadCommitted, readOnly: Boolean = false)(body: Db => A ! Async): A ! Async`.

**Agents** (module `okay-scala2-agent`) — `Model.scripted(replies: String*)`, `Model.scriptedCalls(replies: (String, Seq[(String, String)])*)` (tool calls as `(name, JSON arguments)`), `Model.anthropic(apiKey, model, maxTokens = 1024)`, `Model.openAi(apiKey, model, url)`;
`Tools.empty.on[A](name, description)(run: A => String)(implicit Schema[A]): Tools`, `tools.declarations: Seq[(String, String, String)]` (name, description, JSON Schema text);
`Policy.all`, `Policy.window(budget: Int)`;
`Chat(model, tools = Tools.empty, policy = Policy.window(4000), maxSteps = 8, approve: Call => Boolean = _ => true)`, `chat.say(message): String ! Async`, `chat.transcript: Seq[okay.agent.Turn]`;
`Call(id: String, name: String, argsJson: String)`.

**UI** (module `okay-scala2-ui`) — `UiApp.run[S](init: S)(view: S => Ui)(update: (S, Event) => S)(host: UiHost): S ! Async`, `UiApp.runWith[S](...)(host, external: Source[Event])`, `UiApp.window[S](title)(init)(view)(update)`;
`UiHost.terminal()`, `UiHost.swing(root: java.awt.Container)`;
`ScriptedHost(events: Event*)` / `ScriptedHost.open(events: Event*)` (without the closing `Closed`), `.host: UiHost`, `.frames: Vector[Ui]`.
`Dialog.show(ui: Ui): Event ! Dialog`, `Dialog.ask[A](message)(implicit Schema[A]): Option[A] ! Dialog`, `Dialog.run[A](host: UiHost)(prog: A ! Dialog): Option[A] ! Async`, `Dialog.replay[A](prog, events: Seq[Event]): (Vector[Ui], Option[A])`; `Screens.of[S](init: S)(view: S => Ui)(update: (S, Event) => Either[Nav, S]): Screen`.
`FormState.blank[A](implicit Schema[A])`, `FormState.of[A](a: A)`; `form.view: Ui`, `form.edit(e: Event): FormState[A]`, `form.errors: Vector[(String, String)]`, `form.decoded: Either[String, A]`, `form.json: String`, `form.withLabels(labels: Map[String, String])`.

**WebSockets** (module `okay-scala2-ws`) — `WebSocket.connect(url): WsClient ! Async`, `WebSocket.binary(bytes: Array[Byte]): Frame`, `WebSocket.bytes(f: Frame): Option[Array[Byte]]`;
`WsClient`: `send(f: Frame)`, `sendText(text)`, `close()` (each `Unit ! Async`), `frames: Source[Frame]`, `texts: Source[String]`;
`WsSession.fold[S](init: S)(step: (S, Frame) => (S, Seq[Frame])): WsSession`, `WsSession.echo`, `WsSession.replay(s, incoming: Seq[Frame]): Vector[Frame]`;
`WsServer.use[A](port)(routes: Request => Response ! Async)(sessions: PartialFunction[Request, WsSession])(body: Int => A ! Async): A ! Async`.

**Nondeterminism** — `Choose.choose[A](as: A*): A ! Choose`, `Choose.fail[A]`, `Choose.guard(ok: Boolean)`;
`Choose.all[R, A](e: A ! (Choose with R)): Seq[A] ! R`, `Choose.first[R, A](n)(e): Seq[A] ! R`;
`Logic.cut[R <: Choose, A](e: A ! R): A ! R`, `Logic.ifte[R <: Choose, A, B](cond)(th: A => B ! R)(el: => B ! R)`, `Logic.interleave[R <: Choose, A](a, b)`, `Logic.fairBind[R <: Choose, A, B](m)(f)`;
`Search.bestOf[R, A](n)(gen: A ! R)(ok: A => Boolean): Option[A] ! R`, `Search.all[R, A](n)(gen)(ok): Seq[A] ! R`, `Search.majority[A](answers: Seq[A]): Option[A]`.

**Resilience** (module `okay-scala2-resilience`) — `Guards.breaker[A](b: Breaker)(prog: A ! Async, failing: Either[Throwable, A] => Boolean = _.isLeft)`, `Guards.bulkhead[A](b: Bulkhead)(prog)`, `Guards.limiter[A](l: Limiter, key: String = "")(prog)`, `Guards.hedge[A](afterMillis: Long, max: Int = 2)(prog)`, `Guards.deadline[A](d: Deadline)(prog)`, `Guards.retry[A](policy: LazyList[Long])(prog)`, each an `A ! Async`.

**Persist** (module `okay-scala2-persist`) — `Persist.topic(store: Store, name, partitions: Int = 1): Topic`, `Persist.typed[A](topic, version: Int = 1)(implicit Schema[A]): Typed[A]`, `Persist.stream(topic, partition, from: Long, chunk: Int = 256): Source[Record]`, `Persist.tail(topic, partition, from, chunk = 256, pollMillis = 25L): Source[Record]`.

**Transactions** (module `okay-scala2-stm`) — `Stm.ref[A](init: A): TRef[A]`, `Stm.atomically[A](tx: A ! Tx): A ! Async`; `Tx.read[A](r: TRef[A]): A ! Tx`, `Tx.write(r, a): Unit ! Tx`, `Tx.modify[A, B](r)(f: A => (A, B)): B ! Tx`, `Tx.update(r)(f: A => A): Unit ! Tx`, `Tx.retry[A]`, `Tx.check(cond: Boolean): Unit ! Tx`, `Tx.orElse[A](a, b): A ! Tx`.

**Stores** (module `okay-scala2-stores`) — `Caches.get(c: Cache[K, V], k): Option[V] ! Async`, `Caches.put(c, k, v)`, `Caches.invalidate(c, k)`, `Caches.getOrLoad(c, k)(load: K => V ! Async): V ! Async`, `Caches.writeThrough(c, k)(commit: A ! Async): A ! Async`, `Caches.drain(topic, c, keyOf: String => K, from: Long, max: Int = 512): Long ! Async`, `Caches.latest(v: View[K, V], k)`, `Caches.refresh(v)`;
`Blobs.put(b: Blob, key, bytes: Source[ArraySeq[Byte]]): Etag ! Async`, `Blobs.putBytes(b, key, bytes: Array[Byte])`, `Blobs.putFile(b, key, path: Path, chunk: Int = 65536)`, `Blobs.getBytes(b, key, range: Option[(Long, Long)] = None): Either[String, Array[Byte]] ! Async`, `Blobs.stream(b, key, range = None): Source[ArraySeq[Byte]]`, `Blobs.head(b, key): Option[Meta] ! Async`, `Blobs.list(b, prefix): Source[Meta]`, `Blobs.delete(b, key)`, `Blobs.backup(root: Path, b, prefix = "persist", active = true): Vector[String] ! Async`, `Blobs.restore(b, root, prefix = "persist")`;
`Documents.onTopic[A](topic, indexes: Map[String, A => String] = Map.empty)(implicit Schema[A]): Docs[A]`, `Documents.get(d: Docs[A], id): Option[Docs.Versioned[A]] ! Async`, `Documents.put(d, id, a, cond: Cond = Cond.Always): PutResult ! Async`, `Documents.delete(d, id, cond = Cond.Always)`, `Documents.query(d, field, equals, max: Int = 256): Source[(String, A)]`.

**Models** (module `okay-scala2-llm`) — `Llm.http: Transport`, `Llm.transport(post: (String, Map[String, String], String) => Source[String]): Transport`, `Llm.anthropic(transport, apiKey, model, messages: Seq[(String, String)], maxTokens: Int = 1024, url = ...): Source[String]`, `Llm.openAi(transport, apiKey, model, messages, maxTokens: Option[Int] = None, url = OpenAi.chatUrl): Source[String]`, `Llm.first[A](tokens: Source[String])(implicit Schema[A]): Option[A] ! Async`, `Llm.cut[A](tokens)(implicit Schema[A]): Structured.Cut[A] ! Async`.

**Retrieval** (module `okay-scala2-rag`) — `Rag.memory(embed: Seq[String] => Seq[Array[Float]]): VectorIndex`, `Rag.hashing(dim: Int = 64)`; `VectorIndex`: `add(sources: Seq[okay.rag.Source], budget: Int = 400, batch: Int = 32): Ingest.Progress`, `search(query, k): Seq[Scored]`, `hybrid(keywords: Postings, query, k): Seq[Scored]`, `size: Int`.
`Rag.pgvector(db: Db, table: String, dim: Int, embed, metric: PgVector.Metric = Cosine): PgIndex ! Async`; `PgIndex`: `add(sources, budget = 400, batch = 32): Ingest.Progress ! Async`, `search(query, k): Seq[Scored] ! Async`, `hybrid(keywords, query, k): Seq[Scored] ! Async`, `size: Int ! Async`.

**MCP** (module `okay-scala2-mcp`) — `McpClient.connect(link, name, version): McpClient ! Async`, `McpClient.spawn(command: Seq[String], name, version)`; `McpClient`: `server: Option[(String, String)]`, `tools: Seq[McpTool] ! Async`, `call(name, argsJson: String): String ! Async`, `resources`, `read(uri): Option[String] ! Async`, `prompts`, `prompt(name, args: Map[String, String] = Map.empty): Seq[Turn] ! Async`; `McpTool(name, description, schema: String)`; `McpServer.run(link, name, version, tools: Tools, resources: Map[String, String] = Map.empty): Unit ! Async`; `McpLink.pair(): (Link, Link)`, `McpLink.of(in, out): Link`.

**Optics** (module `okay-scala2-optics`) — `Lens[S, A](get: S => A, set: (S, A) => S)`: `get`, `set(a): S => S`, `modify(f): S => S`; `Prism[S, A](preview: S => Option[A], review: A => S)`, `Prism.subtype[S, A <: S](implicit ClassTag[A])`, `Prism.some[A]`: `preview`, `review`, `set`, `modify`; `Affine[S, A](preview, set: (S, A) => S)`: `preview`, `set`, `modify`; `Traversal[S, A](parts: S => Vector[A], rebuild: (S, Vector[A]) => S)`, `Traversal.each[A]`, `Traversal.eachList[A]`: `toVector`, `set`, `modify`; `Iso[S, A](to: S => A, from: A => S)`: `get`, `reverseGet`, `modify`. Every kind has `andThen` with every kind, answering the kind the lattice gives.

**Workflows** (module `okay-scala2-workflow`) — `Workflow[Q, A]` (`Workflow.apply[Q, A]`): `ask(q: Q): A ! Workflow[Q, A]`, `now: Long ! ..`, `uuid`, `random`, `patch(id): Boolean ! ..`, `sleep(millis): Unit ! ..`, `awaitSignal(name): String ! ..`, `awaitChild(id)`, `cancelled: Option[String] ! ..`; `Workflows.drive[Q, A, R](wf, journal: List[Either[Wf.SysA, A]], runtime: Wf.Runtime = Wf.Runtime.live)(oracle: Q => A): (Wf.Step[Q, R], List[Either[Wf.SysA, A]])`, `Workflows.advance(wf, journal, runtime = live)` (same answer, no oracle), `Workflows.replay(wf, journal): Option[R]`, `Workflows.elapsed`, `Workflows.got(payload: String)`.
**Durable agent** — `Chat(model, tools, policy, maxSteps, approve, journal: Option[okay.agent.Durable.Journal] = None, onRepeat: String => Durable.OnRepeat = _ => Durable.OnRepeat.Fail)`.

**Services** (module `okay-scala2-services`) — `Actors.spawn[S, M](init)(behavior: (S, M) => S ! Async): ActorRef[M] ! Async`, `Actors.spawn(init, supervise: Supervise[S], capacity: Int)(behavior)`, `Actors.child(parent, init, supervise = Supervise.Stop)(behavior)`, `Actors.tell(actor, m): Boolean ! Async`, `Actors.ask[M, R](actor, within: Long)(message: Reply[R] => M): Option[R] ! Async`, `Actors.stop(actor)`;
`Outboxes.enqueue(outbox, db: Db, topic, value: Array[Byte], key = Array.empty, part = 0): String ! Async`, `Outboxes.relayOnce(outbox, db, store, batch = 256): Int ! Async`, `Outboxes.pending(outbox, db): Long ! Async`, `Outboxes.first(inbox, db, id): Boolean ! Async`, `Outboxes.once[A](inbox, db, id)(body: A ! Async): Option[A] ! Async`;
`Logs.debug/info/warn/error(message, fields: (String, String)*): Eff[Writer[Log.Line], Unit]`, `Logs.failure(message, e, fields*)`, `Logs.to[R, A](write: Log.Line => Unit, min = Log.Level.Info, clock = ...)(e: Eff[Writer[Log.Line] & R, A]): Eff[R, A]`; `Tracing.span[A](tracer, name, attrs: (String, String)*)(e: Eff[Async, A]): Eff[Async, A]`;
`Operations.routes(store, lifecycle: Option[Lifecycle] = None, red: Seq[Red] = Nil): PartialFunction[Request, Response ! Async]`, `Operations.measured(red, label: Request => String)(routes)`, `Operations.admitted(lifecycle)(routes)`, `Operations.drain(lifecycle, graceMillis): Boolean ! Async`;
`Kafkas.source[K, V](consumer, pollMillis = 1000): Source[ConsumerRecord[K, V]]`, `Kafkas.commit(consumer): Unit ! Async`, `Kafkas.send(producer, records: Seq[ProducerRecord[K, V]]): Unit ! Async`; `Postgres.connect(host, port, user, password, database): Db ! Async`.

**`Prog[A]`** — `map`, `flatMap`, `attempt: Prog[Either[Throwable, A]]`,
`recover(h: Throwable => Prog[A])`, `run(): A`,
`runEither(): Either[Throwable, A]`. `object Prog`: `pure`, `delay`,
`fail(e: Throwable)`, `fromEither`, `sequence(ps: List[Prog[A]])`.
`object Bridge` (Scala 3 only): `lift[A](p: A ! Async): Prog[A]`,
`program[A](p: Prog[A]): A ! Async + Throws % Throwable`.

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
