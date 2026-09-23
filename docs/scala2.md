# okay from Scala 2.13

okay is a Scala 3 library. This guide is for a codebase that is still
on **Scala 2.13** and wants to use it: effects, several in one program,
your own effects, continuations, streams, fibers and channels, and above
them codecs, HTTP, SQL, agents and UI. It all
lives in one package, `okay.scala2`, from the module `okay-scala2`.

Every snippet below was copied from
`okay-scala2/probe/src/test/scala/TestScala2Guide.scala`. That file is
a Scala 2.13 suite, compiled by scalac 2.13.18 under
`-Xlint -Werror` in this repository's ordinary gate. So the examples
compile and pass as written, in the Scala 2 dialect.

Contents:

1. [Setting up the build](#1-setting-up-the-build)
2. [A first program: `Prog`](#2-a-first-program-prog)
3. [Several effects in one program: `Eff`](#3-several-effects-in-one-program-eff)
4. [Failure](#4-failure)
5. [Your own effect](#5-your-own-effect)
6. [Continuations: `Cont`](#6-continuations-cont)
7. [Streams: `Source`](#7-streams-source)
8. [Fibers and channels](#8-fibers-and-channels)
8a. [Codecs: JSON, CBOR, JSON Schema](#8a-codecs-json-cbor-json-schema)
8b. [HTTP: routes, a server, a client](#8b-http-routes-a-server-a-client)
8c. [SQL: queries and transactions](#8c-sql-queries-and-transactions)
8d. [Agents: a model, tools, a conversation](#8d-agents-a-model-tools-a-conversation)
8e. [UI: the view as a value, the loop as a fold](#8e-ui-the-view-as-a-value-the-loop-as-a-fold)
8f. [Forms from a Schema](#8f-forms-from-a-schema)
8g. [WebSockets](#8g-websockets)
8h. [Nondeterminism and search](#8h-nondeterminism-and-search)
8i. [Scenarios and screens: Dialog and Nav](#8i-scenarios-and-screens-dialog-and-nav)
8j. [Resilience: breaker, bulkhead, limiter, hedge, deadline, retry](#8j-resilience-breaker-bulkhead-limiter-hedge-deadline-retry)
9. [Scala 3 and Scala 2, side by side](#9-scala-3-and-scala-2-side-by-side)
10. [Errors you may see, and what they mean](#10-errors-you-may-see-and-what-they-mean)
11. [What is not here, and why](#11-what-is-not-here-and-why)
12. [How it works](#12-how-it-works)

---

## 1. Setting up the build

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

**okay is not on Maven Central yet.** Until it is, publish it to your
own machine from a checkout of okay, as
[Building a chat application](building-a-chat-app.md#1-get-the-library--it-is-not-published-yet)
describes. For this module, publishing it and what it depends on is
enough:

```
scripts/gate.sh "okayJVM/publishLocal; okayAsyncJVM/publishLocal; okayPlatformJVM/publishLocal; okayStreamJVM/publishLocal; okayScala2/publishLocal"
```

The version is `ThisBuild / version` in okay's build.sbt (0.2.0-SNAPSHOT
at the time of writing: the next release, as a snapshot). A snapshot
you re-publish is not always noticed by a build that already resolved
it: `sbt reload`, or delete `~/.ivy2/local/dev.okay/<module>` and publish
again.

This setup was checked twice. The first check is this repository's own
2.13 project, `okayScala2Probe` in build.sbt, which uses the same
settings, except that it depends on the module through `dependsOn` and
so excludes the stdlib through `projectDependencies`. The second check
was a separate sbt project outside the repository, against the
`publishLocal` above. There, the block above compiled, `sbt run`
printed the right answers both forked and unforked, and an unforked
`sbt test` passed.

What each line is for:

- `-Ytasty-reader` lets scalac 2.13 read Scala 3 classes. okay is
  published as Scala 3, so without it nothing resolves.
- `for2_13Use3` asks for the `_3` artifact from a 2.13 build.
- Two standard libraries. Since Scala 3.8 the Scala 3 stdlib is
  published as `scala-library:3.x`, and it carries Scala 3 type
  information (TASTy) where the 2.13 jar carries Scala 2's. scalac 2.13
  must read its OWN stdlib first. The 3.9 one must also be present,
  behind it: some classes okay needs exist only there, at compile time
  and at run time. The transitive jar is excluded because sbt refuses
  to put a 2.13 compiler under a newer stdlib, and it is then appended
  at the end. Section 10 lists the error each mistake here produces.
- **Both** `dependencyClasspath` and `dependencyClasspathAsJars`. The
  first serves compilation and `test`. `sbt run` builds its classpath
  from the second. With only the first, the consumer project compiled
  and then failed on `run`, forked or not, with
  `NoClassDefFoundError: scala/reflect/Enum`.
- Use JDK 21 or newer, as for okay in general
  ([specs/jdk-compatibility.md](../specs/jdk-compatibility.md)): on
  the JVM, okay runs fibers on virtual threads.

Then, in your code:

```scala
import okay.scala2._
```

## 2. A first program: `Prog`

`Prog[A]` is a program that may suspend, fail, and be run. It is
okay's `Async + Throws % Throwable` behind a type Scala 2 can read.
Building one runs nothing; `run()` runs it on the calling thread.

```scala
val greeting: Prog[String] = for {
  name <- Prog.delay(sys.props.getOrElse("user.name", "world"))
  n <- Prog.pure(name.length)
} yield "hello " + name + " (" + n + ")"

assert(greeting.run().startsWith("hello "))
```

`Prog.delay` is where code that might throw belongs. An exception
thrown inside it becomes the program's failure, which `runEither()`,
`attempt` and `recover` all see:

```scala
def parse(s: String): Prog[Int] = Prog.delay(s.trim.toInt)

assertEquals(parse(" 42 ").run(), 42)
assert(parse("x").runEither().isLeft)
assertEquals(parse("x").recover(_ => Prog.pure(0)).run(), 0)
```

`Prog` is the simple case. If you need state, a log, configuration, or
your own effects next to it, use `Eff`.

## 3. Several effects in one program: `Eff`

`Eff[R, A]` is a program that answers `A` and needs the effects `R`.
`R` is written as an intersection of **capabilities**:

| capability | operations | handler |
|---|---|---|
| `State[S]` | `State.get`, `State.put`, `State.modify` | `State.run(initial)(prog)` gives `(finalState, answer)` |
| `Reader[E]` | `Reader.ask` | `Reader.run(env)(prog)` |
| `Writer[W]` | `Writer.tell` | `Writer.run(prog)` gives `(Vector[W], answer)` |
| `Throws[E]` | `Throws.raise` | `Throws.run(prog)` gives `Either[E, answer]` |
| `Async` | `Async.delay`, `Async.attempt`, `fork`, `sleep`, ... | `Eff.runAsync(prog)` |

A function that needs one capability is declared with just that one.
`Eff` is contravariant in `R`, so such a program fits any wider row
without conversion. Here `count` needs only `State` and is used inside
a program that also needs `Writer`:

```scala
def count(word: String): Eff[State[Map[String, Int]], Unit] =
  State.modify[Map[String, Int]](m => m.updated(word, m.getOrElse(word, 0) + 1))

def countAll(text: String): Eff[State[Map[String, Int]] with Writer[String], Int] = {
  val words = text.split("\\s+").toList.filter(_.nonEmpty)
  words.foldLeft(Eff.pure(0): Eff[State[Map[String, Int]] with Writer[String], Int]) { (acc, w) =>
    for {
      n <- acc
      _ <- count(w)
      _ <- Writer.tell("saw " + w)
    } yield n + 1
  }
}

val (log, (counts, total)) = Eff.run(Writer.run(State.run(Map.empty[String, Int])(countAll("a b a"))))
assertEquals(total, 3)
assertEquals(counts, Map("a" -> 2, "b" -> 1))
assertEquals(log, Vector("saw a", "saw b", "saw a"))
```

How to read the last line from the inside out:

- `State.run(Map.empty)(...)` handles `State`. It leaves an
  `Eff[Writer[String], (Map[String, Int], Int)]`.
- `Writer.run` handles `Writer`. It leaves
  `Eff[Any, (Vector[String], (Map[String, Int], Int))]`.
- `Eff.run` accepts only `Eff[Any, _]`, meaning nothing left to handle.

scalac 2.13 infers what each handler leaves; no type arguments are
needed. **The order of the handlers is meaningful.** Here is the same
idea with an error in the middle:

```scala
final case class Config(limit: Int)

def withdraw(amount: Int): Eff[Reader[Config] with State[Int] with Throws[String], Int] = for {
  cfg <- Reader.ask[Config]
  balance <- State.get[Int]
  _ <- if (amount > cfg.limit) Throws.raise[String, Unit]("over the limit")
       else if (amount > balance) Throws.raise[String, Unit]("insufficient funds")
       else State.put(balance - amount)
  left <- State.get[Int]
} yield left

def attempt(amount: Int): (Int, Either[String, Int]) =
  Eff.run(State.run(100)(Throws.run(Reader.run(Config(limit = 50))(withdraw(amount)))))

assertEquals(attempt(30), (70, Right(70)))
assertEquals(attempt(80), (100, Left("over the limit")))
```

`Throws` is handled INSIDE `State`, so a failed withdrawal still
reports the balance. If you swap them, `Throws.run(State.run(100)(...))`,
the answer becomes `Either[String, (Int, Int)]`, and the state is lost
on failure (`TestScala2Guide`, "the handler order decides what a
failure keeps"). That is the standard reading of handler order in effect
systems, and okay's Scala 3 API behaves the same way.

## 4. Failure

There are two kinds of failure, and they differ in one way:

- **`Throws[E]` is a typed failure.** It shows in the row, and code
  cannot forget to handle it: a program with `Throws` left in its row
  does not compile when you run it (see section 10).
- **Exceptions.** `Prog.delay` and `Async.attempt` turn a thrown
  exception into a failure value (`Throws[Throwable]`). `Async.delay`
  does not: an exception thrown inside it propagates out of
  `Eff.runAsync`. The same goes for an exception thrown from a function
  you pass to `map` or `flatMap`. Wrap code that may throw in
  `attempt`/`delay` at the point where you want the failure to become a
  value. All four behaviours are pinned in `TestScala2Guide` ("§4").

## 5. Your own effect

In Scala 3, okay declares an effect with `derives Effect`. In Scala 2
you write an ordinary sealed trait whose cases extend `Op`, and an
object that extends `Effect`:

```scala
sealed trait KV[A] extends Op[A]
final case class Get(key: String) extends KV[Option[String]]
final case class Put(key: String, value: String) extends KV[Unit]
object KV extends Effect[KV]
```

The type parameter of each case is what the operation answers: `Get`
answers `Option[String]`, and `Put` answers `Unit`. `KV.send(op)`
performs an operation, and the capability in the row is `Effect[KV]`.

A **handler** decides what the operations mean. It receives each
operation together with `k`, the rest of the program after it:

```scala
def inMemory[R, B](store: scala.collection.mutable.Map[String, String]): Handler[KV, R, B] =
  new Handler[KV, R, B] {
    def apply[X](op: KV[X], k: X => Eff[R, B]): Eff[R, B] = op match {
      case Get(key) => k(store.get(key))
      case Put(key, value) => store(key) = value; k(())
    }
  }

val program: Eff[Effect[KV], Option[String]] = for {
  _ <- KV.send(Put("lang", "scala"))
  v <- KV.send(Get("lang"))
} yield v.map(_.toUpperCase)

val store = scala.collection.mutable.Map.empty[String, String]
assertEquals(KV.run(program)(a => Eff.pure(a))(inMemory(store)), Some("SCALA"))
assertEquals(store.toMap, Map("lang" -> "scala"))
```

In `case Get(key) => k(store.get(key))`, the match tells scalac that
here `X` is `Option[String]`, so passing an `Option[String]` to `k`
typechecks.

The program says nothing about HOW the store works. The same program
under a different handler, one that records instead of storing, gives a
dry run:

```scala
val log = ListBuffer.empty[String]
def dryRun[R, B]: Handler[KV, R, B] = new Handler[KV, R, B] {
  def apply[X](op: KV[X], k: X => Eff[R, B]): Eff[R, B] = op match {
    case Get(key) => log += ("get " + key); k(None)
    case Put(key, value) => log += ("put " + key + "=" + value); k(())
  }
}
assertEquals(KV.run(program)(a => Eff.pure(a))(dryRun), None)
assertEquals(log.toList, List("put lang=scala", "get lang"))
```

What the handler does with `k` is its choice:

- **once**: an ordinary effect, as above.
- **not at all**: the rest of the program is abandoned (early exit,
  like an exception).
- **several times**: several answers. A handler for a `Flip`
  operation that calls `k(true)` and `k(false)` and concatenates the
  results enumerates every outcome
  (`okay-scala2/probe/src/test/scala/TestOwnEffectFromScala2.scala`).

**`handle` or `run`?** `KV.handle(prog)(ret)(h)` removes `Effect[KV]`
and leaves the rest of the row, so use it when other effects remain.
For the LAST effect, use `KV.run(prog)(ret)(h)`, which returns the
answer itself. With `handle` in that position, scalac 2.13 has
nothing left to infer the rest of the row from and picks `Any`, and
`-Xlint` reports that (section 10).

## 6. Continuations: `Cont`

`Cont[A, S, R]` is okay's delimited continuation type. `Cont.shift`
captures "the rest of the block" up to the nearest `Cont.reset` as a
function `k`, which may be called any number of times:

```scala
// the continuation k is "the rest of the block": here, _ * 2 then + 1
val twice: Int = Cont.reset(
  Cont.shift[Int, Int, Int](k => k(k(3))).map(_ * 2).map(_ + 1)
)
assertEquals(twice, 15)
```

`k(3)` is `3 * 2 + 1 = 7`, and `k(7)` is `15`. The three type
parameters are what `shift` receives, what the rest of the block
answers, and what the whole block answers; the last two may differ
(answer-type modification). Scala 2 cannot infer them from use, so
write them out. `Cont` is stack-safe: 100 000 binds, nested either
way, are tested from 2.13.

## 7. Streams: `Source`

```scala
val words: Source[String] = Source("the", "quick", "brown", "fox", "jumps")
val lengths: Eff[Async, Vector[Int]] = words.filter(_.length > 3).map(_.length).runCollect
assertEquals(Eff.runAsync(lengths), Vector(5, 5, 5))

val total: Eff[Async, Int] = words.zipWithIndex.take(3).runFold(0) { case (acc, (w, _)) => acc + w.length }
assertEquals(Eff.runAsync(total), 13)
```

- Constructors: `Source(...)`, `fromIterable`, `range`, `unfold`,
  `empty`.
- Transformations: `map`, `filter`, `mapConcat`, `take`, `takeWhile`,
  `drop`, `zipWithIndex`, `++`, and `merge`, which reads two sources
  at once.
- Consumers: `runCollect`, `runForeach`, `runFold`. Each returns an
  `Eff[Async, _]`, so nothing runs until `Eff.runAsync`.
- A source can also be written as a program:
  `Source.fromEff(e: Eff[Writer[A] with Async, Unit])`, where each
  `Writer.tell` emits an element. `toEff` converts back.
- `take` stops pulling once it has enough, so it works on an
  infinite `unfold`.

## 8. Fibers and channels

`Async.fork(e)` starts `e` on its own fiber, which on the JVM is a
virtual thread. A `Channel` is bounded: `send` waits while it is full
and `receive` waits while it is empty. Both are programs, so waiting
never blocks a thread you did not give it. A worker pool, then:

```scala
val jobs = Channel[Int](8)
val results = Channel[Int](8)

def worker: Eff[Async, Unit] = jobs.receive.flatMap {
  case Some(n) => results.send(n * n).flatMap(_ => worker)
  case None => Eff.pure(())
}

def feed(ns: List[Int]): Eff[Async, Unit] = ns match {
  case n :: rest => jobs.send(n).flatMap(_ => feed(rest))
  case Nil => Async.delay(jobs.close())
}

val program: Eff[Async, Int] = for {
  w1 <- Async.fork(worker)
  w2 <- Async.fork(worker)
  _ <- Async.fork(feed((1 to 10).toList))
  _ <- Async.fork(w1.join.flatMap(_ => w2.join).flatMap(_ => Async.delay(results.close())))
  sum <- results.source.runFold(0)(_ + _)
} yield sum

assertEquals(Eff.runAsync(program), 385)
```

`results.source` reads the channel as a `Source` that ends when the
channel is closed. Also available:

- `Async.par(a, b)` runs two programs at once and answers both results.
- `Async.race(a, b)` answers the first result and cancels the other.
- `Async.timeout(ms)(e)` answers `None` past the deadline.
- `Async.sleep(ms)`.
- On a fiber: `join`, `joinEither`, `cancel`.
- On a channel: `offer` (does not wait), `close`, `isClosed`.

## 8a. Codecs: JSON, CBOR, JSON Schema

Module `okay-scala2-codec`. Most of okay-codec works from Scala 2.13
AS IT IS: `okay.codec.Schema` (the type, its cases, `wrap`, `refine`,
`enumeration`, and the instances for `Int`, `String`, `Option`, `List`
and the rest, which Scala 2's implicit search finds), `okay.codec.Cbor`,
`Yaml` and `Validate`. Two things do not, and the module replaces them:

- **`derives Schema`** is a Scala 3 macro. `Schemas.product1` …
  `product16` build a product's schema from the field names, the
  companion's `apply` and a projection back to a tuple, the way circe's
  `forProductN` does. `Schemas.sum`/`variant` build a sealed
  hierarchy's. `Schemas.constant` builds a case object's.
- **`okay.codec.Json`** cannot be read by scalac 2.13 at all (its
  TASTy crashes the reader). `okay.scala2.Json` writes and reads JSON
  as text, and `okay.scala2.JsonSchema.of` renders a schema's JSON
  Schema as text.

The model below is copied from
`okay-scala2/probe/src/test/scala/TestCodecFromScala2.scala`:

```scala
final case class Person(name: String, age: Int, email: Option[String], tags: List[String])
object Person {
  implicit val schema: Schema[Person] =
    Schemas.product4("Person", "name", "age", "email", "tags")(Person.apply)(p => (p.name, p.age, p.email, p.tags))
}

sealed trait Shape
final case class Circle(r: Double) extends Shape
final case class Rect(w: Double, h: Double) extends Shape
case object Empty extends Shape
object Shape {
  implicit val circle: Schema[Circle] = Schemas.product1("Circle", "r")(Circle.apply)(_.r)
  implicit val rect: Schema[Rect] = Schemas.product2("Rect", "w", "h")(Rect.apply)(r => (r.w, r.h))
  implicit val empty: Schema[Empty.type] = Schemas.constant("Empty", Empty)
  implicit val schema: Schema[Shape] = Schemas.sum[Shape]("Shape")(
    Schemas.variant[Shape, Circle]("Circle"),
    Schemas.variant[Shape, Rect]("Rect"),
    Schemas.variant[Shape, Empty.type]("Empty"))
}

final case class Tree(label: String, kids: List[Tree])
object Tree {
  implicit lazy val schema: Schema[Tree] = Schemas.product2("Tree", "label", "kids")(Tree.apply)(t => (t.label, t.kids))
}
```

and the use, from the same file:

```scala
val text = Json.write(ada)
assertEquals(text, """{"name":"ada","age":36,"email":"ada@example.org","tags":["math","engines"]}""")
assertEquals(Json.read[Person](text), Right(ada))
assertEquals(Cbor.read[Person](Cbor.write(ada)), Right(ada))
```

- The imports are `okay.codec.{Cbor, Schema}` and
  `okay.scala2.{Json, JsonSchema, Schemas}`. A Scala 3 top-level alias
  is invisible to Scala 2, so `Schema` keeps its okay-codec name.
- A recursive type's schema is an `implicit lazy val`. The field
  schemas are by-name implicits, so it can refer to itself.
- The wire format is exactly okay-codec's, so a Scala 2 service and a
  Scala 3 service read each other's JSON and CBOR. Products are
  objects keyed by field name, sums are one-entry objects keyed by
  case name, and `None` is an absent field.
- Decode errors are `Left`s, in okay-codec's own words, for example
  `expected SInt, got JStr(old)`. They name the expected schema and the
  value found, but not the field's path.

## 8b. HTTP: routes, a server, a client

Module `okay-scala2-http`. okay-http's `Request`, `Method` and `Body`
are readable from Scala 2, so a request is okay-http's own
(`okay.http.Request.get(url)`, `Request(Method.Post, url, headers,
Body.Text(...))`). What is not readable, and what this module provides
in its place:

- **`Response`.** okay-http's response has a streamed body whose type
  names the union row, so it cannot be read. `okay.scala2.Response` has
  `text`, `html`, `bytes`, `json`, `status` and `lines` (a streamed
  body), and reads back with `.status`, `.header`, `.text`, `.bytes`.
- **Routes.** okay-http's `Route` uses Scala 3 generic tuples. In
  Scala 2, routing is pattern matching: `Routes { case ... }` over
  extractors `GET`/`POST`/`PUT`/`PATCH`/`DELETE` and `Path`, with
  `Requests.query`/`queryAll`/`json`/`text` for the rest. Path segments
  and query values are percent-decoded by okay-http's own reader,
  which is now public as `okay.http.Urls`.

The routes below are copied from
`okay-scala2/probe/src/test/scala/TestHttpFromScala2.scala`:

```scala
final case class User(id: Int, name: String)
object User {
  implicit val schema: Schema[User] = Schemas.product2("User", "id", "name")(User.apply)(u => (u.id, u.name))
}

val users = scala.collection.concurrent.TrieMap(1 -> User(1, "ada"))

val routes: Request => Eff[Async, Response] = Routes {
  case GET(Path("users", id)) =>
    Async.delay(users.get(id.toInt) match {
      case Some(u) => Response.json(u)
      case None => Response.text("no user " + id, 404)
    })
  case r @ POST(Path("users")) =>
    Requests.json[User](r) match {
      case Right(u) => Async.delay { users.put(u.id, u); Response.json(u, 201) }
      case Left(e) => Eff.pure(Response.text(e, 400))
    }
  case r @ GET(Path("search")) =>
    Eff.pure(Response.text("q=" + Requests.query(r, "q").getOrElse("") + " tags=" + Requests.queryAll(r, "tag").mkString(",")))
}
```

A handler is an ordinary function `Request => Eff[Async, Response]`, so
it can be tested without a socket: `Eff.runAsync(routes(Request.get("/users/1")))`.
A request no case matches gets a 404.

Serving, and calling it back, from the same file's live suite:

```scala
val client = Client()
val got = Eff.runAsync(Server.use(0)(routes) { port =>
  client.get("http://127.0.0.1:" + port + "/users/1").map(r => (r.status, r.text))
})
assertEquals(got, (200, """{"id":1,"name":"ada"}"""))
```

- `Server.use(port)(handler)(body)` serves while `body` runs, and stops
  afterwards, however the body ends. Port `0` means any free port,
  and `body` receives the port actually bound. Underneath is okay-http's
  `Server.serve` under okay's `Resource`.
- `Server.start(port)(handler)` returns a `RunningServer` right away,
  for a service that serves for the whole life of the process.
  `server.close()` stops it and waits until it has stopped. If the
  port cannot be bound, `start` throws.
- `Client()` has `send`, `get`, `post`, `postJson`, each an
  `Eff[Async, Response]` with the body read in full, and `lines`,
  which is a `Source[String]` streamed line by line.
- The socket tests are tagged `Live`, like every suite in this
  repository that binds a port, so the default gate runs only the
  socket-free half. `sbt integrationTest` runs both.

## 8c. SQL: queries and transactions

Module `okay-scala2-sql`. okay-sql's data types are readable from
Scala 2 and used as they are: `okay.sql.SqlValue` (parameters), `Bad`
(a row that did not decode), `Drift` (a column that no longer matches),
`Isolation`, and the `Sql` driver trait, together with okay-jdbc's
`JdbcSql`. What Scala 2 cannot use is every operation, because each one
answers a program. `okay.scala2.Db` provides those operations as `Eff`
and `Source`.

The example below is copied from
`okay-scala2/probe/src/test/scala/TestSqlFromScala2.scala`, where it
runs against an in-memory H2:

```scala
final case class Person(id: Long, fullName: String, age: Int)
object Person {
  implicit val schema: Schema[Person] =
    Schemas.product3("Person", "id", "fullName", "age")(Person.apply)(p => (p.id, p.fullName, p.age))
}
```

```scala
val prog = for {
  a <- db.update("INSERT INTO person VALUES (?, ?, ?)", SqlValue.I64(1L), SqlValue.Text("Ada Lovelace"), SqlValue.I32(36))
  b <- db.updateOf("INSERT INTO person (id, full_name, age) VALUES (?, ?, ?)", Person(2, "Charles Babbage", 79))
  people <- db.all[Person]("SELECT * FROM person ORDER BY id")
} yield (a + b, people)
assertEquals(Eff.runAsync(Throws.run(prog)), Right((2L, Vector(Person(1, "Ada Lovelace", 36), Person(2, "Charles Babbage", 79)))))
```

- `Db.jdbc(connection)` goes over a JDBC connection, which the caller
  owns and closes. `Db(sql)` goes over any okay-sql driver.
- **Rows are decoded by column LABEL.** A field `fullName` reads the
  column `full_name`, so a `SELECT *` that reorders columns cannot
  shear the mapping.
- **Parameters are always bound**, positionally, through the driver's
  prepared path: as `SqlValue`s, or from a case class with
  `updateOf`/`rowsOf`/`allOf`.
- **A row that does not decode is data, not a throw.** In `rows` (a
  stream) it is a `Left(Bad)`. In `all` the first one fails the
  program as a typed `Throws[Bad]`, so `all` answers
  `Eff[Async with Throws[Bad], Vector[A]]`, and `Throws.run` turns
  that into an `Either`.
- **`db.transaction()(tx => ...)`** commits when the body completes and
  rolls back when it fails. Underneath is okay-sql's `Typed.transact`
  under `Resource`, the same region as in Scala 3.
- **`db.verify[A](query)` catches drift at startup.** It returns one
  `Drift` for each column that no longer matches: a column that is
  missing, of another type, or nullable where the field is not an
  `Option`.
- H2 reports an unquoted column name in upper case, so a `Bad` or a
  `Drift` from it says `FULL_NAME`.
- In UNFORKED sbt tests across several modules, open the connection
  through the driver itself (`new org.h2.Driver().connect(url, props)`)
  rather than `DriverManager`. `DriverManager` scans for drivers once
  per JVM and then serves only the ones visible to the caller's class
  loader. So when another module's suite has registered the same driver
  first, it answers "No suitable driver found" even though the suite
  passes when run alone. The probe hit exactly this in the full matrix.

## 8d. Agents: a model, tools, a conversation

Module `okay-scala2-agent`. okay-agent's data is readable from Scala 2
(`okay.agent.Turn`, `Reply`), as long as nothing touches a JSON
field, because `okay.codec.Json` is not readable. The agent program
and the assembly of its handlers are not usable. So the module
provides four things:

- `Model` — scripted (for tests), or a real provider:
  `Model.anthropic(apiKey, model)` or `Model.openAi(apiKey, model, url)`.
- `Tools` — declared and implemented in one place, with the arguments
  decoded by the same `Schema` that declares them to the model.
- `Policy` — how the conversation is kept within the context:
  `Policy.all`, or `Policy.window(budget)`.
- `Chat` — okay-agent's loop (ask the model, run the tools it calls,
  repeat until it answers), with a conversation that carries over from
  one `say` to the next.

The code below is copied from
`okay-scala2/probe/src/test/scala/TestAgentFromScala2.scala`:

```scala
final case class SearchArgs(query: String, limit: Option[Int])
object SearchArgs {
  implicit val schema: Schema[SearchArgs] =
    Schemas.product2("SearchArgs", "query", "limit")(SearchArgs.apply)(a => (a.query, a.limit))
}

val searched = scala.collection.mutable.ListBuffer.empty[SearchArgs]

val tools: Tools = Tools.empty.on[SearchArgs]("search", "look something up") { a =>
  searched += a
  s"${a.limit.getOrElse(10)} hits for '${a.query}'"
}
```

```scala
val model = Model.scriptedCalls(
  ("let me look", Seq("search" -> """{"query":"okay","limit":3}""")),
  ("found 3 hits", Seq.empty))
val chat = Chat(model, tools, Policy.all)
assertEquals(Eff.runAsync(chat.say("find okay")), "found 3 hits")
assertEquals(searched.toList, List(SearchArgs("okay", Some(3))))
assertEquals(results(chat), Seq("3 hits for 'okay'"))
```

Here `results(chat)` collects the `Turn.Result` texts from
`chat.transcript`.

- **Tool approval.** `Chat(..., approve = call => ...)` decides every
  tool call before it runs. It sees a `Call(id, name, argsJson)`, with
  the arguments as JSON text. A denied call is answered "denied", and
  the model sees that answer.
- **The window policy is visible to the model.** `Policy.window(budget)`
  pins system turns, evicts the oldest turns past the budget (about
  four characters per token), and tells the model what it dropped
  (a `Turn.Summary`).
- **Handlers are the test doubles.** A scripted `Model` is not a mock
  of anything: it is another handler for the same effect, which is
  okay-agent's design.
- **Real providers were not called while this was written.** No API
  key was available, so the one test against a real model (Live) was
  skipped. What `Model.anthropic` and `Model.openAi` do is okay-agent's
  own `Provider`, the same one the Scala 3 API uses.

## 8e. UI: the view as a value, the loop as a fold

Module `okay-scala2-ui`. okay-ui's tree `Ui`, its `Event`s and `Frame`
(the pure text renderer) are readable from Scala 2, so a view is built
with okay-ui's own constructors and events are matched as they are.
What Scala 2 cannot use is the loop, `Ui.run`, and a `Host`: both
answer programs. `UiApp` provides the loop as an `Eff`, and `UiHost`
the hosts.

The counter below is copied from
`okay-scala2/probe/src/test/scala/TestUiFromScala2.scala`:

```scala
// a counter: two buttons, a label
def view(n: Int): Ui = Ui.Column(Vector(
  Ui.Text("count: " + n),
  Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc")))))

def update(n: Int, e: Event): Int = e match {
  case Event.Pressed("inc") => n + 1
  case Event.Pressed("dec") => n - 1
  case _ => n
}
```

```scala
val host = ScriptedHost(Event.Pressed("inc"), Event.Pressed("inc"), Event.Pressed("dec"), Event.Pressed("nope"))
assertEquals(Eff.runAsync(UiApp.run(0)(view)(update)(host.host)), 1)
assertEquals(host.frames, Vector(view(0), view(1), view(2), view(1)))
```

- `UiApp.run(init)(view)(update)(host)` is okay-ui's own `Ui.run`. It
  renders the view, folds each event through `update`, renders again
  only when the view changed (the no-op `"nope"` drew nothing), stops
  at `Event.Closed`, and returns the final state.
- **Hosts.** `UiHost.terminal()` draws in this process's terminal.
  `UiHost.swing(container)` draws in a Swing container, and
  `UiApp.window(title)(...)` opens a window of its own. For tests,
  `ScriptedHost(events*)` delivers the given events and then `Closed`,
  and keeps every frame. `okay.ui.Frame.render(ui)` turns a frame into
  plain text lines.
- `UiApp.runWith(...)(host, external)` merges a `Source[Event]` from
  the world (a timer, a socket) in beside the user's events.
- **An enum case is typed as the CASE in Scala 2.** Scala 3 widens
  `Event.Pressed("inc")` to `Event`, and Scala 2 does not. So an
  invariant container needs the type written out: `Source[Event](...)`.
- The object is `UiApp`, not `App`, so that `import okay.scala2._`
  cannot capture a Scala 2 `object Main extends App`.
- The Swing and terminal hosts are okay-ui's own. They are not
  exercised by the 2.13 probe, which has no display and no tty.

## 8f. Forms from a Schema

okay-ui's `Form` renders a form from the same `Schema` that decodes
it, and folds the user's edits into the value. All of its functions
speak `okay.codec.Json`, which Scala 2 cannot read. So
`okay.scala2.FormState[A]` (module `okay-scala2-ui`) holds that value
itself and speaks only in `A`, `Ui` and `Event`. The code below is
copied from `okay-scala2/probe/src/test/scala/TestFormFromScala2.scala`:

```scala
final case class Signup(name: String, age: Int, newsletter: Boolean)
object Signup {
  implicit val schema: Schema[Signup] =
    Schemas.product3("Signup", "name", "age", "newsletter")(Signup.apply)(s => (s.name, s.age, s.newsletter))
}
```

```scala
val filled = FormState.blank[Signup]
  .edit(Event.Edited("name", "Ada"))
  .edit(Event.Edited("age", "36"))
  .edit(Event.Toggled("newsletter", true))
assertEquals(filled.errors, Vector.empty)
assertEquals(filled.decoded, Right(Signup("Ada", 36, newsletter = true)))
```

and as the state of a `UiApp` loop, from the same file:

```scala
val host = ScriptedHost(Event.Edited("name", "Cy"), Event.Edited("age", "5"))
val done = Eff.runAsync(UiApp.run(FormState.blank[Signup])(_.view)(_.edit(_))(host.host))
assertEquals(done.decoded, Right(Signup("Cy", 5, newsletter = false)))
```

- A field's key is its path: `name`, and for nested records
  `where.city`. Edits arrive as okay-ui's own `Event.Edited`,
  `Toggled`, `Chosen`, or a whole `Submitted` form.
- `errors` lists the fields that do not validate yet, as (path,
  message). `view` shows each error beside its field. `decoded` is
  `Right` once the value is complete.
- `FormState.of(a)` starts filled from a value. `withLabels` gives
  fields human names.

## 8g. WebSockets

Module `okay-scala2-ws`. okay-http's `Frame` is readable from Scala 2,
and matching on it works. A socket's operations and a server session
(a `Stage[Frame, Frame, Unit]`, which is a program) are not usable
directly. So the client here is a set of `Eff` operations and a
`Source`, and a server session is written as a fold, the way Scala 2
writes a state machine. The session below is copied from
`okay-scala2/probe/src/test/scala/TestWsFromScala2.scala`:

```scala
// each text frame answered with how many have arrived so far
val counting: WsSession = WsSession.fold(0) {
  case (n, Frame.Text(t)) => (n + 1, Seq(Frame.Text(s"${n + 1}: $t")))
  case (n, _) => (n, Seq.empty)
}
```

and served, then talked to, from the same file's live suite:

```scala
val routes = Routes { case GET(Path("health")) => Eff.pure(Response.text("ok")) }
val got = Eff.runAsync(WsServer.use(0)(routes)({ case _ => counting }) { port =>
  for {
    ws <- WebSocket.connect("ws://127.0.0.1:" + port + "/count")
    _ <- ws.sendText("x")
    _ <- ws.sendText("y")
    replies <- ws.texts.take(2).runCollect
    _ <- ws.close()
    health <- Client().get("http://127.0.0.1:" + port + "/health")
  } yield (replies, health.text)
})
assertEquals(got, (Vector("1: x", "2: y"), "ok"))
```

- `WsSession.fold(init)(step)` answers each frame the client sends with
  the next state and the frames to send back. Underneath is
  okay-stream's `Stage.transduce`, the same shape a Scala 3 session
  has. `WsSession.echo` sends text frames straight back.
- `WsSession.replay(session, frames)` runs a session with no socket and
  returns what it would send. A session is a pure program, so this is
  exactly what a client would receive, which makes it the way to test
  one.
- `WsServer.use(port)(routes)(sessions)(body)` serves ordinary requests
  (the `Routes` of section 8b) and WebSocket upgrades on the same port.
  Underneath is okay-jetty, and a request no session matches is refused
  the upgrade.
- `WebSocket.connect(url)` opens a client over okay-http's JDK transport:
  `send`, `sendText`, `frames`, `texts`, `close`.
- A binary frame carries okay's `Chunk`, a Scala 3 alias that Scala 2
  cannot see. The type it names, `ArraySeq[Byte]`, Scala 2 can see:
  `Frame.Ping(ArraySeq[Byte](1, 2))` works. `WebSocket.binary(bytes)` and
  `WebSocket.bytes(frame)` convert to and from `Array[Byte]`.
- The live suite binds a port, so it is tagged `Live`. It was run and
  passed while this was written.

## 8h. Nondeterminism and search

`Choose` is one more capability of `Eff`: a program that performs
`Choose.from(...)` has several answers, and a handler decides what they
mean. It is okay's own `Choose` and `Logic` underneath. The code below is
copied from `okay-scala2/probe/src/test/scala/TestChooseFromScala2.scala`:

```scala
val triples = for {
  a <- Choose.from(1 to 13: _*)
  b <- Choose.from(a to 13: _*)
  c <- Choose.from(b to 13: _*)
  _ <- Choose.guard(a * a + b * b == c * c)
} yield (a, b, c)
assertEquals(Eff.run(Choose.all(triples)), Seq((3, 4, 5), (5, 12, 13), (6, 8, 10)))
```

```scala
// the naturals from n, as an infinite search
def nats(n: Int): Eff[Choose, Int] = Choose.from(true, false).flatMap(stop => if (stop) Eff.pure(n) else nats(n + 1))
```

```scala
val fair = Choose.interleave(nats(0), Choose.from(100, 200))
val got = Eff.run(Choose.first(6)(fair))
assert(got.contains(100) && got.contains(200), got.toString)
```

- **Handlers:** `Choose.all` (every answer), `Choose.first(n)` (lazily,
  so the search may be infinite), `Choose.cut` (commit to the first
  answer), `Choose.ifte` (the soft cut: the else branch runs only when
  the condition has no answer).
- **Fair search:** `Choose.interleave` and `Choose.fairBind`. A plain
  `flatMap` over an infinite branch never reaches the second branch;
  these take turns.
- **The handler order is the design, as everywhere in okay.** With
  `State` handled INSIDE the search, each branch has its own state. With
  it handled OUTSIDE, all branches share one:
  `Choose.all(State.run(0)(prog))` is `Seq((1, 1), (2, 2))`, while
  `State.run(0)(Choose.all(prog))` is `(3, Seq(1, 3))`.
- **Search over samples:** `Search.bestOf(n)(gen)(ok)` runs `gen` up to
  `n` times and stops at the first result that passes, which is how "ask
  the model until the JSON parses" is written: `gen` can be `chat.say(...)`.
  `Search.all` and `Search.majority` (self-consistency) are there too.

## 8i. Scenarios and screens: Dialog and Nav

`Dialog` (module `okay-scala2-ui`) is okay-ui's scenario effect, as a
capability of `Eff`. `Dialog.show(ui)` draws a screen and answers the
next event, so a whole scenario is one program. The code below is
copied from `okay-scala2/probe/src/test/scala/TestDialogNavFromScala2.scala`:

```scala
// a scenario as one program: two questions, then an answer
val greet: Eff[Dialog, String] = for {
  first <- Dialog.show(Ui.Column(Vector(Ui.Text("hello?"), Ui.Button("yes", "yes"), Ui.Button("no", "no"))))
  answer <- first match {
    case Event.Pressed("yes") => Dialog.show(Ui.Input("", "name", "your name")).map {
      case Event.Edited(_, name) => "hi " + name
      case _ => "hi"
    }
    case _ => Eff.pure("bye")
  }
} yield answer
```

```scala
val (drawn, answer) = Dialog.replay(greet, Seq(Event.Pressed("yes"), Event.Edited("name", "ada")))
assertEquals(answer, Some("hi ada"))
```

- `Dialog.run(host)(prog)` runs a scenario on a host (`None` if the host
  closes first). `Dialog.replay(prog, events)` runs it with no host at
  all and returns every screen it drew, which is how a scenario is
  tested.
- `Dialog.ask[A](message)` is a form drawn from `A`'s `Schema` that keeps
  asking until the value decodes. `$ok` submits and `$cancel` answers
  `None`.
- **Screens as a stack need almost no facade.** `okay.ui.Screen` is a
  plain trait, and Scala 2 implements it directly. `Nav`'s cases
  (`Push`, `Pop`, `Stay`, `To`, `PopTo`) and `Nav.state`, `Nav.update` and
  `Nav.view` are readable and pure, so a stack runs in the ordinary
  loop: `UiApp.run(Nav.state(root))(Nav.view)(Nav.update)(host)`. The
  one unreadable helper, `Nav.screen` (its update answers the union
  `Nav | S`), is `Screens.of(init)(view)(update)`, with an `Either`.

## 8j. Resilience: breaker, bulkhead, limiter, hedge, deadline, retry

Module `okay-scala2-resilience`. okay-resilience's pieces are built with
their own constructors from Scala 2 (`new Breaker(name, failures,
openMillis)`, `new Bulkhead(name, permits, queue)`, `new Limiter(name,
ratePerSecond, burst)`, `Deadline.in(millis)`), and a refusal is its own
`Refused.*`. `Guards` runs a program through a piece. The code below is
copied from `okay-scala2/probe/src/test/scala/TestResilienceFromScala2.scala`:

```scala
val b = new Breaker("pay", 2, 60000L)
val runs = new AtomicInteger
val failing = Async.delay[Int] { runs.incrementAndGet(); throw new IllegalStateException("down") }
assert(outcome(Guards.breaker(b)(failing)).isLeft)
assert(outcome(Guards.breaker(b)(failing)).isLeft)
outcome(Guards.breaker(b)(failing)) match {
  case Left(r: Refused.BreakerOpen) => assertEquals(r.name, "pay")
  case other => fail("expected BreakerOpen, got " + other)
}
assertEquals(runs.get, 2)
```

```scala
val calls = new AtomicInteger
val flaky = Async.delay { if (calls.incrementAndGet() < 3) throw new IllegalStateException("not yet") else "ok" }
assertEquals(Eff.runAsync(Guards.retry(okay.Retry.immediate(5))(flaky)), "ok")
```

(`outcome(e)` there is `scala.util.Try(Eff.runAsync(e)).toEither`.)

- The pieces compose by nesting, like the programs they wrap:
  `Guards.breaker(b)(Guards.bulkhead(h)(Guards.limiter(l, key)(call)))`.
- A refusal is a thrown `Refused`, carrying `retryAfterMillis` when the
  piece knows it. That is what a server maps to 429 or 503.
- `Guards.breaker(b)(prog, failing)` can count a returned value as a
  failure, for example a response with status 5xx.
- `Guards.hedge` overlaps attempts, so it is only for operations that
  are safe to repeat. `Guards.retry` does not overlap: it waits each
  delay of the policy (`okay.Retry.constant`, `exponential`,
  `jittered`, `immediate`).

## 8k. The durable log: okay-persist

Module `okay-scala2-persist`. okay-persist's engine is synchronous and
plain, so a Scala 2 caller uses it directly: `new MemoryStore`,
`FileStore.open(dir)`, `topic.append(key, value, Ack.Durable)`,
`topic.read(partition, from, max)`, `Offsets`, `Snapshots`. `Persist`
supplies the three things that do not carry over. The code below is
copied from `okay-scala2/probe/src/test/scala/TestPersistFromScala2.scala`:

```scala
val store = new MemoryStore
val t = Persist.topic(store, "events")
assertEquals(t.append("k".getBytes, "a".getBytes, Ack.Durable), 0L)
assertEquals(t.append("k".getBytes, "b".getBytes, Ack.Durable), 1L)
t.read(0, 0L, 10) match {
  case Topic.Read.Records(rs) => assertEquals(rs.map(r => new String(r.value)), Vector("a", "b"))
  case other => fail(other.toString)
}
```

```scala
val typed = Persist.typed[Deposit](Persist.topic(new MemoryStore, "deposits"))
typed.append("acct-1".getBytes, Deposit("acct-1", 100), Ack.Durable)
```

```scala
val prog = for {
  writer <- Async.fork(Async.sleep(30).flatMap(_ => Async.delay { t.append("k".getBytes, "1".getBytes, Ack.Durable); t.append("k".getBytes, "2".getBytes, Ack.Durable); () }))
  seen <- Persist.tail(t, 0, 0L, pollMillis = 5).map(r => new String(r.value)).take(3).runCollect
  _ <- writer.join
} yield seen
assertEquals(Eff.runAsync(prog), Vector("0", "1", "2"))
```

- `Persist.topic(store, name, partitions = 1)`: `Store.topic` is a
  trait method, and a trait's abstract-method DEFAULTS are invisible
  from Scala 2, so `store.topic("t")` would ask for the partitions and
  the policy. `Persist.topic` passes okay-persist's own defaults.
- `Persist.typed[A](topic, version = 1)` is Scala 3's `topic.of[A]`,
  an extension, which Scala 2 cannot see. It needs an implicit
  `Schema[A]` (section 8a), and `read` answers `Typed.Decoded.Ok` or a
  decode failure per record, never a thrown exception.
- `Persist.stream(topic, partition, from)` reads to the end and
  finishes. `Persist.tail(...)` keeps polling for new records until the
  consumer stops, for example with `take`. Both are a `Source[Record]`
  (section 7): okay-persist's own streams yield chunks, flattened here.

## 8l. Transactions: okay-stm

Module `okay-scala2-stm`. The cell is okay's own `TRef`: `Stm.ref(init)`
(the same as `TRef(init)`), `ref.get`, `ref.modify(f)`. `Tx` is the
transaction language as a capability of `Eff`, and `Stm.atomically`
runs a transaction as one atomic step of an `Eff[Async, A]`. The code
below is copied from `okay-scala2/probe/src/test/scala/TestStmFromScala2.scala`:

```scala
def transfer(from: TRef[Int], to: TRef[Int], amount: Int): Eff[Tx, Unit] = for {
  balance <- Tx.read(from)
  _ <- Tx.check(balance >= amount)
  _ <- Tx.write(from, balance - amount)
  _ <- Tx.update(to)(_ + amount)
} yield ()
```

```scala
val a = Stm.ref(100)
val b = Stm.ref(0)
Eff.runAsync(Stm.atomically(transfer(a, b, 30)))
assertEquals((a.get, b.get), (70, 30))
```

`Tx.check` is `Tx.retry` unless the condition holds, and a retry does
not spin: the transaction parks until something it READ changes, then
runs again. Here a transfer waits for a deposit made by another fiber:

```scala
val prog = for {
  waiting <- Async.fork(Stm.atomically(transfer(account, out, 50)))
  _ <- Async.sleep(20)
  _ <- Stm.atomically(Tx.write(account, 80))
  _ <- waiting.join
} yield (account.get, out.get)
assertEquals(Eff.runAsync(prog), (30, 50))
```

- `Tx.orElse(a, b)` runs `a`, and if `a` retries, runs `b` instead.
  `a`'s writes are discarded as if it never ran. If `b` retries too,
  the whole transaction waits on what either branch read.
- A transaction's row is `Tx` alone, so an `Async` inside it is a type
  error: `Stm.atomically(Async.delay(println(1)))` does not compile. A
  conflict re-runs the transaction, and I/O must not run twice.
- On the JVM `atomically` is okay-stm's TL2 strategy: a version per
  cell, validation on every read, a commit that never blocks.

This is the design of Harris, Marlow, Peyton Jones and Herlihy,
"Composable memory transactions" (PPoPP 2005,
[doi:10.1145/1065944.1065952](https://doi.org/10.1145/1065944.1065952)),
which introduced `retry` and `orElse`. The commit is Dice, Shalev and
Shavit's TL2, "Transactional Locking II" (DISC 2006,
[doi:10.1007/11864219_14](https://doi.org/10.1007/11864219_14)).

## 8m. Stores: cache, blob, documents

Module `okay-scala2-stores`. Each store is built from Scala 2 with its
own constructors, and its plain values (`Regime`, `Etag`, `Meta`,
`Cond`, `PutResult`, the `Stats`) are used directly. The operations
are programs, and three objects provide them: `Caches`, `Blobs`,
`Documents`. The code below is copied from
`okay-scala2/probe/src/test/scala/TestStoresFromScala2.scala`.

A cache (okay-cache). `getOrLoad` is the read to use: on a miss ONE load
per key runs, and a caller that asks meanwhile waits for it rather than
loading again:

```scala
val cache = Cache.memory[String, Int](Regime.Invalidated, 100)
val loads = new AtomicInteger
val slowLoad = (k: String) => Async.sleep(20).map { _ => loads.incrementAndGet(); k.length }
val prog = for {
  a <- Async.fork(Caches.getOrLoad(cache, "hello")(slowLoad))
  b <- Async.fork(Caches.getOrLoad(cache, "hello")(slowLoad))
  x <- a.join
  y <- b.join
  cached <- Caches.get(cache, "hello")
} yield (x, y, cached)
assertEquals(Eff.runAsync(prog), (5, 5, Some(5)))
assertEquals(loads.get, 1)
```

- `Caches.writeThrough(cache, k)(commit)` runs the write, THEN
  invalidates `k`. The other order leaves a window in which a reader
  loads the old value back into the cache.
- Across nodes, a writer publishes the key with
  `Invalidations.append(topic, key)` and every reader runs
  `Caches.drain(topic, cache, keyOf, from)`, which answers the next
  offset to drain from.

A blob store (okay-blob): `Fs(root)` on a disk, `S3.wired(...)` for S3
and anything that speaks its API. An absent key is a `Left` naming it,
never an exception:

```scala
val blob = Fs(Files.createTempDirectory("okay-s2-blob"))
val prog = for {
  _ <- Blobs.putBytes(blob, "reports/a.txt", "alpha".getBytes)
  _ <- Blobs.put(blob, "reports/b.txt", Source(ArraySeq.unsafeWrapArray("be".getBytes), ArraySeq.unsafeWrapArray("ta".getBytes)))
  _ <- Blobs.putBytes(blob, "other/c.txt", "gamma".getBytes)
  a <- Blobs.getBytes(blob, "reports/a.txt")
  b <- Blobs.getBytes(blob, "reports/b.txt")
  missing <- Blobs.getBytes(blob, "reports/none.txt")
  keys <- Blobs.list(blob, "reports/").map(_.key).runCollect
} yield (a.map(new String(_)), b.map(new String(_)), missing.isLeft, keys)
```

- `Blobs.put` takes the bytes as a `Source` of chunks and
  `Blobs.stream` gives them back the same way, so an object larger
  than memory never has to be in memory. A chunk is
  `ArraySeq[Byte]` (okay's `Chunk`, whose alias Scala 2 cannot see).
- `Blobs.backup(root, blob)` copies okay-persist's closed segments to
  the blob, and `Blobs.restore(blob, root)` brings them back.

Documents (okay-docs): `Documents.onTopic[A](topic, indexes)` keeps them
on an okay-persist topic, given a `Schema[A]` (section 8a). Every write
takes a condition, and a refused write says what is there now:

```scala
val people = Documents.onTopic[Person](Persist.topic(new MemoryStore, "people"), Map("city" -> ((p: Person) => p.city)))
val prog = for {
  first <- Documents.put(people, "ada", Person("Ada", "London"), Cond.IfAbsent)
  again <- Documents.put(people, "ada", Person("Ada", "Paris"), Cond.IfAbsent)
  _ <- Documents.put(people, "alan", Person("Alan", "London"))
  found <- Documents.get(people, "ada")
  londoners <- Documents.query(people, "city", "London").map(_._1).runCollect
} yield (first, again, found.map(_.value), londoners.sorted)
```

- `Cond.Always`, `Cond.IfAbsent`, `Cond.IfVersion(v)`: compare-and-set
  on the version `get` returned. The answer is `PutResult.Applied(version)`
  or `PutResult.Stale(current)`.
- `onTopic` is a factory rather than `new TopicDocs(...)`, because
  `new` from Scala 2 makes the TASTy reader read the whole class
  (section 10).

## 8n. Models, retrieval, MCP: okay-llm, okay-rag, okay-mcp

Three modules, one per library: `okay-scala2-llm`, `okay-scala2-rag`,
`okay-scala2-mcp`. The code below is copied from
`okay-scala2/probe/src/test/scala/TestLlmFromScala2.scala`,
`TestRagFromScala2.scala` and `TestMcpFromScala2.scala`.

**A completion as a token stream.** Section 8d holds a model for a
conversation. `Llm` is the layer under it: one completion, token by
token, as a `Source[String]`. The transport is `Llm.http` in
production. In a test it is a function answering the response's lines,
which is also how a Scala 2 program plugs in an HTTP client it already
has:

```scala
val server = Llm.transport((_, _, _) => Source(openAiLine("4"), "", openAiLine("2"), "", "data: [DONE]", ""))
val answer = Llm.openAi(server, "key", "gpt-test", Seq("user" -> "6 * 7?")).runFold("")(_ + _)
assertEquals(Eff.runAsync(answer), "42")
```

`Llm.anthropic(transport, apiKey, model, messages)` is the same for
Anthropic's Messages API. `Llm.first[A](tokens)` reads the stream only
until the text so far decodes as an `A` (by its `Schema`, section 8a),
and stops there. The rest of the completion is never read, and a
provider that bills by generated tokens is told to stop:

```scala
val point = Llm.first[Point](Llm.openAi(server, "key", "gpt-test", Seq("user" -> "a point")))
assertEquals(Eff.runAsync(point), Some(Point(1, 2)))
assert(pulled.get < 10, s"pulled ${pulled.get} tokens from an endless stream")
```

**Retrieval.** okay-rag's splitting and keyword search are plain
functions, used directly: `Ingest.segment(doc, budget)(size)`,
`Keyword.index(segments)`, `Keyword.search(index, query, k)`. The vector
side is a `VectorIndex`, which holds the store and the embedding model.
The model is a plain function, a batch of texts in and one vector per
text out, so any embedding API fits:

```scala
val index = Rag.memory(Rag.hashing())
val progress = index.add(docs)
assertEquals((progress.sources, progress.embedded, index.size), (3, progress.segments, progress.segments))
val hits = index.search("multiply numbers", 3)
assertEquals(hits.head.segment.source, "Math.scala")
```

- `Rag.hashing()` is a deterministic stand-in (hashed character
  trigrams), for tests and offline pipelines. It is not semantic.
- `index.hybrid(keywords, query, k)` fuses the vector hits with keyword
  hits by reciprocal rank (Cormack, Clarke and Büttcher, SIGIR 2009,
  [doi:10.1145/1571941.1572114](https://doi.org/10.1145/1571941.1572114)).
  Use it when queries name exact identifiers as often as ideas.

**MCP.** A server's tools are okay-scala2-agent's `Tools` (section 8d),
so one declaration serves a local agent and an MCP server. Arguments
and schemas cross as JSON text. `McpLink.pair()` connects a server and
a client in one process; `McpLink.of(in, out)` and `McpClient.spawn(command,
...)` connect over a process's stdio:

```scala
val tools = Tools.empty.on[Add]("add", "add two numbers")(x => (x.a + x.b).toString)
```

```scala
val (serverEnd, clientEnd) = McpLink.pair()
val prog = for {
  server <- Async.fork(McpServer.run(serverEnd, "calc", "1.0", tools))
  client <- McpClient.connect(clientEnd, "probe", "1")
  listed <- client.tools
  sum <- client.call("add", "{\"a\": 2, \"b\": 40}")
  _ <- server.cancel
} yield (client.server, listed.map(_.name), listed.head.schema.contains("\"b\""), sum)
assertEquals(Eff.runAsync(prog), (Some(("calc", "1.0")), Seq("add"), true, "42"))
```

- `McpServer.run(link, name, version, tools, resources)` serves until the
  link closes. `resources` maps a uri to its text, and a client reads it
  with `client.read(uri)`, which is `None` for a uri the server lacks.
- A client also has `resources`, `prompts` and `prompt(name, args)`.
  The last answers the conversation opening as okay-agent's `Turn`s.
- Not wrapped: okay-rag's `PgVector` (a vector store across a wire).
  Its wrapper would need a live Postgres to test, and the default gate
  runs none. It is the same `VectorIndex` shape when it is asked for.

## 8o. Optics: lens, prism, affine, traversal, iso

Module `okay-scala2-optics`. An okay optic is `Optic[C[_[_, _]], S, T,
A, B]`: its kinds are Scala 3 top-level aliases, its constraints are
type lambdas over intersections, and every operation is an `inline`
extension, so none of it reaches Scala 2. Here the five kinds are Scala
2 classes, `Lens[S, A]`, `Prism[S, A]`, `Affine[S, A]`,
`Traversal[S, A]` and `Iso[S, A]`. Each is a shell over okay's own
optic: building one calls okay's constructor, `andThen` is okay's
composition, and `get`, `set`, `modify`, `preview`, `toVector` are
okay's operations. The shells are monomorphic: a `set` does not change
the type. Scala 2 has no macros here, so a lens is written by hand from
a getter and a copy. The code below is copied from
`okay-scala2/probe/src/test/scala/TestOpticsFromScala2.scala`:

```scala
val address = Lens[Person, Address](_.address, (p, a) => p.copy(address = a))
val city = Lens[Address, String](_.city, (a, c) => a.copy(city = c))
```

```scala
val personCity = address.andThen(city)
assertEquals(personCity.get(ada), "London")
assertEquals(personCity.set("Paris")(ada).address, Address("Paris", "N1"))
assertEquals(personCity.modify(_.toUpperCase)(ada).address.city, "LONDON")
```

A prism picks one case of a sum. `Prism.subtype` does it by class for a
sealed hierarchy, and a lens after it reaches inside that case only:

```scala
val circle = Prism.subtype[Shape, Circle]
val shapes = Vector[Shape](Circle(1.0), Rect(2.0, 3.0))
assertEquals(shapes.map(circle.preview), Vector(Some(Circle(1.0)), None))
assertEquals(shapes.map(circle.andThen(radius).modify(_ * 10)), Vector[Shape](Circle(10.0), Rect(2.0, 3.0)))
```

A traversal reaches every part, in order:

```scala
val prices = items.andThen(Traversal.each[Item]).andThen(price)
assertEquals(prices.toVector(order), Vector(10, 25))
assertEquals(prices.modify(_ * 2)(order).items.map(_.price), Vector(20, 50))
```

- Composition follows the lattice. An iso keeps the kind it meets. A lens
  then a lens is a lens. A lens then a prism, or a prism then a lens, is
  an `Affine`: at most one part, so `preview` rather than `get`, and a
  `set` where the part is absent leaves the value unchanged. Anything
  then a traversal is a traversal.
- `Prism.some[A]` is the value inside a `Some`. `Traversal(parts,
  rebuild)` makes a traversal from two functions; `Traversal.each` and
  `eachList` cover vectors and lists.
- A prism's `review` travels beside the okay optic and is composed
  explicitly, because the profunctor encoding has no way to build an
  `S` without one.
- The encoding is Pickering, Gibbons and Wu, "Profunctor optics: modular
  data accessors" (Programming 1(2), 2017,
  [doi:10.22152/programming-journal.org/2017/1/7](https://doi.org/10.22152/programming-journal.org/2017/1/7));
  docs/theory/10-optics.md explains it.

## 8p. Durable workflows and durable agents

Module `okay-scala2-workflow`. A durable program is one that survives
its process. Its whole state is a JOURNAL of the answers it was given,
and a new process over the same journal replays the program to exactly
where the old one stood, asking nobody anything twice.
[Chapter 23 of the continuations book](continuations/23-durable-workflows.md)
explains the engine; this section is how Scala 2 uses it.

A workflow is an ordinary program, `Eff[Workflow[Q, A], R]`. `Q` is the
type of the questions it asks the outside world, and `A` the type of the
answers. `Workflow[Q, A]` holds the operations. Every operation's answer
is journalled, the clock and ids included, so a replay reads them back.
The code below is copied from
`okay-scala2/probe/src/test/scala/TestWorkflowFromScala2.scala`:

```scala
val wf = Workflow[String, String]
val runtime = Wf.Runtime.scripted(1700000000000L, "bk-1", 0.25)
```

```scala
val booking: Eff[Workflow[String, String], String] = for {
  city <- wf.ask("which city?")
  when <- wf.now
  ref <- wf.uuid
  nights <- wf.ask(s"how many nights in $city?")
} yield s"$city/$nights/$when/$ref"
```

`Workflows.drive` runs it, an oracle answering the questions, and hands
back where it stopped and the journal. `Workflows.replay` is the "new
process": it reads only the journal.

```scala
val (_, journal) = Workflows.drive(booking, Nil, runtime)(oracle)
assertEquals(Workflows.replay(booking, journal), Some("Kyiv/3/1700000000000/bk-1"))
```

A worker has no oracle. `Workflows.advance` goes as far as the runtime
alone can take the run and stops at the author's next question. Whoever
answers (a person, an API) appends `Right(answer)`, and the next
`advance` goes on from there:

```scala
val (first, entries1) = Workflows.advance(booking, Nil, runtime)
assertEquals(first, Wf.Step.Asking[String, String]("which city?"))
val journal1 = entries1 :+ Right("Lviv")
val (second, entries2) = Workflows.advance(booking, journal1, runtime)
assertEquals(second, Wf.Step.Asking[String, String]("how many nights in Lviv?"))
```

A durable sleep and a signal stop the run as `Waiting`. Nothing is held
in memory meanwhile. A scheduler appends `Workflows.elapsed` when the
instant passes, and whoever sends the signal appends `Workflows.got(payload)`:

```scala
val (slept, e1) = Workflows.advance(approval, Nil, runtime)
assertEquals(slept, Wf.Step.Waiting[String, String](Wf.Wait.Until(1700000000000L + 86400000L)))
val j1 = e1 :+ Workflows.elapsed
val (waiting, e2) = Workflows.advance(approval, j1, runtime)
assertEquals(waiting, Wf.Step.Waiting[String, String](Wf.Wait.Signal("approve")))
```

- `wf.patch("id")` changes a running program safely: it is `true` for new
  runs and `false` for a journal written before the branch existed
  (Temporal's `getVersion`).
- `wf.random`, `wf.awaitChild(id)` and `wf.cancelled` are the rest of the
  operations. `Wf.Runtime.live` reads the real clock;
  `Wf.Runtime.scripted` is for tests. A Scala 2 class can implement
  `Wf.Runtime` too.
- The journal is `List[Either[Wf.SysA, A]]`, plain data to keep in any
  store.

**A durable agent** (section 8d's `Chat`) takes a journal of its tool
calls. A new `Chat` over the same journal, after a restart, replays the
calls that already happened instead of running them again. A payment is
not made twice. From `TestDurableAgentFromScala2.scala`:

```scala
val first = Chat(script, tools, Policy.all, journal = Some(journal))
assertEquals(Eff.runAsync(first.say("pay ada")), "paid")
assertEquals(payments, 1)
```

```scala
val second = Chat(script, tools, Policy.all, journal = Some(journal))
assertEquals(Eff.runAsync(second.say("pay ada")), "paid")
assertEquals(payments, 1)
```

`journal` is `new Durable.MemoryJournal` there. `okay.agent.Durable.Journal`
is a three-method trait (`append`, `complete`, `all`), so a Scala 2 class
backed by a table is enough. `onRepeat` says, per tool, what a call means
if the journal holds its intent but not its outcome. The default refuses
(`Fail`); `Redo` runs it again, for a tool that is safe to repeat.

## 8q. Services: actors, outbox, logs, traces, ops, Kafka, Postgres

Module `okay-scala2-services`. In these libraries the values and the
builders are plain, and Scala 2 uses them directly: `ActorRef`,
`Supervise`, `Reply`, `new Outbox()`, `new Inbox()`, `Dialect`,
`Log.Line`, `new Tracer(topic)`, `new Red(name)`, `new Lifecycle()`,
`new KafkaStore(bootstrap)`. What does not carry over is each
operation that answers a program. Those are the objects `Actors`,
`Outboxes`, `Logs`, `Tracing`, `Operations`, `Kafkas` and `Postgres`,
each named apart from the library's own. The code below is copied from
`okay-scala2/probe/src/test/scala/TestServicesFromScala2.scala`.

**Actors** (okay-actor). An actor is a state and what a message does to
it. `ask` sends a message carrying a `Reply` box and waits a bounded time
for the answer:

```scala
val counter: (Int, Msg) => Eff[Async, Int] = {
  case (n, Add(k)) => Eff.pure(n + k)
  case (n, Get(reply)) => Async.delay { reply(n); n }
  case (_, Boom) => Async.delay(throw new IllegalStateException("boom"))
}
```

```scala
val prog = for {
  actor <- Actors.spawn(0)(counter)
  _ <- Actors.tell(actor, Add(2))
  _ <- Actors.tell(actor, Add(3))
  total <- Actors.ask[Msg, Int](actor, 1000)(Get(_))
  _ <- Actors.stop(actor)
  after <- Actors.tell(actor, Add(1))
} yield (total, after)
assertEquals(Eff.runAsync(prog), (Some(5), false))
```

A behaviour that throws stops the actor, unless it was spawned with a
policy: `Actors.spawn(init, Supervise.Restart(() => fresh), capacity)`
starts again from a fresh state, and `Supervise.Resume` keeps the state
and drops the message. `Actors.child(parent, ...)` is stopped with its
parent.

**The outbox** (okay-outbox) writes a message in the same database, and
so the same transaction, as the change it announces; a relay publishes
it to an okay-persist store later, exactly once per message. An inbox
runs a message's handler once per message id:

```scala
_ <- Outboxes.enqueue(outbox, db, "orders", "order-1".getBytes)
waiting <- Outboxes.pending(outbox, db)
relayed <- Outboxes.relayOnce(outbox, db, store)
```

```scala
first <- Outboxes.once(inbox, db, "msg-1")(Async.delay { handled += 1; "done" })
second <- Outboxes.once(inbox, db, "msg-1")(Async.delay { handled += 1; "done" })
```

`db` is okay-scala2-sql's `Db` (section 8c). Call `enqueue` inside its
`transaction` to commit the message with the change. The pattern is the
transactional outbox of Richardson, *Microservices Patterns* (Manning,
2018), chapter 3.

**Logs and traces** (okay-obs). A log line is a `Writer` of `Log.Line`,
so logging is a capability like any other, and `Logs.to` sends each line
to a sink as it is said:

```scala
val work: Eff[Writer[Log.Line], Int] = for {
  _ <- Logs.debug("noise")
  _ <- Logs.info("started", "job" -> "42")
  _ <- Logs.failure("failed", new IllegalStateException("disk"))
} yield 7
val answer = Eff.run(Logs.to[Any, Int](l => lines += l, Log.Level.Info, () => 1000L)(work))
```

`Tracing.span(tracer, name, attrs*)(program)` runs a program inside a
span; the tracer writes finished spans to an okay-persist topic, and
`okay.obs.Otlp` exports them.

**Ops** (okay-ops): the health, readiness, metrics and stats endpoints,
in the HTTP facade's terms (section 8b), and a RED meter (rate, errors,
duration) around any routes:

```scala
val health = Eff.runAsync(Operations.routes(store)(Request.get("/healthz")))
assertEquals((health.status, health.text), (200, "live=true"))
val red = new Red("api")
val app = Operations.measured(red, _ => "hello") { case _ => Eff.pure(Response.text("hi")) }
```

`Operations.admitted(lifecycle)(routes)` refuses new requests once
draining begins, and `Operations.drain(lifecycle, graceMillis)` waits for
the requests in flight: a graceful shutdown.

**Kafka and Postgres.** `new KafkaStore(bootstrap)` is an okay-persist
`Store`, so section 8k works over Kafka unchanged. `Kafkas.send`,
`Kafkas.source` and `Kafkas.commit` drive a plain Kafka producer and
consumer. `Postgres.connect(host, port, user, password, database)` is a
`Db` over okay-pg's own implementation of the wire protocol. Its SQL is
Postgres' own, with numbered placeholders (`$1, $2`) where JDBC writes
`?`. Both are tested against real servers in
`TestServicesLiveFromScala2`, which is tagged `Live` and runs under
`sbt integrationTest`.

## 9. Scala 3 and Scala 2, side by side

| Scala 3 (`okay`) | Scala 2.13 (`okay.scala2`) |
|---|---|
| `A ! (State % Int + Writer % String)` | `Eff[State[Int] with Writer[String], A]` |
| `State.get[Int]`, `Writer.tell(w)` | the same names, on the companions in `okay.scala2` |
| `State.run(s)(p)` / `State.handle(s)(p)` | `State.run(s)(p)`, which leaves the rest of the row |
| `enum KV[+A] derives Effect` | `sealed trait KV[A] extends Op[A]` + `object KV extends Effect[KV]` |
| a handler as `F !> S`, `Effects[Free].handle` | `Handler[F, R, B]`, `KV.handle` / `KV.run` |
| `shift` / `reset` / `Cont[A, S, R]` | `Cont.shift` / `Cont.reset` / `Cont[A, S, R]` |
| `Source[A]` (a type alias) | `Source[A]` (a class) |
| `Async.spawn`, `Fiber`, `Channel` | `Async.fork`, `Fiber`, `Channel` |
| `case class P(...) derives Schema` | `Schemas.productN("P", ...)(P.apply)(p => (...))` |
| `okay.codec.Json.write` / `read` | `okay.scala2.Json.write` / `read` (text in, text out) |
| `Route / "users" / Route[Int]("id")` + `Router` | `Routes { case GET(Path("users", id)) => ... }` |
| `okay.http.Response`, `Server.serve` under `Resource` | `okay.scala2.Response`, `Server.use` / `Server.start` |
| `Typed.rows(db, sql)`, `Typed.transact(db)(...)` | `Db.jdbc(conn).rows[A](sql)` / `.all[A]`, `db.transaction()(tx => ...)` |
| `Agent.converse(...)` under `Handler.union` of model, tool and context handlers | `Chat(model, tools, policy).say(message)` |
| `Ui.run(init)(view)(update)(host)` | `UiApp.run(init)(view)(update)(host)`, hosts from `UiHost` |
| `direct { ... }` blocks | not available: use `for` |

## 10. Errors you may see, and what they mean

Each message below was seen while building this module. The
unhandled-effect row is also pinned in `TestScala2Guide` with
`compileErrors`.

| message | cause | fix |
|---|---|---|
| `Unsupported Scala 3 inline method flatMap; found in class okay.Free` | code calls the Scala 3 API (`okay.*`) directly | use the types in `okay.scala2` |
| `error while loading Json, class file '.../okay/codec/Json.tasty' is broken (class scala.MatchError/49)` | code names `okay.codec.Json` (or `JsonSchema.of`) from Scala 2 | `okay.scala2.Json` / `okay.scala2.JsonSchema` |
| `Unsupported Scala 3 union in bounds of type T; found in method wrapRefArray in class scala.LowPriorityImplicits` | the 3.9 stdlib comes BEFORE the 2.13 one on the compile classpath | the exclusion and the appended jar from section 1 |
| `could not find package scala.annotation.internal` | the 3.9 stdlib is missing at compile time | append it (section 1) |
| `NoClassDefFoundError: scala/reflect/Enum` | the 3.9 stdlib is missing at run time. On `sbt run` with everything else right, it means the `dependencyClasspathAsJars` line is missing | the whole block of section 1, both lines |
| ``Expected `<project> / scalaVersion` to be 3.9.0 or later, but found 2.13.18`` | sbt found `scala-library:3.9.0` among the dependencies (SIP-51) | exclude it on the dependency (section 1); do NOT reach for `allowUnsafeScalaLibUpgrade`, which makes 3.9 the compile stdlib and gives the second error of this table |
| `type mismatch` ... `required: okay.scala2.Eff[okay.scala2.State[Int] with Any,?]` (for a program over `State[Int] with Writer[String]` passed straight to `Eff.run(State.run(1)(...))`) | an effect is left unhandled (here `Writer`); scalac reports it at the handler, not at the missing one | handle it before `Eff.run` |
| `a type was inferred to be Any` at `X.handle(...)` | `handle` used for the last effect | use `X.run(...)` (section 5) |
| `Unsupported Scala 3 union in bounds of type +; found in object okay.Effects$package` (at your `package` line) | code names a Scala 3 class whose CONSTRUCTOR mentions an effect row, such as `okay.http.Response`; or code writes `new` for a class whose METHODS do, such as `new okay.docs.TopicDocs[A](topic)`, because `new` makes the reader complete the whole class | use the `okay.scala2` type (`okay.scala2.Response`), or its factory (`Documents.onTopic`) — a factory that answers the class is fine: `Fs(root)` works where `new TopicDocs` does not |
| `Unsupported Scala 3 generic tuple type scala.Tuple` | code names okay-http's `Route` | route by pattern matching (section 8b) |
| `type Chunk is not a member of package okay`, or `not found: type Schema` for an alias you imported, or `can't find type required by method memory ...: okay.Pure; perhaps it is missing from the classpath` (any Scala 3 top-level alias, also when it is only INSIDE a signature you call) | Scala 3 top-level aliases are invisible from Scala 2 | name the type the alias stands for: `ArraySeq[Byte]` for `Chunk[Byte]`, `okay.codec.Schema` for `Schema` |
| `type mismatch; found: Source[Event.Pressed]; required: Source[Event]` | from Scala 2 a Scala 3 enum case is typed as the case | write the type argument: `Source[Event](...)` |
| `No suitable driver found for jdbc:...` in tests that pass alone | unforked sbt tests share one JVM, and `DriverManager` serves only drivers visible to the loader that registered them first | open through the driver (`new org.h2.Driver().connect(url, props)`) (section 8c) |

## 11. What is not here, and why

- **Direct style** (`direct { ... }`, auto-colouring) is built from
  Scala 3 macros, and Scala 2 cannot expand those. Use
  for-comprehensions instead. It is not planned.
- **The rest of the library.** Codecs (section 8a), HTTP (8b), SQL
  (8c), agents (8d) and UI (8e) are covered. Several pieces are not
  wrapped yet: durable agents, and okay-ui's cancellable scopes
  (`Scope`) inside a dialog. Forms, WebSockets, nondeterminism with
  search, and dialogs with screens are wrapped (sections 8f to 8i). The remaining modules are not
  wrapped either.
- **Performance.** Every `okay.scala2` combinator calls okay's own
  combinator, and the program underneath is the same `Free` tree the
  Scala 3 API builds. What Scala 3 code gets and a 2.13 caller does
  not is okay's `inline` fast paths at the call site. That cost has not
  been measured; do not assume the Scala 3 benchmark numbers in
  docs/benchmarks.md hold for code written against this facade.
- **Future Scala versions.** 2.13's TASTy reader supports Scala 3
  releases up to a version tied to each 2.13 release. When okay moves
  to a newer Scala 3, the 2.13 probe in this repository is the first
  thing to break, and the fix is usually a newer 2.13.

## 12. How it works

In one paragraph: okay's effect row is a union type, which Scala 2
cannot write, and its combinators are `inline`, which Scala 2 cannot
call. `okay-scala2` is written in Scala 3. It keeps okay's program
underneath each class and exposes plain methods whose signatures a
Scala 2 compiler can read. On the Scala 2 side the row is a phantom
intersection of capabilities (the shape of ZIO 1's environment), so
one cast, in one function, re-types the stored program at each
handler's concrete row. The theory, with the literature, is in
[theory ch. 13](theory/13-rows-without-unions.md). The measurements
and the design decisions, including the hypotheses that turned out to
be wrong, are in
[specs/scala2-facade.md](../specs/scala2-facade.md). The API page is
[modules/okay-scala2.md](modules/okay-scala2.md).
