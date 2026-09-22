# okay from Scala 2.13

okay is a Scala 3 library. This guide is for a codebase that is still
on **Scala 2.13** and wants to use it: effects, several in one program,
your own effects, continuations, streams, fibers and channels. It all
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
  ("dev.okay" %% "okay-scala2" % "0.1.1")
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

The version is `ThisBuild / version` in okay's build.sbt (0.1.1 at the
time of writing).

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
| `direct { ... }` blocks | not available: use `for` |

## 10. Errors you may see, and what they mean

Each message below was seen while building this module. The
unhandled-effect row is also pinned in `TestScala2Guide` with
`compileErrors`.

| message | cause | fix |
|---|---|---|
| `Unsupported Scala 3 inline method flatMap; found in class okay.Free` | code calls the Scala 3 API (`okay.*`) directly | use the types in `okay.scala2` |
| `Unsupported Scala 3 union in bounds of type T; found in method wrapRefArray in class scala.LowPriorityImplicits` | the 3.9 stdlib comes BEFORE the 2.13 one on the compile classpath | the exclusion and the appended jar from section 1 |
| `could not find package scala.annotation.internal` | the 3.9 stdlib is missing at compile time | append it (section 1) |
| `NoClassDefFoundError: scala/reflect/Enum` | the 3.9 stdlib is missing at run time. On `sbt run` with everything else right, it means the `dependencyClasspathAsJars` line is missing | the whole block of section 1, both lines |
| ``Expected `<project> / scalaVersion` to be 3.9.0 or later, but found 2.13.18`` | sbt found `scala-library:3.9.0` among the dependencies (SIP-51) | exclude it on the dependency (section 1); do NOT reach for `allowUnsafeScalaLibUpgrade`, which makes 3.9 the compile stdlib and gives the second error of this table |
| `type mismatch` ... `required: okay.scala2.Eff[okay.scala2.State[Int] with Any,?]` (for a program over `State[Int] with Writer[String]` passed straight to `Eff.run(State.run(1)(...))`) | an effect is left unhandled (here `Writer`); scalac reports it at the handler, not at the missing one | handle it before `Eff.run` |
| `a type was inferred to be Any` at `X.handle(...)` | `handle` used for the last effect | use `X.run(...)` (section 5) |

## 11. What is not here, and why

- **Direct style** (`direct { ... }`, auto-colouring) is built from
  Scala 3 macros, and Scala 2 cannot expand those. Use
  for-comprehensions instead. It is not planned.
- **The rest of the library** (HTTP, SQL, codecs, the agent stack, UI,
  ...) is not wrapped. Each piece that a 2.13 user needs gets a facade
  the same way, when someone needs it.
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
