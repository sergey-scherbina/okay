# okay2 — the core, written a second time in Scala 2.13

`okay-scala2` (docs/scala2.md) is a FACADE: Scala 3 code that wraps
the library so a 2.13 compiler can read its signatures. `okay2` is the
other road: the core itself — the freer tree, its rotation, the row,
the handlers, `Cont` — written again in Scala 2.13, with nothing of
Scala 3 on the classpath. One standard library, no arrangement, no
`-Ytasty-reader`. The design turned out to be a property of the
design and not of the language: the runtime never depended on the
union type, only the spelling of the row did (specs/okay2.md).

Every snippet below is a line of `okay2/src/test/scala/okay2/*.scala`,
compiled by scalac 2.13.18 under `-Xlint -Werror`. `okay2/` is its own
sbt build (it shares nothing with the Scala 3 build but the
repository), gated with `cd okay2 && ../scripts/gate.sh test`.

Contents:

1. [The build](#1-the-build)
2. [A program and its row](#2-a-program-and-its-row)
3. [Your own effect](#3-your-own-effect)
4. [Handlers, in any order](#4-handlers-in-any-order)
5. [Failure](#5-failure)
6. [Continuations](#6-continuations)
7. [Interpreting one effect into others](#7-interpreting-one-effect-into-others)
8. [What is different from Scala 3, and why](#8-what-is-different-from-scala-3-and-why)
9. [Interop: cats, fs2, zio](#9-interop-cats-fs2-zio)
10. [Streams: chunks, stages, pipelines, windows](#10-streams-chunks-stages-pipelines-windows)
11. [Async and the JVM platform](#11-async-and-the-jvm-platform)
12. [Channels and sources](#12-channels-and-sources)
13. [Literature](#13-literature)

## 1. The build

```sbt
libraryDependencies += "dev.okay" %% "okay2" % "<version>"
```

That is all: `okay2` is a Scala 2.13 artifact with no dependencies.
In this repository it is the separate build under `okay2/`:
`cd okay2 && sbt publishLocal` is what a consumer does until it is
published.

## 2. A program and its row

A program is `A ! R`: it computes `A` performing the operations of the
row `R`. In Scala 2 every infix type operator has the same precedence,
so the row after `!` is parenthesised. `State[Int]` is the signature
fixed at one state type (`State % Int` is the same type); a row of
several is written with `+`.

```scala
    val p: Int ! (State[Int]) =
      for {
        _ <- State.modify[Int](_ + 1)
        _ <- State.modify[Int](_ * 10)
        n <- State.get[Int]
      } yield n
    assertEquals(State.run[Int, Int](4)(p), (50, 50))
```

A program is built at its own signature's row and LANDS in a wider one
by `at`; the witness is compile-time only, and the cast behind it is
free because the row erases:

```scala
    type Row = State[Int] + Produce
    val p: Int ! Row =
      for {
        n <- State.get[Int].at[Row]
        m <- Produce.produce(n * 2).at[Row]
        _ <- State.set(m).at[Row]
      } yield n + m
    val (s, a) = State.handle(4)(p).runWith
    assertEquals((s, a), (8, 12))
```

A row that does not mention the effect refuses the program — at
compile time, by name:

```scala
    val errors = compileErrors("Writer.tell(\"x\").at[State[Int] + Produce]")
    assert(errors.contains("does not fit in the row"), errors)
```

`bind` infers the union when the continuation answers in another
row, and `plus` adds a signature with no witness at all:

```scala
    val p: Int ! (State[Int] + Produce) = State.get[Int].bind(n => produce(n + 1))
    assertEquals(State.handle(1)(p).runWith, (1, 2))
```

## 3. Your own effect

A signature is a `Row` whose `Op` names its operations; the operations
live in the companion, with one implicit that says what the signature
is — the Scala 2 spelling of `enum Produce[+A] derives Effect`:

```scala
sealed trait Produce extends Row { type Op[+A] = Produce.Emit[A] }

object Produce {
  final case class Emit[+A](a: A)

  implicit val effect: Effect[Produce] = Effect.of[Produce]

  /** each operation answers with its own value */
  implicit val handler: Handler[Produce] = new Handler[Produce] {
    def handle[A](a: Emit[A]): A = a.a
  }

  def produce[A](a: A): A ! Produce = Free.inject[Produce, A](Emit(a))
}
```

`Effect.of` reads the class off the ClassTag of the operations, and
that is what a row split tests at run time. A ROW has no such class —
its `Op` is abstract — so `Effect.of[State[Int] + Produce]` is refused,
which is right: the erasure of a union would be a class every
operation matches.

A comonadic `Handler` answers each operation with a value, and
`runWith` runs the program by it; a row is run by one handler per
effect, assembled by `Handler.union`; and every handler is a recording
one for free, because the operations are already data:

```scala
    type Row = Op + Produce
    implicit val opH: Handler[Op] = new Handler[Op] { def handle[A](a: Op.Val[A]): A = a.a }
    implicit val rowH: Handler[Row] = Handler.union[Op, Produce]
    val p: Int ! Row = Op.op(1).at[Row].flatMap(x => produce(x + 1).at[Row])
    assertEquals(p.runWith, 2)
    // recording is a decorator over the real handler
    val log = List.newBuilder[Any]
    assertEquals(p.runWith(rowH.tracing(log += _)), 2)
    assertEquals(log.result(), List(Op.Val(1), Produce.Emit(2)))
```

## 4. Handlers, in any order

`State.handle(s)(p)` runs the State part of `p` and leaves a program
over the rest of the row. In Scala 3 a union commutes, so the handler
finds its signature wherever it is; in Scala 2 `A + B + C` is
`(A + B) + C`, so `okay2` finds it with a witness (`Remove[F, R]`, "R
without F") — the effect may be anywhere, and handlers may be applied
in either order:

```scala
  type Row = State[Int] + Writer[String] + Produce
```

```scala
    // handlers in EITHER order: `Remove` finds the signature anywhere in the row
    val (ws, (s, x)) = Writer.run[String, (Int, Int), Writer[String] + Produce](State.handle(41)(p)).runWith
    assertEquals((s, ws, x), (41, Seq("x"), 42))
    val (s2, (ws2, x2)) = State.handle(41)(Writer.run[String, Int, Row](p)).runWith
    assertEquals((s2, ws2, x2), (41, Seq("x"), 42))
```

A stack-safe 1M-element State program, indexed as the Scala 3 core's
test does it:

```scala
    val n = 1000000
    assertEquals(State.index(LazyList.from(1).take(n))._1, n.toLong)
```

## 5. Failure

`Throws[E]` fails with any E; `runEither` reifies it, `runOption` the
reason-less `Abort`, `runUnsafe` throws. `recover` answers the failure
INSIDE the row, so what follows neither knows nor cares:

```scala
    type Row = Throws[String] + Produce
    val p: Int ! Row = Throws.raise[String, Int]("x").at[Row].recover(e => produce(e.length).at[Row])
    assertEquals(Throws.runEither[Int, String, Row](p).runWith, Right(1))
    val q: Int ! Row = Throws.raise[String, Int]("x").at[Row].orElse(pure(9))
    assertEquals(Throws.runEither[Int, String, Row](q).runWith, Right(9))
```

An abort does not run what follows it, and a `Writer` beside it sees
exactly what happened before:

```scala
    type Row = Throws[String] + Writer[String]
    val p: Int ! Row = Writer.tell("before").at[Row].flatMap(_ => Throws.raise[String, Int]("stop").at[Row]).flatMap(x => Writer.tell("after").at[Row].map(_ => x))
    val (ws, r) = !.run(Writer.run[String, Either[String, Int], Writer[String] + Pure](Throws.runEither[Int, String, Row](p).plus[Pure]))
    assertEquals(r, Left("stop"))
    assertEquals(ws, Seq("before"))
```

## 6. Continuations

`Cont[A, S, R]` means `(A => S) => R`: the parameterised continuation
monad with Danvy and Filinski's `shift`, answer-type modification
included. It is a facade over the same tree every program is made of,
and a 1M-deep chain is safe:

```scala
    val c: Cont[Int, String, String] =
      shift[Int, String, String](k => k(20) + "!").flatMap(x => Cont.Pure[Int, String](x * 2))
    assertEquals(c / (x => s"got $x"), "got 40!")
```

```scala
    val abort: Int /> Int = shift[Int, Int, Int](_ => -1)
    assertEquals(reset(abort.flatMap(x => Cont.Pure[Int, Int](x + 1))), -1)
    val twice: Int /> Int = shift[Int, Int, Int](k => k(1) + k(10))
    assertEquals(reset(twice.flatMap(x => Cont.Pure[Int, Int](x * 2))), 22)
```

Type-changing state rides on it — the state's TYPE moves through the
answer type, and the compiler checks the protocol order:

```scala
    val r = PState.run[Int, Boolean, String](41) {
      for {
        n <- PState.get[Int, (Boolean, String)]                              // n: Int
        _ <- PState.set[Int, String, (Boolean, String)]((n + 1).toString)    // the state is a String now
        s <- PState.get[String, (Boolean, String)]                           // s: String
        _ <- PState.set[String, Boolean, (Boolean, String)](s.length == 2)   // the state is a Boolean now
      } yield s + "!"
    }
    assertEquals(r, (true, "42!"))
```

The runner's own copy of the rotation is licensed by a law: it agrees
with `Func`, the closure encoding that never rotates, on the answer
and on the order the effects happened in, over twelve bind-tree shapes
(TestFree).

## 7. Interpreting one effect into others

A handler valued in a PROGRAM (`translate`) may answer an operation
with more computation — here a Reader is answered by a program that
tells on the way, and the Writer beside it is forwarded untouched:

```scala
    type Row = Reader[Int] + (Writer[String] + Pure)

    val prog: Int ! Row =
      Reader.ask[Int].at[Row].flatMap(x =>
        Reader.ask[Int].at[Row].map(_ + x))
```

```scala
    val (ws, a) = !.run(Writer.run[String, Int, Writer[String] + Pure](told))
    assertEquals(a, 42)
    assertEquals(ws, Seq("asked", "asked"))
```

A Cont-valued handler (`handle`) may abort or resume the rest of the
program more than once:

```scala
    assertEquals(both.runWith, List(11, 11))
```

## 8. What is different from Scala 3, and why

Each of these was measured before it was decided (specs/okay2.md):

- **A row is a type of kind `*`** with a member `type Op[+A]`, and
  `F + G` leaves `Op` abstract. Scala 2 cannot give a type alias the
  kind `* -> *` by partial application, so the Scala 3 union
  `[A] =>> F[A] | G[A]` has no spelling; an abstract member erases to
  Object exactly as the union does, and an operation of a union row
  is held raw in the tree (TestRow checks the class).
- **A union does not commute.** `A + B` and `B + A` are different
  types. `at` reorders a program's row by a witness, and every handler
  finds its signature by `Remove`, so neither costs you anything but
  the spelling.
- **Construct at the signature, widen with `at`.** `effect[F + G, A]
  (op)` does not type — no operation IS a value of an abstract `Op`.
  Every signature's named constructors are its API.
- **Parenthesise the row after `!`**, and write parameterised
  signatures applied (`State[Int]`), because `A + B % C` is
  `(A + B) % C` in Scala 2.
- **No `inline`.** The hot paths are ordinary methods for the JIT;
  the number against the Scala 3 core is a backlog item, not a claim.
- **Handlers are traits**, since Scala 2 has no polymorphic function
  types: `Interpr[F, S]` (the Cont-valued `F !> S`), `Interpret[F, G]`
  (translate's), `Relay[F]` (relay's).
- **`Distinct` is not checked**: two `State[_]` of different
  parameters in one row are two types to the witness and one class to
  the split, and misroute loudly at the first wrong answer, as the
  Scala 3 core did before its macro.

## 9. Interop: cats, fs2, zio

Three modules beside the core in the `okay2/` build: `okay2-cats`,
`okay2-fs2`, `okay2-zio` (kyo publishes for Scala 3 only, so there is
no `okay2-kyo`). The shape is the freer-monad one: a program is a
tree, and interop is interpreting that tree in the target monad — an
`Into[R, M]` says what each operation of the row means in `M`, a
union's is composed from its parts, and `foldTo` walks the tree
stack-safely. Every snippet is a line of the modules' tests.

**cats.** A program row is a `Monad`, and a row with `Throws[E]` at
its head a `MonadError` — `import okay2.cats.instances._`:

```scala
    type Prog[A] = A ! Produce
    val M = Monad[Prog]
    val p: Prog[Int] = M.flatMap(M.pure(1))(x => produce(x + 1))
    assertEquals(p.runWith, 2)
```

```scala
    type Row = Throws[String] + Produce
    type Prog[A] = A ! Row
    val E = MonadError[Prog, String]
    val failed: Prog[Int] = E.raiseError[Int]("no")
    val mended: Prog[Int] = E.handleErrorWith(failed)(e => produce(e.length).at[Row])
```

The `Io` row's operations ARE `IO` values: `Io.lift` puts one in a
program, `Io.run` folds a program into one `IO` once every other
effect has been handled:

```scala
    type Row = State[Int] + Io
    val p: Int ! Row = for {
      n <- State.get[Int].at[Row]
      m <- Io.lift(IO(n * 2)).at[Row]
      _ <- State.set(m).at[Row]
      k <- Io.lift(IO.pure(1)).at[Row]
    } yield m + k
    val io: IO[(Int, Int)] = Io.run(State.handle(21)(p))
    assertEquals(io.unsafeRunSync(), (42, 43))
```

Any monad, by an `Into` of your own — here Option:

```scala
    val intoOption: Into[Produce, Option] = new Into[Produce, Option] {
      def apply[X](e: Produce.Emit[X]): Option[X] = Some(e.a)
    }
    assertEquals(foldTo[Option, Int, Produce](p)(intoOption), Some(3))
```

**fs2.** A Writer program IS a stream. `toFs2` reads it as one, the
other effects run between the elements, lazily — `take(1)` runs
nothing past the first element:

```scala
    type Row = Writer[String] + Io
    var side = List.empty[String]
    val p: Unit ! Row = for {
      _ <- Writer.tell("a").at[Row]
      _ <- Io.lift(IO { side ::= "io" }).at[Row]
      _ <- Writer.tell("b").at[Row]
    } yield ()
    val s: Stream[IO, String] = toFs2[IO, String, Unit, Row, Io](p)
    assertEquals(side, Nil) // building the stream ran nothing
    assertEquals(s.compile.toList.unsafeRunSync(), List("a", "b"))
    assertEquals(side, List("io"))
```

And an IO stream as a Writer program, one element pulled per `Io`
operation:

```scala
    val s: Stream[IO, Int] = Stream.range(1, 6).evalMap(i => IO { pulled += 1; i })
    val p: Unit ! (Writer[Int] + Io) = fromFs2(s)
    assertEquals(pulled, 0)
    val collected: IO[(Seq[Int], Unit)] = Io.run(Writer.run[Int, Unit, Writer[Int] + Io](p))
    assertEquals(collected.unsafeRunSync()._1, Seq(1, 2, 3, 4, 5))
    assertEquals(pulled, 5)
```

**zio.** The same three: the `Zio` row (`Task` as an operation),
`foldTo` into a ZIO with an environment and an error type of your
choosing, a Writer program as a `ZStream`:

```scala
    type Row = State[Int] + Zio
    val p: Int ! Row = for {
      n <- State.get[Int].at[Row]
      m <- Zio.lift(ZIO.attempt(n * 2)).at[Row]
      _ <- State.set(m).at[Row]
    } yield m + 1
    val z: Task[(Int, Int)] = Zio.run(State.handle(21)(p))
    assertEquals(run(z), (42, 43))
```

```scala
    val s: ZStream[Any, Throwable, String] = toZStream[Any, Throwable, String, Unit, Row, Zio](p)
```

What the Scala 3 core's interop has and this does not yet: an `Async`
program run under `IO.blocking`, and chunked streams — okay2 has
neither effect yet (backlog `okay2-stage2`). `fromZStream` collects
the stream in one operation: a pull that survives across a program's
operations is a scoped resource, and that is the Resource effect's
job when it comes.

## 10. Streams: chunks, stages, pipelines, windows

`okay2-stream` is okay-stream's pure layer (the asynchronous one —
channels, merge — follows `okay2-async`). Every snippet is a line of
its tests. A writer program IS a stream, and the core's `Stream`
typeclass observes any carrier by `uncons`:

```scala
    val p = told(1, 2, 3)
    assertEquals(p.uncons.map(_._1), Some(1))
    assertEquals(p.toLazyList.toList, List(1, 2, 3))
    assertEquals(p.iterator.toList, List(1, 2, 3))
```

**Chunks.** A chunked stream is an ordinary writer stream of whole
batches: the tree steps once per chunk, an element costs an array
index, and a `range` is a `long[]`. Everything is lazy — an infinite
generator builds only the chunks that are pulled:

```scala
    var built = 0
    val s = Chunks.generate(0)(x => { built += 1; x })(_ + 1)(8)
    assertEquals(built, 0)
    assertEquals(s.elements.take(20).toList, (0 until 20).toList)
    assertEquals(built, 24) // ceil(20/8) = 3 chunks of 8
```

```scala
    val ref = LazyList.from(0).map(_ * 2).filter(_ % 3 == 0).take(100).toList
    val c = Chunks.take(Chunks.filter(Chunks.map(Chunks.nats[Int](7))(_ * 2))(_ % 3 == 0))(100)
    assertEquals(c.elements.toList, ref)
```

**Stages.** A consumer is a program with the `Take` effect (the dual
of a writer), and `pipe` pairs the two as coroutines: the consumer
drives, so a finite consumer ends an infinite producer:

```scala
  def count(n: Int): Nothing ! Writer[Int] = Writer.tell(n).flatMap(_ => count(n + 1))
```

```scala
    assertEquals(pipe(count(0))(sums(5, 0)), 0 + 1 + 2 + 3 + 4)
```

A stage awaits I and tells O; `transduce` is the skeleton every stage
is made of — a state, a step that may tell nothing, one or many, and
a flush — and `into`/`through` compose stages demand-driven:

```scala
    val evens: Stage[Int, Int, Int] = Stage.transduce[Int, Int, Int](0)((sum, i) => {
      val s2 = sum + i
      if (i % 2 == 0) Stage.tell[Int, Int](s2).map(_ => s2) else pure(s2)
    }, s => Stage.tell[Int, Int](-s).map(_ => s))

    val (out, answer) = Effects.run(Writer.run[Int, Int, Writer[Int]](into(told(1, 2, 3, 4, 5, 6))(evens)))
    assertEquals(out, Seq(3, 10, 21, -21)) // 1+2, +3+4, +5+6, then the flush
    assertEquals(answer, 21)
```

Where Scala 3 overloads `through` four ways, Scala 2 gives each
pairing a word: `pipe`/`pipeIn`, `through`/`throughIn`, `into`/`intoIn`
— the `In` forms for stages and producers that perform another effect
between their elements, forwarded in order and lazily.

**Pipelines as values.** An operator tree you can rewrite (map fusion,
filter fusion, take pushed into a range) and then compile onto the
chunked transformers; a property test says every rewrite preserves
semantics:

```scala
    val p = Pipeline.range(0, 100).map(_ + 1).map(_ * 2).filter(_ > 4).filter(_ % 2 == 0)
    assertEquals(Pipeline.depth(p), 5)
    assertEquals(Pipeline.depth(Pipeline.optimize(p)), 3) // range, one map, one filter
```

**Event-time windows.** Keyed panes over an `Aggregator`, closed by a
watermark, reading no clock; as a class and as a stage:

```scala
    val w = Windows.tumbling(10L, 0L)((e: Ev) => e.key)((e: Ev) => e.ts)(sum)
    val out = Vector.newBuilder[Pane[String, Long]]
    w.add(Ev(3, "a", 1))(p => { out += p; () })
    assertEquals(out.result(), Vector.empty)
    w.add(Ev(11, "a", 2))(p => { out += p; () })
    assertEquals(out.result(), Vector(Pane(0L, 10L, "a", 1L)))
```

**A fold that stops.** `FoldUntil` on every carrier pulls exactly as
much as it needs — no chunk after the satisfying one, no effect after
the stop:

```scala
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.take[Int](3)), Vector(0, 1, 2))
    assertEquals(pulls, 1)
```

## 11. Async and the JVM platform

`okay2-async` is the effect; `okay2-platform` is the JVM under it.
`import okay2.platform._` installs the three capabilities every
blocking or forking door asks for — `CanBlock`, `Timer`, the default
`Scheduler` (Loom where the JVM has it) — as one implicit, so a local
`implicit val S: Scheduler = Schedulers.forkJoin()` overrides it with
no ambiguity. `Async(a)` suspends a computation (the Scala 3 core's
`async(a)`; a function of that name would collide with the package),
and a program stays a program until it is run:

```scala
    val prog: Int ! Async = Async(20).flatMap(x => Async(x + 22))
    assertEquals(Effects.run(Async.run(prog)), 42)
    assertEquals(prog.runWith, 42)
```

`par` runs both sides on their own fibers and sees EITHER side fail
without waiting out the healthy one; `race` cancels the loser;
`timeout` is settled by the program's first outcome of either kind:

```scala
    val a = new CompletableFuture[Unit]()
    val b = new CompletableFuture[Unit]()
    val prog = Async.par(
      Async { a.complete(()); b.get(10, TimeUnit.SECONDS); 1 },
      Async { b.complete(()); a.get(10, TimeUnit.SECONDS); 2 })
    assertEquals(prog.runWith, (1, 2))
```

```scala
    assertEquals(Async.timeout(2000)(Async(2)).runWith, Some(2))
```

A supervised scope owns its children: the first failure, a child's
or the body's, cancels every other child and leaves the scope with
that error. The body takes the nursery as a parameter:

```scala
    assertEquals(Effects.run(Async.run(Async.supervised[Int] { n =>
      val a = n.fork(Async(40)); val b = n.fork(Async(2))
      a.joinAsync.flatMap(x => b.joinAsync.map(y => x + y))
    })), 42)
```

The same effect drives through callbacks with nothing parked —
`runAsync` answers a `Future`, and a 10 000-operation chain drives in
constant stack:

```scala
    def go(n: Int): Int ! Async = if (n == 0) pure(0) else Async(1).flatMap(x => go(n - x).map(_ + x))
    Async.runAsync(go(10000)).map(v => assertEquals(v, 10000))
```

Retry policies are streams of delays, and `Retry.async` is retry as
a program on the platform timer:

```scala
    assertEquals(Retry.exponential(10).take(4).toList, List(10L, 20L, 40L, 80L))
```

The schedulers are the Scala 3 core's: `loom` (a virtual thread per
fiber), `forkJoin`, `drive` (the tree walked on pool threads, no
thread per fiber), `own`/`adaptive` (owned workers over Chase-Lev
deques, the helper rule, the stuck-check), `threads`. `Interruptible`
lifts blocking code as an operation whose cancel interrupts it,
whatever the scheduler.

## 12. Channels and sources

A channel is a queue between fibers, the primitive of CONCURRENT
streams; nobody waits in a thread — a receiver that finds it empty
leaves a callback, a sender that finds it full leaves the element and
a callback, and `send`/`receive` are Async programs. `StmChannel`
keeps its whole state in one immutable value behind one CAS, which is
what makes the close contract hold by construction: acceptance is
final, and the end comes after the buffer.

```scala
    val c = Channel[Int]()
    assert(c.offer(1)); assert(c.offer(2)); c.close()
    assertEquals(c.toLazyList.toList, List(1, 2))
    assertEquals(c.receiveBlocking(), None)
```

```scala
    val c = Channel[Int](capacity = 1)
    assertEquals(wait1(Async.runAsync(c.send(1))), true)
    val second = Async.runAsync(c.send(2))
    Thread.sleep(20)
    assert(!second.isCompleted, "a send into a full channel completed without room")
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(wait1(second), true)
```

A `Source[W]` is a program that tells its elements, performing Async
between them. `merge` joins two by READINESS — a fiber per source
feeds one channel, each side keeps its own order, the fibers start at
the first pull, and the merge is bounded by default. Scala 2 has no
union type, so the merged element type is the common supertype
(`merge[Any]`), or `either` keeps the side as data:

```scala
    val merged: Source[Any] = ticks(List(1, 3, 5)).merge[Any](ticks(List("a", "b")))
    val got = merged.toLazyList.toList
    assertEquals(got.collect { case i: Int => i }, List(1, 3, 5))
    assertEquals(got.collect { case s: String => s }, List("a", "b"))
```

`chunked = true` trades readiness for throughput (one channel
transaction per 16 elements) and `flushAfter` bounds how long a
partial chunk may wait; `Channel.buffer` runs a producer ahead of
its consumer; and the terminals are programs, not parked values:

```scala
    val s = Source.range(0L, 100L)
    assertEquals(s.runCollect.runWith, (0L until 100L).toVector)
```

The Scala 3 core's default channel is a ring buffer with termination
travelling as a mark (`SentinelChannel` over `Growing`/`Ring`/
`Segments`); those mechanisms are a later stage here, and
`Channel.apply` is the reference `StmChannel`.

## 13. Literature

- Oleg Kiselyov and Hiromi Ishii, "Freer Monads, More Extensible
  Effects" (Haskell Symposium 2015) — the tree, the relay handler,
  and why no Functor is needed.
- Olivier Danvy and Andrzej Filinski, "Abstracting Control" (LFP 1990)
  — `shift`/`reset` and answer-type modification, which `Cont` and
  `PState` carry in their signatures.
- Atze van der Ploeg and Oleg Kiselyov, "Reflection without Remorse"
  (Haskell Symposium 2014) — the left-nesting cost `resume`'s
  rotation amortises; the type-aligned queue it proposes was measured
  unnecessary for the Scala 3 core, and the same rotation is here.
- Eric Torreborre, "eff" for Scala 2 (atnos-eff) — the prior art for
  a kind-`*` row of effects with membership as implicits in Scala 2;
  `okay2` differs in holding the operation raw (no tagged union at
  run time) and dispatching by class.
- docs/theory/ and specs/freer-base.md, specs/scala2-facade.md — the
  Scala 3 core's own account of the same machine, and the facade this
  module is the other road beside.
