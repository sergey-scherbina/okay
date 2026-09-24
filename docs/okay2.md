# okay2 — the core, written a second time in Scala 2.13

`okay-scala2` (docs/scala2.md) is a FACADE: Scala 3 code that wraps
the library so a 2.13 compiler can read its signatures. `okay2` is the
other road: the core itself — the freer tree, its rotation, the row,
the handlers, `Cont` — written again in Scala 2.13, with nothing of
Scala 3 on the classpath. One standard library, no arrangement, no
`-Ytasty-reader`. The design turned out to be a property of the
design and not of the language: the runtime never depended on the
union type, only the spelling of the row did (specs/okay2.md).

TWO ROADS, TWO JOBS (operator, 2026-09-24). `okay2` is okay on pure
Scala 2: for whoever needs exactly that, with no dependency on Scala 3
at all. `okay-scala2` is something else: a door from Scala 2 code into
the Scala 3 world, okay first — the code stays Scala 2 and uses the
Scala 3 libraries. okay (Scala 3) is the more powerful of the two, and
okay2 carries LESS by default: it grows when somebody needs something
specific, not to mirror every file. The vocabulary the three share is
okay's — `State.handle`/`set`, `Writer.collect`, `Throws.runEither`,
`Choose.choose`/`runChoice` — so a program written in it compiles on
okay2 and through the facade alike (docs/scala2.md, section 3a).

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
13. [Resources, once, delimited control, capabilities](#13-resources-once-delimited-control-capabilities)
14. [Generators](#14-generators)
15. [Literature](#15-literature)

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
several is written with `+`, and `+` is `with`: a row is the
INTERSECTION of what the program requires, and `Free` is contravariant
in it. So the order a row is written in does not matter, a program
needing less is already a program in a row that needs more, and a
handler finds its effect anywhere in the row by itself.

```scala
    val p: Int ! (State[Int]) =
      for {
        _ <- State.modify[Int](_ + 1)
        _ <- State.modify[Int](_ * 10)
        n <- State.get[Int]
      } yield n
    assertEquals(State.run[Int, Int](4)(p), (50, 50))
```

A program is built at its own signature's row and is ALREADY a program
in any wider one — widening is subtyping. `at` names the row the call
site wants and is the identity:

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

The order of a row is not part of its type, and a program with an
effect left unhandled is refused by `run`:

```scala
    implicitly[(Int ! (State[Int] + Writer[String])) <:< (Int ! (Writer[String] + State[Int]))]
    implicitly[(Int ! (Writer[String] + State[Int])) <:< (Int ! (State[Int] + Writer[String]))]
```

```scala
    val errors = compileErrors("!.run(State.handle(1)(prog))")
    assert(errors.contains("type mismatch"), errors)
```

`bind` infers the union when the continuation answers in another
row (a plain `flatMap` does too), and `plus` adds a signature:

```scala
    val p: Int ! (State[Int] + Produce) = State.get[Int].bind(n => produce(n + 1))
    assertEquals(State.handle(1)(p).runWith, (1, 2))
```

### Why the type is called `Row`

`Row` is the name of the one trait every signature extends, and of the
bound on a program's row parameter (`R <: Row`). The word is the
standard one. A ROW is a finite collection of labels with, optionally,
a variable standing for "whatever else": Wand introduced row variables
for records \[Wand 1987\], Rémy gave them the ML type system they still
have \[Rémy 1989\], and Leijen carried them over to effects. In Koka a
function's type lists the effects it may perform as an *effect row*,
`<state<int>, console | e>`, where `e` is the rest \[Leijen 2014,
2017\]. Links and Frank type effect handlers with rows too
\[Hillerström & Lindley 2016; Lindley, McBride & McLaughlin 2017\].

okay2 reads the same way. `State[Int] + Writer[String]` is a row of two
labels, and a handler's `R` is the row variable, the "`| e`":
`State.handle` takes a program in `State[S] with R`, handles the
`State[S]` label, and hands back a program in `R`, the rest. `Pure` is
the empty row, and it is `Row` itself.

One difference from those languages is in the encoding, not the idea.
Koka and Links build rows into the type system; Scala 2 has no rows, so
okay2 spells one as an INTERSECTION of requirements, which is how
Brachthäuser, Schuster and Ostermann read effects (*effects as
capabilities*, 2020): a program that may perform the operations of F
or G is one that requires a handler for F AND one for G. The Scala 3
core spells the same row as a union of operations, `F + G = [A] =>>
F[A] | G[A]`, and its row parameter is a type constructor, `F[+_]`,
with no supertype to bound it. There `okay.Row` is the object of row
MEMBERSHIP — `Row.at`, `Row.In`, `Row.Sub`, `Row.Has` (renamed from
`RowLift` on 2026-09-24 so that both cores say `Row`); here `Row` is
the row's own type, needed because a Scala 2 row has to be a type of
kind `*` (section 8).

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
that is what a row split tests at run time. Give it a SIGNATURE, never
a row: scalac 2 resolves an intersection's `#Op` to its last parent's
(`(Writer[String] + State[Int])#Op` is `State.Op`), so a row's ClassTag
would test for one class only. Nothing asks for one — a row has no
`TypeableK` — and for the same reason okay2 never reads an operation at
a row's `#Op`: `Inject` holds it as `Any`, and the typed view comes
from `Split`, at one signature, after its class test.

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

A row may not hold two signatures of ONE class. A split tells
signatures apart by their class, so in `Ask[Int] + Ask[String]` the
String ask would reach the Int handler and die of a ClassCastException
at the first wrong answer. `Distinct[F + G]` refuses such a row at
compile time, and everything that splits a parameterised signature out
of a row asks for it: `Handler.union`, `Into.union`, `IntoZ.union`, the
handlers of `State`, `Reader`, `Writer` and `Throws` (with
`recover`/`orElse`), the kernels `relay`, `translate`, `interpret` and
`handle`, and `toFs2`/`toZStream`. A signature with no type parameter —
`Delim`, `Once`, `Resource`, `Choose`, `Async` — cannot occur twice
with different types, so its handlers need no check. The same part
written twice, distinct classes and an abstract part in generic code
all pass:

```scala
    val _ = implicitly[Distinct[State[Int] + Writer[String] + Reader[Int]]]
    val _ = implicitly[Distinct[State[Int] + State[Int]]]
```

Where a test is finer than its class, `Distinct.unchecked` says so.

### The simple form, as okay-scala2 writes it

Optional, beside the form above: `okay2.simple` has the facade's three
names, so a file written for `okay-scala2` compiles here with its
imports changed — `import okay2._` and
`import okay2.simple.{Effect, Handler, Op}` (selective, because okay2
has its own `Effect` and `Handler`, and an explicit import outranks the
wildcard). The operations extend `Op`, the object IS the effect, and
one handler shape does everything — each operation with the rest of the
program as a function:

```scala
  def console[R, B](out: ListBuffer[String], input: String): Handler[Console, R, B] =
    new Handler[Console, R, B] {
      def apply[X](op: Console[X], k: X => B ! R): B ! R = op match {
        case PrintLn(s) => out += s; k(())
        case ReadLn => k(input)
      }
    }
```

```scala
    val prog: String ! (Effect[Console] + State[Int]) = for {
      name <- Console.send(ReadLn)
      _ <- State.set(name.length)
      _ <- Console.send(PrintLn("hi " + name))
    } yield name
    val out = ListBuffer.empty[String]
    val handled = Console.handle(prog)(a => pure(a))(console(out, "ada"))
    assertEquals(!.run(State.handle(0)(handled)), (3, "ada"))
```

That handler resumes once here; it may resume many times or never, so
it is okay's `Interpr` with the `shift` done for you. The suite is the
facade's own `TestOwnEffectFromScala2`, copied with only its package and
imports changed.

## 4. Handlers, in any order

`State.handle(s)(p)` runs the State part of `p` and leaves a program
over the rest of the row: its parameter is `Free[State[S] with R, A]`,
and scalac infers `R` from the intersection, wherever State stands in
it. Over three effects, all six orders:

```scala
  type Row3 = State[Int] + Writer[String] + Reader[Int]
```

```scala
      !.run(Writer.run(Reader.run(5)(State.handle(1)(prog)))),
      !.run(Reader.run(5)(Writer.run(State.handle(1)(prog)))),
      !.run(State.handle(1)(Reader.run(5)(Writer.run(prog)))),
      !.run(Reader.run(5)(State.handle(1)(Writer.run(prog)))),
      !.run(State.handle(1)(Writer.run(Reader.run(5)(prog)))),
      !.run(Writer.run(State.handle(1)(Reader.run(5)(prog)))))
```

A helper of your own that is polymorphic in the rest of the row spells
its PARAMETER with `Free` and `with`: scalac 2 does not look through an
alias (`!`, `+`) to solve a row variable, and through them `R` would
come out as the whole row. Inside it, name the rest for the handler:

```scala
    def countFrom[R <: okay2.Row, A](p: Free[State[Int] with R, A]): (Int, A) ! R = State.handle[Int, A, R](0)(p)
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
    assertEquals(Throws.runEither(p).runWith, Right(1))
    val q: Int ! Row = Throws.raise[String, Int]("x").at[Row].orElse(pure(9))
    assertEquals(Throws.runEither(q).runWith, Right(9))
```

An abort does not run what follows it, and a `Writer` beside it sees
exactly what happened before:

```scala
    type Row = Throws[String] + Writer[String]
    val p: Int ! Row = Writer.tell("before").at[Row].flatMap(_ => Throws.raise[String, Int]("stop").at[Row]).flatMap(x => Writer.tell("after").at[Row].map(_ => x))
    val (ws, r) = !.run(Writer.run(Throws.runEither(p).plus[Pure]))
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
    val (ws, a) = !.run(Writer.run(told))
    assertEquals(a, 42)
    assertEquals(ws, Seq("asked", "asked"))
```

A Cont-valued handler (`handle`) may abort or resume the rest of the
program more than once:

```scala
    assertEquals(both.runWith, List(11, 11))
```

### The four handler shapes, spelled as okay spells them

okay2 has okay's four ways to give an effect its meaning, under okay's
names and in okay's argument order. The one thing Scala 2 cannot write
is a polymorphic function literal (`[X] => (e: F[X]) => …`), so where
okay passes one, okay2 passes an anonymous class with the same method:

| okay (Scala 3) | okay2 (Scala 2) | what it may do |
|---|---|---|
| `new Handler[F] { def handle[A](a: F[A]): A }` | `new Handler[F] { def handle[A](a: F.Op[A]): A }` | answer with a value |
| `!.relay(p)(ret)([X, Y] => e => …)` | `!.relay(p)(ret)(new Relay[F] { def apply[X, Y](e) = … })` | resume exactly once |
| `Effects[Free].handle[F, G](p)(ret)([X] => e => shift(…))` | `!.handle[F, G](p)(ret)(new Interpr[F, S] { def apply[X](e) = shift(…) })` | abort, resume many times |
| `!.translate(p)([X] => e => …)` | `!.translate(p)(new Interpret[F, G] { def apply[X](e) = … })` | answer with more program |

```scala
      !.handle[Throws[String], Produce](calc(b))(a => pure(a))(new Interpr[Throws[String], Int ! Produce] {
        def apply[X](e: Throws.Op[String, X]): Cont[X, Int ! Produce, Int ! Produce] =
          shift[X, Int ! Produce, Int ! Produce](_ => pure(-1))
      }).runWith
```

```scala
    val handled: Int ! Produce = !.relay[Int, Int, Op, Produce](prog)(a => pure(a))(new Relay[Op] {
      def apply[X, Y](o: Op.Val[X]): X /> Y = Cont.Pure(o.a)
    })
```

`!.handle[F, G]` names the effect and the rest first and infers the
program and the answer, as okay's two type-parameter clauses do: its
first half answers a value class whose `apply` takes the rest, so
nothing is allocated for the split.

## 8. What is different from Scala 3, and why

Each of these was measured before it was decided (specs/okay2.md):

- **A row is a type of kind `*`** with a member `type Op[+A]`, and
  `F + G` is `F with G`. Scala 2 cannot give a type alias the kind
  `* -> *` by partial application, so the Scala 3 union
  `[A] =>> F[A] | G[A]` has no spelling. Its dual has one: the union of
  a program's OPERATIONS is the intersection of its REQUIREMENTS, and
  `Free` is contravariant in the row. An operation of a row of several
  is held raw in the tree (TestRow checks the class).
- **Rows commute and widen by subtyping** (stage 8). Until then `+` was
  a sealed trait that did not commute, and the row layer carried six
  membership rules, a `Remove` witness per handler and an `…At` twin of
  each; all gone. `at` is kept as the identity so a call site can name
  its row.
- **Never read an operation at a row's `#Op`**: an intersection's is its
  last parent's, and reading at it is a `ClassCastException` (measured).
  `Inject` holds the operation as `Any`; `Handler.Of[F]` and
  `Into.Of[F, M]` are the typed forms, for ONE signature.
- **Row-generic parameters are spelled with `Free`**, not `!`/`+`:
  scalac 2 does not look through an alias to solve a row variable.
- **Union handlers and `Into`s are explicit** (`Handler.union`,
  `Into.union`): an implicit rule over `F + G` matches every type and
  diverges. For the same reason `Replayable` — the one inductive check
  over a row — is derived by a small macro that reads the row's parts.
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
    val intoOption: Into[Produce, Option] = new Into.Of[Produce, Option] {
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
    val s: Stream[IO, String] = toFs2[IO, String, Unit, Io](p)
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
    val collected: IO[(Seq[Int], Unit)] = Io.run(Writer.run(p))
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
    val s: ZStream[Any, Throwable, String] = toZStream[Any, Throwable, String, Unit, Zio](p)
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

    val (out, answer) = Effects.run(Writer.run(into(told(1, 2, 3, 4, 5, 6))(evens)))
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

## 13. Resources, once, delimited control, capabilities

**Resource** ties release to the SCOPE: acquire inside it, and the
scope releases at its end in reverse order, whatever else the program
does — at its value, at a throw, or when a residual that forwarded
other effects completes:

```scala
    def res(n: String) = Resource.acquire { log ::= s"open $n"; n } (r => log ::= s"close $r")
    val prog = res("a").flatMap(a => res("b").map(b => a + b))
    assertEquals(Resource.scoped(prog), "ab")
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
```

```scala
    type F = Resource + Later
    val prog: Int ! F =
      Resource.acquire(())(_ => released = true).at[F].flatMap(_ => later(41).at[F].map(_ + 1))
    val residual: Int ! Later = Resource.run(prog)
    assertEquals(released, false)
    assertEquals(residual.runWith, 42)
    assertEquals(released, true)
```

`run` asks the residual row how a forwarded operation reports failure
(`Failing`): the core answers for `Pure`, `import okay2.async._`
answers for any row with `Async` in it, and a row with no answer is a
compile error rather than a scope that silently leaks. An `Async.Run`
that throws on the outer handler still releases:

```scala
    type F = Resource + Async
    val prog = Resource.acquire { log ::= "open"; "r" } (_ => log ::= "close").at[F]
      .flatMap(_ => Async[Int] { log ::= "run"; throw new RuntimeException("boom") }.at[F])
    val out = outcome(Async.runAsync(Resource.run(prog)))
    assert(out.exists(_.isFailure), s"expected the failure, got $out")
    assertEquals(log.reverse, List("open", "run", "close"))
```

**Once** is call-by-need for programs, as an effect: the first demand
runs the program, every later demand of the same value answers from a
cell the handler keeps, and the tree holds no cell, so the same program
run twice replays:

```scala
    val q: Int ! R = !.once[Int, W](Free.delay(() => { hits += 1; pure[R, Int](hits * 10) }))
    val prog: Int ! R = q.flatMap(a => q.flatMap(b => q.map(c => a + b + c)))
    assertEquals(logged(prog), (Seq(), 30))
    assertEquals(hits, 1)
```

**Delim** is multi-prompt delimited control as an effect: a prompt is
a typed tag, `push` installs it, `shift` captures up to the NAMED
prompt and hands the rest of the program over as a value — to invoke
twice, to drop, to keep:

```scala
    val r = !.run(Delim.reset[Int, P] { p =>
      Delim.shift[Int, Int, P](p) { k =>
        k(1).flatMap(a => k(2).map(b => a + b))
      }.map(_ * 10)
    })
    assertEquals(r, 30)
```

```scala
    val prog: Int ! Row =
      push[Int, P](outer) {
        push[Int, P](inner) {
          Delim.shift[Int, Int, P](outer)(_ => pure[Row, Int](99))
        }.map { x => innerFinished = true; x + 1 }
      }.map(_ + 1000)
```

The typed door is EVIDENCE rather than a prompt: a `Prompted.Aux[R, F]`
can only be held inside the `scope`/`delimited` that installed the
delimiter, it carries the row, and a function that captures is written
apart from any prompt and runs only where a delimiter is in force.
Scala 2 has no context functions, so the evidence is passed first:

```scala
    def banner(in: Delim.Prompted.Aux[Int, W]): Int ! (Delim + W) =
      Writer.tell("hello").at[Delim + W].flatMap(_ => Delim.shift[Int, Int](in)(k => k(5)).map(_ + 1))
    assertEquals(!.run(Writer.run(Delim.delimited[Int, W](banner))), (Seq("hello"), 6))
```

The four patterns are names over that door. `collect`/`emit` reads a
push producer as a pull — the walk stays a walk:

```scala
  def walk(t: Tree)(e: Delim.Emitting.Aux[Int, P]): Unit ! R = t match {
    case Leaf(a) => Delim.emit(e)(a)
    case Node(l, r) => walk(l)(e).flatMap(_ => walk(r)(e))
  }
```

```scala
    assertEquals(!.run(Delim.collect[Int, P](walk(t))), List(1, 2, 3))
```

`pause`/`resumable` stops in the middle and hands the rest back as a
value; `replay` re-derives where a dialogue stands from its journal,
exactly when the row is `Replayable` (a `Writer` in it is refused at
compile time):

```scala
  def booking(s: Delim.Asking.Aux[String, String, String, P]): String ! R = for {
    city <- Delim.pause(s)("Which city?")
    nights <- Delim.pause(s)(s"How many nights in $city?")
    pay <- Delim.pause(s)(s"Pay ${nights.toInt * 90} for $city?")
  } yield if (pay == "yes") s"Booked $city for $nights nights" else "Cancelled"
```

```scala
    val start = !.run(Delim.resumable[String, String, String, P](booking))
    assertEquals(start.asking, Some("Which city?"))
```

```scala
    val back = !.run(Delim.replay[String, String, String, P](booking)(j2))
    assertEquals(back.asking, Some("Pay 270 for Kyiv?"))
```

The prompt stack can also be a TYPE. `Delim.Stacked` hands the body a
stack value whose doors ask for evidence that the prompt is on it, so
a shift with no reset, a shift to a foreign prompt of the same answer
type, and a shift to a prompt whose reset has returned are compile
errors rather than a run-time `NoPrompt`:

```scala
    val r = !.run(Stacked.delimited[Int, P] { outer =>
      outer.stack.reset[Int, P] { inner =>
        // the outer prompt is second on the inner stack: `there` finds it below `here`
        inner.stack.shift[Int, Int, P](outer.p)(k => k(1).map(_ + 100))
      }.map(_ + 1)
    })
    // k is the rest up to OUTER: (_ + 1) is inside k, (+ 100) is outside
    assertEquals(r, 102)
```

**Choose** is nondeterminism: the handler is multi-shot, resuming the
rest of the program once per alternative, and `Logic` is LogicT on
top of it — `msplit`, the cut, the soft cut, the fair `interleave`
that lets two infinite branches take turns:

```scala
    val prog: Int ! Choose =
      choose(1, 2, 3).flatMap(x => choose(10, 20).map(x * _))
    assertEquals(!.run(runChoice(prog)), Seq(10, 20, 20, 40, 30, 60))
```

```scala
    val evens = nats.map(_ * 2)
    val odds = nats.map(_ * 2 + 1)
    val six = !.run(observe[Long, Pure](6)(interleave(evens, odds)))
    assertEquals(six, Seq(0L, 1L, 2L, 3L, 4L, 5L))
```

Which reading `Once` gets under a search is handler ORDER: `Once.run`
inside the search backtracks its cells with the branches, outside it
shares one store, and the counts say which:

```scala
    val handled: Seq[Int] ! W = Once.run(runChoice(prog))
    val (log, out) = !.run(Writer.run(handled))
    assertEquals(out, Seq(11, 12))
    assertEquals(hits, 1)
    assertEquals(log, Seq("x"))
```

Across FIBRES the threaded reading runs a shared handle once per
fibre; `SharedOnce` (okay2-async) is one store for all of them, and a
demand met while the program is in flight waits for its answer:

```scala
    val (a, b) = Async.par(store.run(p), store.run(p)).runWith
    assertEquals((a, b), (42, 42))
    assertEquals(runs.get, 1, "the shared store ran the program more than once")
```

**Provide** is the capability pair: `provide` installs values for a
block, `wire[A]` reads one by type, and a missing capability is a
compile error. A body that takes several is curried, which is the
chain `Providing.and` composes:

```scala
    assertEquals(provide(prod, logged)(implicit db => implicit log => app), "info:prod-row")
```

```scala
    assertEquals((withDb and withLog and withClock)(implicit d => implicit l => implicit c => app), "t@7:row")
```

A `Module` is an installer that builds in the `Resource` effect, so
acquiring and releasing in reverse order is the region's obligation;
the right operand of `and` may be built INSIDE the left's context,
which is how the dependency graph is checked by the compiler:

```scala
    val db = module[Db]({ log ::= "open db"; new Db { val name = "db" } })(_ => log ::= "close db")
    val pool = db and { implicit d: Db =>
      module[Pool]({ log ::= s"open pool over ${wire[Db].name}"; new Pool { def borrow() = 7 } })(_ => log ::= "close pool")
    }
    val app: Int ! Resource = pool { _ => implicit p => wire[Pool].borrow() }
    assertEquals(Resource.scoped(app), 7)
    assertEquals(log.reverse, List("open db", "open pool over db", "close pool", "close db"))
```

## 14. Generators

A generator is a program that tells: `Gen[W]` is a `Writer[W]`
program with `Stop` beside it, so a body can end itself from the
middle of a loop. Its combinators are members, so a plain
for-comprehension builds one, and it runs only as far as it is read:

```scala
    val pairs: Gen[(Int, Int)] =
      for {
        x <- Gen(1, 2, 3)
        y <- inner.gen.take(2) if (x + y) % 2 == 0
      } yield (x, y)
    assertEquals(pairs.take(2).toList, List((1, 1), (2, 2)))
```

`iterator` is Python's `next()`: the code between two yields runs when
the second is asked for, not when the first is delivered:

```scala
    val it = c.gen.iterator
    assertEquals(c.steps, 0, "nothing runs at construction")
    assertEquals(it.next(), 1)
    assertEquals(c.steps, 1, "one step: the first yield")
```

A chain of stages is FUSED: the reader applies it per element and
stops the body at exactly the element it needed. The same chain read
as the walks it stands for gives the same answer, and the tests hold
the two equal on generated chains:

```scala
    assertEquals(c.gen.map(_ * 3).filter(_ % 2 == 0).take(2).toList, List(6, 12))
    assertEquals(c.steps, 4, "kept 6 (step 2) and 12 (step 4): four steps, not one more")
```

`zip` pulls one step from each side, which reaches through a fused
`flatMap` with no special case — the hard case of "Stream fusion, to
completeness":

```scala
    val left: Gen[Int] = Gen(1, 2, 3, 4).flatMap(i => Gen.from(Vector.fill(i)(i)))
    val zipped = left.zip(Gen('a', 'b', 'c', 'd', 'e', 'f')).toList
    assertEquals(zipped, List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f'))
```

## 15. Literature

- Oleg Kiselyov and Hiromi Ishii, "Freer Monads, More Extensible
  Effects" (Haskell Symposium 2015) — the tree, the relay handler,
  and why no Functor is needed.
- R. Kent Dybvig, Simon Peyton Jones and Amr Sabry, "A monadic
  framework for delimited continuations" (JFP 2007) — the
  multi-prompt design `Delim` follows: prompts as first-class tags,
  `push` and `shift` as operations of one machine.
- Oleg Kiselyov, Chung-chieh Shan, Daniel Friedman and Amr Sabry,
  "Backtracking, Interleaving, and Terminating Monad Transformers"
  (ICFP 2005) — LogicT, whose `msplit` is `Logic`'s one primitive.
- Christian Queinnec, "Inverting back the inversion of control, or
  Continuations versus page-centric programming" (2003) — the web
  dialogue `Paused`/`resumable` is the type of.
- Oleg Kiselyov, Simon Peyton Jones and Amr Sabry, "Lazy v. Yield:
  Incremental, Linear Pretty-printing" (APLAS 2012), and Roshan James
  and Amr Sabry, "Yield: Mainstream Delimited Continuations" (TPDC
  2011) — yield as the tell a `Gen` is made of.
- Oleg Kiselyov, Aggelos Biboudis, Nick Palladinos and Yannis
  Smaragdakis, "Stream fusion, to completeness" (POPL 2017) — the zip
  through a fused flatMap.
- Olivier Danvy and Andrzej Filinski, "Abstracting Control" (LFP 1990)
  — `shift`/`reset` and answer-type modification, which `Cont` and
  `PState` carry in their signatures.
- Atze van der Ploeg and Oleg Kiselyov, "Reflection without Remorse"
  (Haskell Symposium 2014) — the left-nesting cost `resume`'s
  rotation amortises; the type-aligned queue it proposes was measured
  unnecessary for the Scala 3 core, and the same rotation is here.
- Mitchell Wand, "Complete Type Inference for Simple Objects" (LICS
  1987), and Didier Rémy, "Type Checking Records and Variants in a
  Natural Extension of ML" (POPL 1989) — row variables, the origin of
  the word `Row` (section 2).
- Daan Leijen, "Koka: Programming with Row Polymorphic Effect Types"
  (MSFP 2014) and "Type Directed Compilation of Row-Typed Algebraic
  Effects" (POPL 2017) — the effect row, `<state<int>, console | e>`,
  and its tail variable, which is a handler's `R` here.
- Daniel Hillerström and Sam Lindley, "Liberating Effects with Rows and
  Handlers" (TyDe 2016); Sam Lindley, Conor McBride and Craig
  McLaughlin, "Do Be Do Be Do" (POPL 2017) — rows of effects in Links
  and Frank.
- Jonathan Immanuel Brachthäuser, Philipp Schuster and Klaus Ostermann,
  "Effects as Capabilities: Effect Handlers and Lightweight Effect
  Polymorphism" (OOPSLA 2020) — the reading of a row as the
  capabilities a program REQUIRES, which is why okay2's row is an
  intersection and `Free` contravariant in it.
- Eric Torreborre, "eff" for Scala 2 (atnos-eff) — the prior art for
  a kind-`*` row of effects with membership as implicits in Scala 2;
  `okay2` differs in holding the operation raw (no tagged union at
  run time) and dispatching by class.
- docs/theory/ and specs/freer-base.md, specs/scala2-facade.md — the
  Scala 3 core's own account of the same machine, and the facade this
  module is the other road beside.
