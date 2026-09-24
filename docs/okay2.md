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
9. [Literature](#9-literature)

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

## 9. Literature

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
