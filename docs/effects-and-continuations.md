# Effects and continuations: the whole API on one page

This page is everything a USER of okay needs: one type, `A ! F`, and six words, `pure`, `perform`, `shift`,
`reset`, `handle` and `run`. They work the same in the monadic and the direct style, and the same through the
`Effects[M]` typeclass, in any encoding.

Writing an effect of your own, or a handler for one, is level 2. That is
[your-own-effect.md](your-own-effect.md), [delimited.md](delimited.md) and the `Cont` chapters of
[theory](theory/02-continuations.md). The machine underneath is level 3 ([cont-stack.md](cont-stack.md)).
Nothing on this page needs either.

## The type

`A ! F` is a program that answers an `A` and may perform the operations of `F`. A row of several effects is a
union, `F + G`: `Int ! State % Int + Throws % String` reads, writes an `Int` cell and may fail with a `String`. A
program with no effect left is `A ! Pure`.

## Effects: perform, handle, run

A ready effect gives its operations as programs (`State.get`, `State.set`, `raise`, `Writer.tell`,
`Reader.ask`, `choose`, `Maybe.none`). An operation value becomes a program with `perform(op)`, or
`op.perform`, which is the same method. Programs compose with `for`:

```scala
val counter: Int ! State % Int =
  for
    n <- State.get[Int]
    _ <- State.set(n + 1)
  yield n * 10

val a = counter.handle(State(5)).run   // (6, 50)
```

`handle` takes ONE effect off the row, with a ready handler that is a plain value. `run` gives the value once no
effect is left. Two effects come off one at a time, in either order, and the order is the meaning:

```scala
val checked: Int ! State % Int + Throws % String =
  for
    n <- State.get[Int].plus[Throws % String]
    r <- (if n > 3 then raise[String, Int]("too big") else pure(n)).plus[State % Int]
  yield r

val b = checked.handle(State(5)).handle(Throws.either).run   // Left("too big")
val c = checked.handle(Throws.either).handle(State(1)).run   // (1, Right(1))
```

The handlers: `State(s)` answers `(S, A)`, `Reader(r)` answers `A`, `Writer.log` answers `(Seq[W], A)`,
`Throws.either` answers `Either[E, A]`, `Choose.all` answers every branch as a `Seq[A]`, and `Maybe.option`
answers `Option[A]`. `Reset[R]` is described below. (`.plus[G]` widens a program into a bigger row; a row is a
union, so its order does not matter.)

## Continuations: shift and reset

`shift` captures "the rest of the program, up to the nearest `reset`" as a function `k`. The body decides
what to do with it: call it once, call it twice, or drop it. It is an effect like any other: `Shift % R` in
the row, where `R` is the answer of its `reset`. And `reset` is its handler, `Reset[R]`:

```scala
val twice: Int ! Shift % Int =
  shift[Int, Int, Pure](k => for a <- k(1); b <- k(10) yield a + b).map(_ * 2)

val d = twice.handle(Reset[Int]).run   // 22: k(1) is 2, k(10) is 20
val e = reset[Int, Pure](twice).run    // 22, the same: reset is a handler
```

`k` returns a program, so the body is written like any other program, and continuations mix with effects with
nothing added. Here the rest of the program, which reads and writes the state, runs twice:

```scala
val both: Int ! Shift % Int + State % Int =
  for
    x <- shift[Int, Int, State % Int](k => for a <- k(1); b <- k(10) yield a + b)
    s <- State.get[Int].plus[Shift % Int]
    _ <- State.set(s + 1).plus[Shift % Int]
  yield x * 2 + s

val f = both.handle(Reset[Int]).handle(State(5)).run   // (7, 33): the rest ran twice, the state through both
```

Two captures:

- `shift` is Danvy and Filinski's. The body runs under its `reset`, so it may capture to it again.
- `shift0` is Materzok and Biernacki's. The body runs outside it, and `k` still re-installs it.

For a body that does not capture again, the two answer the same. `shift0` is the cheaper of the two. Dropping
`k` is an early exit:

```scala
val early: Int ! Shift % Int = shift0[Int, Int, Pure](_ => pure(42)).map(_ + 1)
val g = early.handle(Reset[Int]).run   // 42: the continuation was dropped
```

Each answer type is its own effect, so `reset`s of different answer types nest. A capture goes to the nearest
`reset` OF ITS TYPE, crossing the others:

```scala
val crossing: String ! Shift % Int =
  reset[String, Shift % Int](
    for
      a <- shift0[String, Int, Shift % Int](k => k(2).map(_ + "!"))
      b <- shift0[Int, Int, Shift % String](k => k(a * 10).map(_ + 1))
    yield "x" * b)

val h = crossing.map(_.length).handle(Reset[Int]).run   // 22: the Int capture crossed the String reset
```

The answer type's key is made at compile time. A `reset` or `shift` over an abstract type `R` asks for a
`Shift.Key[R]` parameter, as code over an abstract type asks for a `ClassTag`.

## Direct style

The same programs as plain code, with `okay-direct`'s `direct` block. A program marked with `.?` is its
value. The body of `shift` is a block too:

```scala
val both: Int ! State % Int = reset[Int, State % Int](direct {
  val x = shift[Int, Int, State % Int](k => direct { k(1).? + k(10).? }).?
  x * 2 + State.get[Int].?
})

val i = both.handle(State(5)).run   // (5, 32)
```

With `import scala.language.implicitConversions` the marks go too, and an ascribed program colours to its
value ([direct-style.md](direct-style.md), Layer 3):

```scala
val quiet: Int ! State % Int = reset[Int, State % Int](direct {
  val x: Int = shift[Int, Int, State % Int](k => direct { (k(1): Int) + (k(10): Int) })
  x * 2 + (State.get[Int]: Int)
})

val j = quiet.handle(State(5)).run   // (5, 32)
```

## Through the typeclass

Code that should not commit to one encoding is written over `Effects[M]`. It has the same words: `pure`,
`perform`, `shift`, `shift0`, `reset`, `handle(m, h)` and `run(m)`. `Free`, the tree, is the default.
`Eager` applies pure binds as it builds. One program answers the same in both:

```scala
def program[M[_[+_], _]](using E: Effects[M]): M[State % Int, Int] =
  E.reset[Int, State % Int](
    E.shift[Int, Int, State % Int](k => k(1).flatMap(a => k(10).map(b => a + b))).flatMap(x =>
      E.perform[Shift % Int + State % Int, Int](State.Get[Int, Int]()).map(s => x * 2 + s)))

val inFree = summon[Effects[Free]].run(summon[Effects[Free]].handle(program[Free], State(5)))      // (5, 32)
val inEager = summon[Effects[Eager]].run(summon[Effects[Eager]].handle(program[Eager], State(5)))  // (5, 32)
```

In direct style over `Effects[M]`, `Effects.monad[M, F]` is the monad `direct` needs.

## What it costs

Measured (specs/shift-effect.md, history.d `shift-effect-core`):

- 1000 captures under one `reset` take 56 µs with `shift0`, against 55 µs for the same shape on `Delim`'s
  own doors and 61 µs on level 2's `Cont`.
- `shift` takes 69 µs: it re-installs its `reset` for the body.
- 100 separate small `reset`s take 10 µs: each starts the machine once.

## Where it comes from

- O. Danvy, A. Filinski, *Abstracting Control*, LFP 1990: `shift`/`reset`.
- M. Materzok, D. Biernacki, *A Dynamic Interpretation of the CPS Hierarchy*, APLAS 2012: `shift0`, and the
  `$` operator the machine is built on.
- R. K. Dybvig, S. Peyton Jones, A. Sabry, *A Monadic Framework for Delimited Continuations*, JFP 2007:
  prompts, and the segmented stack.
- Y. Forster, O. Kammar, S. Lindley, M. Pretnar, *On the Expressive Power of User-Defined Effects*, JFP 2019:
  `shift`/`reset` as an effect and its handler, the encoding this page uses.
- G. Plotkin, M. Pretnar, *Handlers of Algebraic Effects*, ESOP 2009: a handler takes an effect off the row.
- O. Kiselyov, H. Ishii, *Freer Monads, More Extensible Effects*, Haskell 2015: `A ! F`'s tree.
- A. Filinski, *Representing Monads*, POPL 1994: why a continuation makes any of this direct style.
