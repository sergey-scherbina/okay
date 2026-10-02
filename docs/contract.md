# What okay is: the contract in three parts

Most of this repository is libraries built ON okay: HTTP, SQL, streams,
agents, UI. This page is about what is underneath all of them, the part
a user's code actually depends on. It has three parts, and the list is
complete:

1. **`Effects[M]`**: the kernel. A program is a value of `A ! F`, and
   everything monadic is said in its words.
2. **The static ladder, `Applicative` and `Selective`**: what a monad
   cannot promise.
3. **The vocabulary**: the ready effects, the rows they combine into,
   and the handlers that discharge them.

Everything else is syntax over these (`direct` blocks), doors into them
(the interop modules), or a library written with them.

## 1. The kernel: `Effects[M]`

`A ! F` is a program that answers an `A` and may perform the operations
of `F`. `F` is a ROW: a union of effects, `State % Int + Throws % String`.
A program with nothing left in its row is `A ! Pure`. The operations on
such programs form one final-tagless interface (`src/main/scala/Effects.scala`):

```scala
trait Effects[M[_[+_], _]]:
  def pure[F[+_], A](a: A): M[F, A]
  def perform[F[+_], A](e: F[A]): M[F, A]
  ...
  extension [F[+_], A](m: M[F, A])
    def flatMap[B](f: A => M[F, B]): M[F, B]
    ...
    def foldCont[S](h: F !> S): A /> S
  ...
  def run[A](m: M[Pure, A]): A = reify[M, Pure, A](m)(using this).run
```

`flatMap` and `foldCont` are extension methods, so their first argument is
the program itself, `m: M[F, A]`: `flatMap` is `M[F, A] => (A => M[F, B]) =>
M[F, B]`, and `foldCont` is `M[F, A] => (F !> S) => A /> S`.

Read by what each part gives:

| words | what they give |
|---|---|
| `pure`, `perform`, `flatMap` | a monad for EVERY row `F`; an operation is a program |
| `handle` | the one thing a plain monad lacks: take an effect OFF the row, `M[E + F, A]` to `M[F, B]`. Programs stay open, and the caller chooses what each effect means |
| `shift`, `shift0`, `reset` | delimited control as an effect in the same row, so generators, early exit, backtracking and coroutines are ordinary programs |
| `foldCont` | the meaning: a program's image in `Cont`, where a handler IS a continuation |
| `defer`, `tailcall` | stack safety for any bind shape, mutual recursion included |
| `run` | a program with an empty row, to its value |

### `foldCont`, and what to do with its result

`p.foldCont(h)` is `foldMap` into `Cont`. It folds the program's tree and
answers each operation with `h`, a handler written as a continuation
(`F !> S` is `F ==> ([X] =>> X /> S)`). `Static.foldMap` is the same fold
into any `Selective`.

The result, `A /> S`, is `(A => S) => S` held as data: a computation
that has done everything except decide what happens to its final
answer. You finish it by giving it that last continuation with `/`. What
you get is an `S`, and `S` is your choice. It decides what the
interpretation PRODUCES. One program, three answers
(`src/test/scala/TestFoldCont.scala`):

```scala
enum Op[+A]:
  case Pick(xs: List[Int]) extends Op[Int]

val pair: (Int, Int) ! Op = for
  a <- effect(Op.Pick(List(1, 2)))
  b <- effect(Op.Pick(List(10, 20)))
yield (a, b)
```

**`S` is the answer.** The handler resumes `k` once, and `/ identity`
closes it. That is exactly what `runWith` is:

```scala
val first: Op !> (Int, Int) = [X] => (e: Op[X]) => e match
  case Op.Pick(xs) => Cont.shift[X, (Int, Int), (Int, Int)](k => k(xs.head))

assertEquals(pair.foldCont(first) / identity, (1, 10))
```

**`S` is every answer.** The handler resumes `k` once per choice and
concatenates. The last continuation wraps the one final answer:

```scala
val all: Op !> List[(Int, Int)] = [X] => (e: Op[X]) => e match
  case Op.Pick(xs) => Cont.shift[X, List[(Int, Int)], List[(Int, Int)]](k => xs.flatMap(k))

assertEquals(pair.foldCont(all) / (p => List(p)), List((1, 10), (1, 20), (2, 10), (2, 20)))
```

**`S` is a function of the state.** Each operation receives the current
cell and passes the next one on. The last continuation pairs the answer
with the final cell, and the result is applied to the starting state:

```scala
type St = Int => (Int, Int)
val counter: Int ! State % Int = for
  n <- State.get[Int]
  _ <- State.set(n + 1)
  m <- State.get[Int]
yield n * 100 + m
val cell: (State % Int) !> St = [X] => (e: State[Int, X]) => e match
  case State.Get() => Cont.shift[X, St, St](k => s => k(s)(s))
  case State.Update(f) => Cont.shift[X, St, St](k => s => k(f(s)._1)(f(s)._2))

assertEquals((counter.foldCont(cell) / (a => s => (s, a)))(5), (6, 506))
```

The fourth choice is the one `handle` makes: `S` is another PROGRAM,
`M[G, B]`. The handled effect is answered, the rest of the row is passed
on as operations, and `/ ret` gives the program that remains. So
`foldCont` is the one primitive under all of them. `runWith`, `handle`
and every ready handler are choices of `S` and of the last continuation.

One more interpretation has its own name. With a monad `G`, each
operation is translated by a natural transformation into `G`. That is
`foldMap`, the fold `Static` and `Proc` have too:

```scala
extension [F[+_], A](m: M[F, A])
  def foldMap[G[_]](nt: F ==> G)(using G: Monad[G], R: TailRecM[G]): G[A] =
    ...
```

Folding a program into cats-effect's `IO` (okay-cats, `TestCatsClasses`):

```scala
val p: Int ! Ask = for
  a <- okay.effect(Ask.Num("a"))
  b <- okay.effect(Ask.Num("bb"))
yield a * 10 + b
val toIO: Ask ==> IO = [X] => (e: Ask[X]) => e match
  case Ask.Num(k) => IO(k.length)
assertEquals(p.foldMap(toIO).unsafeRunSync(), 12)
```

It is `G`'s own loop, `TailRecM[G]`, as cats' `Free.foldMap` is. Each
step resumes the program once and answers "continue with the rest" or
"done". So a fold is exactly as stack-safe as the carrier's loop.
Every `TailRecM` in okay is a real loop: a million operations fold
through `Option` on a 128 KB thread and on Scala.js
(specs/eager-carrier-depth.md). A monad with no `TailRecM` cannot be
folded into: that is a compile error, not an overflow.

You call it yourself only to write an interpretation the ready handlers
do not have. To USE an effect, `handle` and `run` are the words.

Two encodings implement it. `Free` is a tree you can step, inspect and
relay. `Eager` binds pure steps as they are built. Code written against
`Effects[M]` runs on either, and programs move between them (`reflect`,
`reify`).

Underneath sit `ParaMonad`, `Control` and `Delimited`: Atkey's
parameterised monad and the delimited-control machine that gives
`foldCont` its meaning. They are the semantics, not the surface. A user
needs them only to write a new kind of handler, never to use one.

Is this enough? For everything MONADIC, yes: by Filinski's theorem every
monad is representable with `shift`/`reset`, and `handle` makes each
effect separately interpretable. It is the same shape as the common core
of the effect libraries beside us. kyo's `A < S` is an effect set with
handlers. A `ZIO[R, E, A]` is a fixed row,
`Reader % ZEnvironment[R] + Throws % E + Async`. A cats `IO` is the row
`Async` alone.

Start with [Effects and continuations: the whole API on one
page](effects-and-continuations.md). It walks these words in the
monadic and the direct style.

## 2. The static half: `Applicative` and `Selective`

A monad's `flatMap` takes an opaque function: nothing about the rest of
the program is known until it runs. Some programs are worth more
BECAUSE they promise less, and that promise cannot be built from
`Effects`. Built through `flatMap`, it is gone. So it is its own
interface (`src/main/scala/Monad.scala`):

```scala
trait Applicative[F[_]] extends Functor[F]:
  def pure[A](a: A): F[A]
  extension [A, B](f: F[A => B])
    def app(a: F[A]): F[B]

trait Selective[F[_]] extends Applicative[F]:
  extension [A, B](fe: F[Either[A, B]])
    def select(f: => F[A => B]): F[B]
```

The first argument of each is the value it extends: `app` is `F[A => B] =>
F[A] => F[B]`, McBride and Paterson's `<*>`, and `select` is `F[Either[A,
B]] => F[A => B] => F[B]`, Mokhov's. The second argument of `select` is by
name: it runs only when the first answers a `Left`.

Three carriers use it:

| carrier | rung | what it can do that `A ! F` cannot |
|---|---|---|
| `Validated[E, A]` | `Selective` | report EVERY error, not the first |
| `Static[F, A]` | `Selective` | list every operation a program may perform before running it |
| `Par[A]` | `Applicative` | run independent leaves at once |

Generic code chooses its rung. `traverse`, `sequence` and `whenS`
take an `Applicative` or a `Selective`, so one program becomes parallel,
accumulating or statically analysable by the instance it is run at.
See [the effects, one page each](effects/throws.md) and
[specs/applicative-static.md](../specs/applicative-static.md).

## 3. The vocabulary: effects, rows, handlers

The kernel says how programs compose. The vocabulary says what they do:

- **The ready effects**: `Reader`, `State`, `Writer`, `Throws`, `Maybe`,
  `Choose`, `Async`, `Resource`, `Once`, `Gen` and the rest, one page
  each under [docs/effects/](effects/reader.md).
- **Rows**: `F + G` (both), `F % S` (an effect at a parameter),
  `Pure` (none). `TypeableK` is how a handler finds its own effect in a
  row.
- **Handlers**: `Handler[F]` answers each operation with a value, and
  `handle` takes an effect off the row. Your own effect is an `enum`
  and a handler, nothing more ([your own effect](your-own-effect.md)).

## What sits on top, and is not the contract

- **Direct style** (`okay-direct`): `direct { val x = p.?; ... }` is
  syntax for `flatMap`. By Filinski's theorem it adds nothing a program
  could not already say.
- **The doors in and out**: `asOkay`, `asIO`, `asZIO`, `asKyo`, and okay's
  classes on the other libraries' types and theirs on ours
  ([effect interop](effect-interop.md),
  [the class ladder](interop-classes.md)). They translate into the
  contract; they do not extend it.
- **Everything else**: streams, fibers, HTTP, SQL, agents, UI, the
  durable log. All of it is libraries written with the three parts
  above.

## Further reading

- Oleg Kiselyov and Hiromi Ishii, *Freer Monads, More Extensible
  Effects*, Haskell Symposium 2015: the row and the freer tree.
- Andrzej Filinski, *Representing Monads*, POPL 1994: why `shift` and
  `reset` are enough for every monad.
- Robert Atkey, *Parameterised Notions of Computation*, JFP 2009: the
  `ParaMonad` underneath.
- Gordon Plotkin and Matija Pretnar, *Handlers of Algebraic Effects*,
  ESOP 2009: `handle`.
- Conor McBride and Ross Paterson, *Applicative Programming with
  Effects*, JFP 2008, and Andrey Mokhov et al., *Selective Applicative
  Functors*, ICFP 2019: the static half.
- [The theory of Okay](theory/index.md) and the
  [Typepedia](typepedia.md), every core type with its meaning.
