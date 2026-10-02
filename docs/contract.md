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
def flatMap[B](f: A => M[F, B]): M[F, B]
def foldCont[S](h: F !> S): A /> S
def run[A](m: M[Pure, A]): A = reify[M, Pure, A](m)(using this).run
```

Read by what each part gives:

| words | what they give |
|---|---|
| `pure`, `perform`, `flatMap` | a monad for EVERY row `F`; an operation is a program |
| `handle` | the one thing a plain monad lacks: take an effect OFF the row, `M[E + F, A]` to `M[F, B]`. Programs stay open, and the caller chooses what each effect means |
| `shift`, `shift0`, `reset` | delimited control as an effect in the same row, so generators, early exit, backtracking and coroutines are ordinary programs |
| `foldCont` | the meaning: a program's image in `Cont`, where a handler IS a continuation |
| `defer`, `tailcall` | stack safety for any bind shape, mutual recursion included |
| `run` | a program with an empty row, to its value |

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
def app(a: F[A]): F[B]
trait Selective[F[_]] extends Applicative[F]:
def select(f: => F[A => B]): F[B]
```

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
