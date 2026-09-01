# 1. Monads and functors

## The problem monads solve

A pure function returns a value. A program does more: it may consult
state, fail, emit output, wait for a socket. Eugenio Moggi's insight
[Moggi 1989, 1991] was that "a computation of an `A`" and "a value of
type `A`" are different types, and that the difference itself has
structure: for each *notion of computation* there is a type constructor
`T` such that `T[A]` means "a computation yielding an `A`", together
with two operations —

- `pure: A => T[A]` — the computation that does nothing but yield;
- `flatMap: T[A] => (A => T[B]) => T[B]` — do one computation, feed its
  result to the next

— obeying three laws. Spelled in okay's own notation, with `>>=` for
`flatMap`:

```scala
pure(a) >>= f    ==  f(a)                        // left identity
m >>= pure       ==  m                           // right identity
(m >>= f) >>= g  ==  m >>= (x => f(x) >>= g)     // associativity
```

Each says something operational, not merely algebraic.

**Left identity**: wrapping a value with `pure` and immediately
feeding it onward is the same as just calling the next step. `pure`
adds no effect — if it did (logged something, delayed something), this
law would fail, and so would every refactoring that inlines a
`val x = pure(...)`. It is the law that makes `pure` safe to introduce
and eliminate mechanically.

**Right identity**: finishing a computation by returning its result
unchanged changes nothing. This is why a trailing `.map(identity)` or
`.flatMap(pure)` can always be dropped — and why an interpreter may
resume a bare operation with `Pure(_)` as its continuation without
altering the program's meaning.

**Associativity**: how a *sequence* of steps is parenthesized cannot
matter — `do a; do b; do c` has one meaning whether you group it as
`(a; b); c` or `a; (b; c)`. This is the deepest of the three, because
it is what makes sequential composition *refactorable*: extract three
lines into a helper function and the program is grouped differently
but must behave identically. A structure violating it would make
extracting a method a semantics-changing edit.

Why laws at all, rather than just an interface? Because the laws are
the *contract the abstraction sells*. Generic code — a `traverse`, a
retry loop, okay's own interpreter — manipulates `m >>= f` without
knowing what effect `m` performs, and every transformation it applies
is justified by exactly these equations. Break the laws and generic
code silently miscompiles your effect. That triple obeying those laws
is a **monad**, and Moggi showed that state, exceptions,
nondeterminism, continuations and I/O are all instances. Philip Wadler
then carried the idea into programming practice [Wadler 1992, 1995]:
monads are how a pure language *expresses* effects, and the laws are
what make programs refactorable — associativity is precisely the
statement that how you parenthesize a sequence of steps cannot change
what it does.

A **functor** is the weaker structure underneath: just
`map: T[A] => (A => B) => T[B]`. Every monad is a functor
(`map(f) = flatMap(a => pure(f(a)))`, which is how okay derives it —
`Free.scala:43`), but not conversely; chapter 4 turns on the fact that
okay never *requires* the functor structure of its effect signatures.

## What this is in okay

okay's computation type is spelled as an infix operator:

```scala
infix type ![A, F[+_]] = Free[F, A]              // Effects.scala:40
inline def pure[F[+_], A](a: A): A ! F           // Effects.scala:43
inline def effect[F[+_], A](a: F[A]): A ! F      // Effects.scala:46
```

`A ! F` reads "an `A`, computed with effects `F`". The monad operations
live on `Free` (`Free.scala:41–43`):

```scala
inline def flatMap[B](f: A => Free[F, B]): Free[F, B] = Bind(this, f)
inline def map[B](f: A => B): Free[F, B] = flatMap(a => Pure(f(a)))
```

Note what `flatMap` does: **nothing**. It allocates a `Bind` node. This
is the free-monad move — the subject of chapter 4 — and it is worth
seeing now that it makes the monad laws hold *by construction on the
consumer's side*: `Free.fold` (`Free.scala:51–58`) re-associates
`Bind(Bind(a, f), g)` into `Bind(a, f(_).flatMap(g))` as it walks, so
the associativity law is not a proof obligation on every effect but a
rewrite the one interpreter performs.

The parade of Moggi's examples is okay's module list. State
(`State.scala`), exceptions (`Throws.scala` — "typed aborts"),
nondeterminism (`Choice.scala`, chapter 7), output (`Writer.scala`),
input (`Reader.scala`), and the continuation monad itself
(`Cont.scala`, chapter 2). Each is one *signature* — a small GADT or
identity alias naming its operations — rather than a monad of its own,
because in okay there is exactly one monad (`Free`/`!`) and the
signatures plug into it. Why that factoring is the right one is the
subject of chapters 4 and 5.

## Where the laws earn their keep

The laws are not ceremony; two places in this repository lean on them
directly.

**Rebalancing is associativity.** The `fold` rewrite quoted above is
the third law read left-to-right as a rewrite rule.
The benchmarks page measures what it buys: a 10 000-step left-nested
`flatMap` chain — the worst case, built by `foldLeft` — runs without
stack growth and without the quadratic re-walking naïve free monads
exhibit, because every `fold` pass rotates the tree right as it goes
(`docs/benchmarks.md`, section 1). Chapter 4 gives this problem its
literature: it is Voigtländer's asymptotic-improvement observation,
solved here by normalization-in-the-interpreter rather than by
codensity.

**Identity is `Pure`, literally.** `Bind(Pure(a), f)` reducing to
`f(a)` in the same fold is the left-identity law executed at run time.
Right identity — `m.flatMap(Pure(_)) ≡ m` — is why `Free.Inject(a)`
can be resumed as `h(a)(Pure(_))` (`Free.scala:57`) without changing
the program's meaning.

## Why one monad and many signatures, not many monads

The classical alternative — a monad per effect, composed with monad
transformers — is what the extensible-effects line of work explicitly
set out to replace; the argument is laid out in [Kiselyov, Sabry &
Swords 2013] and taken up in chapter 5. The short version visible
already at this chapter's level: with one monad, `flatMap` is defined
once, the laws are discharged once (in `fold`), and combining effects
is a type-level union rather than a nesting order the programmer must
choose and later refactor. What it costs — a tree node per bind, and
an interpretive dispatch — is exactly what chapters 4 and 6 measure
and then claw back.

## References

- Eugenio Moggi. *Computational lambda-calculus and monads.* LICS 1989.
- Eugenio Moggi. *Notions of computation and monads.* Information and
  Computation 93(1):55–92, 1991.
- Philip Wadler. *The essence of functional programming.* POPL 1992.
- Philip Wadler. *Monads for functional programming.* In Advanced
  Functional Programming, LNCS 925, Springer, 1995.
- Oleg Kiselyov, Amr Sabry, Cameron Swords. *Extensible effects: an
  alternative to monad transformers.* Haskell Symposium 2013.
- Janis Voigtländer. *Asymptotic improvement of computations over free
  monads.* MPC 2008.
