# 12. Applicative, Selective, Monad — how much a program says about itself

## The ladder is a ladder of visibility

`Monad.scala` declares four traits in a chain:

```scala
trait Functor[F[_]]                                  // Monad.scala:73
trait Applicative[F[_]] extends Functor[F]           // Monad.scala:80
trait Selective[F[_]] extends Applicative[F]         // Monad.scala:93
trait Monad[F[_]] extends Selective[F]               // Monad.scala:108
```

Read downward, each rung adds power. Read *upward*, each rung adds
something more interesting: **what can be known about a program before
it runs.**

- A `Functor` program is one effect with a pure function after it.
- An `Applicative` program is a pure lambda term whose *arguments* are
  effects — `pure(f) <*> fa <*> fb` (Monad.scala:86). Every effect it
  performs is written in the expression.
- A `Selective` program adds a conditional whose *both* sides are
  written down: `ifS(cond)(t)(e)` (Monad.scala:102). At most one side
  runs; both are visible.
- A `Monad` program gives that up at its first `flatMap`. The
  continuation is a host closure, `A => F[B]`. Nobody can read it. The
  only way to find out what the program does is to do it.

Lindley, Wadler and Yallop \[[2011](#ref-lindley-2011)\] put the
distinction in its sharpest form: *idioms are oblivious, arrows are
meticulous, monads are promiscuous.* An applicative is oblivious to
the values flowing through it — which is exactly why its structure is
static. That obliviousness is not a weakness to be worked around. It
is a property to be *sold*, and this chapter is about what it buys in
Okay: leaves that run at once, effects listed before the run, N
requests collapsed into one, and — as chapter 6 already found from the
other side — a spine the compiler can unfold.

## The applicative *is* the λ-calculus, and for one instance literally

The remark that begins this chapter's story is that an applicative
functor implements the lambda calculus. Stated carelessly it is folk
wisdom; stated carefully it splits into two claims of very different
size, and both are useful.

**The literal claim, for one instance.** Take the applicative of
functions from a fixed environment, `Env => A`:

```scala
pure(a)      =  _ => a                    //  K
f.app(x)     =  e => f(e)(x(e))           //  S
identity     =  e => e                    //  I
```

That is Schönfinkel's and Curry's combinator basis, spelled as an
applicative \[[Schönfinkel 1924](#ref-schonfinkel-1924);
[Curry 1930](#ref-curry-1930)\]. `S` and `K` alone are a complete basis
for the untyped λ-calculus, so this instance really is Turing-complete
in the strict sense.

Okay has this instance twice, under names that do not look like
combinators. `Reader` (Reader.scala:20) is the environment applicative
as an effect: `Reader.ask` (Reader.scala:25) is `I`, `pure` is `K`,
and `app` — the one Free derives for it — is `S`. And Scala 3's own
context functions are the same algebra in the language's syntax:
`A ?=> B` is `Env => A`, and `provide` (Provide.scala:22) is its
runner. Chapter 8's capability style and this chapter's combinators
are one thing seen from two sides.

**The general claim, for every instance.** For an arbitrary `F` the
statement is weaker, and the weakness is the whole point. McBride and
Paterson's idiom bracket \[[2008](#ref-mcbride-2008)\] lifts a pure
lambda term over effectful *arguments*:

```
⟦ f a₁ a₂ … aₙ ⟧  =  pure(f) <*> a₁ <*> a₂ … <*> aₙ
```

The *spine* is the λ-calculus. The *leaves* are the effects. What is
missing — and cannot be added without `flatMap` — is a way to bind one
leaf's answer into another leaf's body. An applicative program cannot
say "fetch this key, and depending on what comes back, fetch that
one".

So the shape of every applicative program is fixed before any of it
runs. Three consequences follow, and Okay now takes all three.

## Consequence one: the leaves may run at once

If no leaf can depend on another's answer, they may run in any order,
or together. That is Marlow, Brandy, Christensen and Epstein's
argument for Haxl \[[2014](#ref-marlow-2014)\] — a paper whose title,
*There is no fork*, names the design: concurrency without a spawn in
the user's program, taken from the applicative structure that was
already there.

`Par` (Par.scala:53) is that reading of `A ! Async`. It is an opaque
carrier over the same programs, whose `app` joins two leaves with
`Async.par` (Par.scala:90):

```scala
extension [A, B](f: Rep[A => B])
  def app(a: Rep[A]): Rep[B] = Async.par(f, a).map((g, x) => g(x))
```

Nothing else changes. Generic code written against `Applicative[F]` —
`traverse`, `sequence`, `replicateA`, `*>`, `<*`, and anything a user
writes — becomes concurrent by *choosing the instance*:

```scala
traverse(keys)(fetch)        // sequential: Free's own Monad-derived app
Par.traverse(keys)(fetch)    // the same program, leaves at once
```

Three details in that instance are worth the space, and each was
decided by something that happened rather than by taste.

**`fmap` does not fork.** `Applicative`'s default `fmap` is
`pure(f).app(a)` (Monad.scala:81) — through this instance that would
fork a fiber to hold a pure function beside the only leaf there is.
The override is `a.map(f)`. It matters because `traverse` calls `fmap`
once per element.

**`Par` is not a `Monad`, deliberately.** A `flatMap` would sequence
the spine while the type still claimed independence, and the
parallelism would vanish exactly where it read most naturally. Haxl
refuses the same instance for the same reason. A program that needs
one answer to build the next is an `A ! Async` and says so in its type.

**`.map` on a `Par` was not always `Par`'s.** Until
comonad-id-map-capture (2026-09-23), `Monad.scala` declared
`given Comonad[Id]` at package level, and through `Functor` it put a
`map` on *every* type in lexical scope. An extension found in
`object Par` loses to a lexical one, because lexical scope beats the
implicit scope of the receiver: a probe wrote `Par(user(id)).map(...)`
the way a reader would, and it type-checked as the identity comonad's
map, returning an `Id` whose next `.app` was "not a member". `Par.map2`,
a plain method that resolves no extension at all, was the way around,
and a test pinned the bad spelling as a compile error so that the day
the footgun was fixed, it would say so. It did: the instance now lives
in `Comonad`'s companion — still found by `summon[Comonad[Id]]`, since
a companion is in the implicit scope of its own type, but no longer in
the lexical scope of a bare value — and the same test now asserts that
`Par(p).map(f)` is `Par`'s.

What it costs is measured, not asserted, and the number is not
flattering in every direction. Eight trivial leaves, JMH, `-f 3`,
three rounds on one box:

| lane | µs/op | B/op |
|---|---|---|
| `bracketPar8` — the idiom bracket at `Par`, seven joins | 52.19–52.40 | 11 401–11 510 |
| `handNested8` — seven `Async.par` calls written out | 52.25–53.10 | 10 503–10 704 |
| `parApplicative8` — `Par.sequence`, i.e. generic `traverse` | 59.12–61.93 | 14 355–14 594 |
| `parAllFlat8` — `parAll`, one fiber per leaf, flat | 9.50–11.75 | 4 195–4 200 |
| `sequential8` — `traverse` at the program's own instance | 0.35–0.42 | 3 840 |

Two readings, and they must not be mixed up — a table of five lanes
answering two questions is how a mismatched pair gets read as a
verdict.

**The wrapper is free.** `bracketPar8` against `handNested8` is the
matched pair: the same seven joins, the same leaves, the same answer,
differing only by the carrier. The ratio is 1.003 and 0.983 across the
two rounds that have both — the carrier is inside the noise, and the
prediction written before measuring (within 10%) is confirmed with
room to spare. The honest residue is in the bytes, which are
load-proof: about +700 to +1 000 B/op, roughly 100–140 bytes per join
for the two closures the instance adds.

**The spine is not free.** Both nested lanes cost about five times
`parAll` — 53.1 against 11.25 in the round where they were measured
together. That is the price of the applicative shape itself, not of
this implementation: `app` is pairwise, so N leaves are N joins and
2N fibers, while `parAll` spawns N and joins them in order. The extra
13% from `bracketPar8` to `parApplicative8` is generic `traverse`
building its `Vector` element by element, which `parAll` also does not
do.

So the rule for a reader: for a flat sequence of same-typed programs
on the JVM, `parAll` remains the right door. `Par` earns its place
where the spine is a genuine lambda term over leaves of different
types, or where the code is generic and has never heard of `Async` —
and there the alternative is not `parAll`, it is running sequentially.

## Consequence two: the effects can be listed before the run

If the structure is static, it can be *read*. Capriotti and Kaposi's
free applicative \[[2014](#ref-capriotti-2014)\] is the type that
makes reading possible: a program as a tree of `Pure` and `Ap` nodes
with operations at the leaves and no function in the spine.

The difficulty is that real programs branch. Mokhov, Lukyanov, Marlow
and Dimino \[[2019](#ref-mokhov-2019)\] found the rung between
applicative and monad that keeps branching visible: `select`, which
runs a scrutinee and then *at most one* of two written-down
alternatives. Their examples are build systems and Haxl's `if` — cases
where "which effects might this need" must be answered without
performing them.

`Static` (Static.scala:42) is the free selective over any Okay
signature:

```scala
enum Static[F[+_], A]:
  case Pure(a: A)
  case Op(fa: F[A])
  case Ap[F[+_], A, B](f: Static[F, A => B], a: Static[F, A]) extends Static[F, B]
  case Select[F[+_], A, B](e: Static[F, Either[A, B]], f: Static[F, A => B]) extends Static[F, B]
```

Three doors come off it. `leaves` (Static.scala:97) answers *what may
this program do*, in program order, before anything runs.
`toFree` (Static.scala:125) turns the spine into an ordinary
`A ! F` so every handler, row and runner in the library applies
unchanged. `foldMap` (Static.scala:159) interprets the spine into any
other `Selective` — which is where consequence three lives.

The approximation is named rather than hidden: `leaves` reports *both*
sides of every `Select`, because which side runs is decided by a value
that does not exist yet. It is an upper bound, exact for a spine with
no `Select` — and an upper bound is what a batcher, a capability list
and a dry run all want. `toFree` is where the difference is made good:
at run time a `Select` performs its scrutinee and then at most one
side. The test that pins this pair is worth reading as the chapter's
one-line summary: a program declares three operations and performs two
(TestStatic, "leaves names both branches before running; toFree runs
at most one").

Why `Static` is not simply four more cases inside `Free` is a design
answer with three parts, and chapter 4 has already supplied two of
them. An `Ap` folds differently from a `Bind` — both sides are
programs, so the walk is a tree and not a list. A fifth case in that
enum re-decides the inlining of every interpreter that matches on it,
and those loops sit at a threshold this repository has measured four
times (chapter 4; specs/core-cleanup.md). And the guarantee `Static`
sells is precisely that **no `Bind` is present**, which a type that has
one cannot make.

What reading costs, and what running one costs, are both measured,
and the pair is the honest case for the type. A thousand leaves, JMH,
`-f 3`, prebuilt against prebuilt:

| lane | µs/op | B/op |
|---|---|---|
| `staticLeaves` — READ the spine, do not run it | 14.45 | 101 048 |
| `monadicPrebuilt` — run the ordinary monadic program | 46.76 | 475 088 |
| `staticToFree` — convert the spine and run it | 80.66 | 843 049 |

Asking what a program will do costs **a third** of doing it. Running
it through `Static` costs **1.72×** the monadic program — a
prediction of 1.3× written before measuring, and refuted. The error
in that prediction is worth naming because it is the kind that sounds
right: it said "each `Ap` becomes a right-nested `Bind`, the shape
`resume` is fastest on", and treated the conversion itself as free.
It is not. `toFree` materialises a second tree, and the residue is
that tree's nodes and closures, about 368 bytes per leaf.

Reading those bytes did find one node that was not earned. The first
version wrapped *both* sides of an `Ap` in a `Delay`; only the left
spine grows under a fold, and the right side is a leaf, which needs no
trampoline. Earning it saved exactly 56 bytes per leaf — one `Delay`
and its thunk — and the 10 000-deep right-nested spine that the
fallback exists for is now a test rather than an assumption.

So `Static` is not a faster way to run a program. It is a way to read
one, and a way to batch one. A program you only want to run should be
written monadic, and the chapter says so in the same place it sells
the type.

## Consequence three: N requests can become one

A batching interpretation is a natural transformation into a carrier
whose `app` *accumulates* its leaves' requests and whose run answers
them together. Nothing in the program changes; the carrier changes.
In Okay that is `foldMap` with a `Selective` instance the consumer
writes:

```scala
final case class Batch[A](keys: Vector[String], run: Map[String, Int] => A)
given Selective[Batch] with
  def pure[A](a: A): Batch[A] = Batch(Vector.empty, _ => a)
  extension [A, B](f: Batch[A => B])
    def app(a: Batch[A]): Batch[B] =
      Batch(f.keys ++ a.keys, m => f.run(m)(a.run(m)))
  ...
```

Fifty leaves, one round trip; the same program run the ordinary way
asks fifty times. Both halves are asserted in the same test, by
counting calls to the store, because "it batches" is a claim about a
number.

That `foldMap` asks for a `Selective` and not an `Applicative` is
forced by `Select`: an applicative carrier has no way to run one side
and not the other. A carrier that wants to run *both* — and a batcher
must, since it fetches what the program *might* ask for — says so by
implementing `select` as Mokhov's `selectA`. That is lawful, and it is
exactly the over-approximation `leaves` reports.

## The same idea, seen from chapter 6

Chapter 6 measured staging: a program written against an abstract
carrier unfolds at compile time, and at the `Func` carrier
(Cont.scala:401) it becomes plain nested closures with no tree at all.
That chapter also recorded the limit honestly — *only static program
structure unfolds; a runtime-`n` loop does not*.

That limit is this chapter's subject under another name. **The
boundary of staging is the boundary between applicative and monad.**
What a partial evaluator can unfold is exactly what an applicative
spine makes visible; what it must leave alone is exactly what
`flatMap`'s opaque continuation hides. The two chapters were measuring
the same wall from opposite sides.

## The optics were already waiting for it

Chapter 10 builds optics on profunctors, and a traversal's constraint
is `Traversing`, whose `traverseOf` asks for an `Applicative[F]` and
nothing more. That slot is not decoration: it is the hole every
carrier of this chapter drops into, with no code in the optics for any
of them.

One optic over every line of an order gives three different things
depending on what fills the slot. At `Validated` the walk reports
every bad line rather than the first. At `Par` the foci are visited at
once and the structure is rebuilt from the answers. At `Static` the
walk is a value before it runs, so `leaves` lists the operations it
would perform — an optic that can be asked what it will do, which
follows from the applicative slot and from nothing else
(TestOpticCarriers).

The boundary is the chapter's boundary, and `okay-ui` states it in its
own words: `Ui.map` rewrites bottom-up, applying its function to the
REBUILT node, and that is not a traversal at all, because applying a
function to a rebuilt node means binding the effect — `F[Ui] >>= f` —
and a traversal has only an applicative. Everything weaker than a
monad fits the slot. Nothing stronger does.

## Where Okay was already doing this without the name

The history is older than the vocabulary, and the library is full of
it.

**Turner's combinators.** David Turner's 1979 implementation technique
\[[Turner 1979](#ref-turner-1979)\] compiles λ-terms into combinators
by *bracket abstraction* and then reduces the graph, with no
environment at run time:

```
[x] x        =  I
[x] y        =  K y                (y ≠ x)
[x] (a b)    =  S ([x] a) ([x] b)
```

plus optimisations — `S (K a) (K b) → K (a b)`, and the `B` and `C`
combinators for the cases where the argument occurs on only one side —
which exist because the naive rule produces enormous terms. Miranda
\[[Turner 1986](#ref-turner-1986)\], the direct ancestor of Haskell,
ran on this technique for a decade.

Okay runs on its modern descendants, in three places:

- an inline program at the `Func` carrier is compiled to closures by
  the Scala inliner — combinator compilation where the reducer is the
  compiler (chapter 6);
- `Fuse` (Fuse.scala) β-reduces optic chains in a macro until no
  combinator survives in the emitted code — Turner's optimisation
  goal, reached by a different route, and measured byte-for-byte
  against the hand-written update (chapter 10);
- `Free.resume`'s rotation, `Bind(Bind(a, f), g) → Bind(a, f(_)
  .flatMap(g))` (Free.scala:138), is the associativity law used as a
  rewrite rule — the `B` combinator's equation, run as a loop.

**The K-versus-S question is a dependency question.** Look again at
the bracket abstraction rule. The choice between `K y` and
`S (…) (…)` is decided by asking *does the bound variable occur free
in this subterm?* That is precisely the analysis behind Haskell's
`ApplicativeDo` \[[Marlow et al. 2016](#ref-marlow-2016)\], which
rewrites independent `do` binds into `<*>` so a carrier like `Par` can
run them together. A 1979 compilation technique and a 2016 desugaring
pass ask the same question of the same syntax — and Okay's `direct`
blocks now ask it too.

`import okay.Direct.parallelBinds.given` turns it on. A maximal run of
two or more consecutive `val x = m.reflect` binds whose right-hand
sides do not mention a name bound earlier in the run is emitted as
spawn-all-then-join-all:

```scala
import okay.Direct.parallelBinds.given
val profile: Profile ! Async = direct:
  val u = fetchUser(id).reflect     // these two do not
  val o = fetchOrders(id).reflect   // mention each other
  Profile(u, o)                     // so they run together
```

The same analysis answers a second question. `direct[F] { ... }` used
to ask its carrier for a `Monad`, which turned away exactly the types
where the sugar reads best: `Validated` refuses a monad on purpose,
because the consistency law would make it stop at the first error. A
block's needs are the block's, though, not the carrier's — a run of
independent binds needs only `Applicative` — so the entry asks for
that and summons the monad only where a bind is actually emitted. At
`Validated` the block becomes the bracket and collects:

```scala
val checked: Checked[Form] = direct:
  val name  = nonEmpty(raw.name)
  val email = looksLikeEmail(raw.email)
  Form(name, email)          // both problems, or a Form
```

**And here the macro does something `Par` cannot.** `app` is pairwise,
so an applicative spine of N leaves is N joins and 2N fibers — the ~5×
measured above. A macro holds the whole GROUP at once, which is the
one position that never has to be pairwise, so it emits the flat
shape instead: N spawns, then N joins in the order written. Measured
at eight leaves, the parallel block is 11.16 µs against `parAll`'s
11.67 — the same shape, within both error bars — while the applicative
spine on the same leaves would have been about five times that. The
tool that writes the code is where the cheap form belongs.

Two things it does not do, both pinned by tests rather than left to
be discovered. Without the import nothing changes at all: the
sequential emission allocates 1 344.002 B/op against master's
1 344.001, identical to the digit. And a bind whose leaf is not an
`Async` program ends the run and stays sequential, which is how a
block over a wider row parallelises its `Async` leaves and nothing
else. That last part took a second lane: the first version decided on
the COMPILED leaf, by which time the row lift had already widened its
type, so the import did nothing at all in a wider row. Reading the
mark's own argument — the program the author actually wrote — is what
fixed it.

## What this does not claim

Two boundaries, stated so that the next reader does not go looking for
machinery that is not there.

Applicative parsers with static first-sets
\[[Swierstra & Duponcheel 1996](#ref-swierstra-1996)\] are the classic
"read the grammar before running it" application, and Okay does not
use them: `okay-parse` is an instruction language over a total builder
(chapter 7's neighbourhood), so the pressure that motivates them is
absent.

And `foldMap` used to be the one door of the free selective that
recursed on the host stack, because the carrier's values must be
combined on the way back up and each level's intermediate type is gone
by then. That is the reassembly other libraries perform with an
internal cast. It can be done without one: a type-aligned list of
what is left to apply carries the alignment in its own constructors,
so `Done` exists only where the answer type is already reached and
consing an argument of type `X` onto a list that finishes an `R`
yields a list that finishes an `X => R`. Matching refines the types
and the walk back up is ordinary code.

The first attempt at it still overflowed at exactly the old depth, and
the reason is worth the sentence: walking down an application's
function side is the obvious axis and the wrong one, because
`traverse`'s fold builds `Ap(Ap(Pure(g), acc), leaf)` and two steps
down reach the pure function, leaving the deep accumulator to be
folded as an ARGUMENT. `Ap(Pure(g), a)` is not an application to walk
past; it is "fold `a`, then map by `g`" — sound exactly because a
`Pure` performs nothing, so running `a` first reorders no effects.
Fifty thousand leaves fold now.

## References

- <a id="ref-schonfinkel-1924"></a>Moses Schönfinkel. *Über die Bausteine der mathematischen Logik.* Mathematische Annalen 92:305–316, 1924.
- <a id="ref-curry-1930"></a>Haskell B. Curry. *Grundlagen der kombinatorischen Logik.* American Journal of Mathematics 52(3):509–536, 1930.
- <a id="ref-turner-1979"></a>David A. Turner. *[A new implementation technique for applicative languages.](https://doi.org/10.1002/spe.4380090105)* Software: Practice and Experience 9(1):31–49, 1979.
- <a id="ref-turner-1986"></a>David A. Turner. *[An overview of Miranda.](https://doi.org/10.1145/15042.15053)* ACM SIGPLAN Notices 21(12):158–166, 1986.
- <a id="ref-mcbride-2008"></a>Conor McBride, Ross Paterson. *[Applicative programming with effects.](https://doi.org/10.1017/S0956796807006326)* Journal of Functional Programming 18(1):1–13, 2008.
- <a id="ref-lindley-2011"></a>Sam Lindley, Philip Wadler, Jeremy Yallop. *[Idioms are oblivious, arrows are meticulous, monads are promiscuous.](https://doi.org/10.1016/j.entcs.2011.02.018)* Electronic Notes in Theoretical Computer Science 229(5):97–117, 2011.
- <a id="ref-capriotti-2014"></a>Paolo Capriotti, Ambrus Kaposi. *[Free applicative functors.](https://doi.org/10.4204/EPTCS.153.2)* MSFP 2014.
- <a id="ref-mokhov-2019"></a>Andrey Mokhov, Georgy Lukyanov, Simon Marlow, Jeremie Dimino. *[Selective applicative functors.](https://doi.org/10.1145/3341694)* ICFP 2019.
- <a id="ref-marlow-2014"></a>Simon Marlow, Louis Brandy, Jonathan Coens, Jon Purdy. *[There is no fork: an abstraction for efficient, concurrent, and concise data access.](https://doi.org/10.1145/2628136.2628144)* ICFP 2014.
- <a id="ref-marlow-2016"></a>Simon Marlow, Simon Peyton Jones, Edward Kmett, Andrey Mokhov. *[Desugaring Haskell's do-notation into applicative operations.](https://doi.org/10.1145/2976002.2976007)* Haskell Symposium 2016.
- <a id="ref-swierstra-1996"></a>S. Doaitse Swierstra, Luc Duponcheel. *[Deterministic, error-correcting combinator parsers.](https://doi.org/10.1007/3-540-61628-4_7)* Advanced Functional Programming, LNCS 1129, 1996.

---

← [6 · Final tagless and staging](06-tagless-staging.md) · [Contents](index.md) · [7 · Logic, streams and sketches](07-logic-streams.md) →
