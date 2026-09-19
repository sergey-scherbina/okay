# Arrows: a computation you can see before you run it

A monad hides its shape. `flatMap` takes a function, and what happens
after the first step lives inside a closure that nothing can look at —
which is right when the shape genuinely depends on the answer, and a
loss when it does not.

An arrow keeps the shape outside. `p >>> q` is a value describing two
steps; you can compose it, draw it, count its stages, and only then
run it. That is the whole trade, and it is why arrows sit beside
monads rather than under them.

This page is examples. Every block is run by `TestArrowsGuide` in
okay-lex, so a rename in the library fails the page instead of rotting
it. The theory is [chapter 10](theory/10-optics.md); optics have
[their own page](optics.md).

## The glyphs

`import okay.Optic.arrows.*` — they are the ones every language with
arrows uses, and a plain function is an arrow, so you can try them on
one:

```scala
(inc >>> triple)(4)              // 15   — then
(triple <<< inc)(4)              // 15   — the other way round
(inc *** size)((4, "abc"))       // (5, 3)      a pair, each half its own way
(inc &&& triple)(4)              // (5, 12)     one in, both out
(inc +++ size)(Left(4))          // Left(5)     a sum, each side its own way
(inc ||| size)(Right("abcd"))    // 4           a sum, one answer
```

They are behind an import on purpose: an extension on a bare
`P[A, B]` reaches every two-parameter type in the library, and putting
one of those in package scope is how `import okay.*` once disabled
named tuples for everyone.

**`>>>` composes within ONE carrier.** An effectful function
`A => F[B]` is a different arrow, and it has a different glyph:

```scala
(half >=> dec)(8)   // Some(3)
(half >=> dec)(3)   // None
```

`>=>` is Kleisli composition, `>>>` is arrow composition, and both
names come from the literature rather than from us. They were one
glyph until 2026-09-18 and could not stay that way: an effectful
function is *also* a `P[A, B]`, so one `>>>` shadows the other and the
types stop lining up.

## Streams, where arrows earn their keep

`Scan` is fast and incremental and cannot be composed — its state is a
value it threads by hand. `Mealy` is the same machine written so that
it composes: one input, one output, and the next machine.

```scala
final case class Mealy[-A, +B](run: A => (Mealy[A, B], B))
```

**Two machines over one input, in one pass.** Before `&&&` this could
not be written at all:

```scala
val both = counting &&& isDigit
Mealy.runAll(both, "a1b")
// Vector((1, false), (2, true), (3, false))
```

**A machine then a plain step, in one pass.** Lift the function into
the machine's own arrow — `>>>` will not mix carriers, and the
compiler says so if you forget:

```scala
val M = Mealy.mealyArrow
val labelled = counting >>> M.arr((n: Int) => s"#$n")
Mealy.runAll(labelled, "abc")     // Vector("#1", "#2", "#3")
```

**An arrow is a value, so composing one does not share its state.**
The same machine twice is two machines:

```scala
Mealy.runAll(counting &&& counting, "ab")   // Vector((1, 1), (2, 2))
```

What this costs is stated where it is paid: a step allocates the next
machine, which is exactly the per-character object the lexer's hot
road had removed. So `Mealy` is the composition layer and `Scan.all`
stays the road; `Mealy.ofScan` is the door between them.

## Optics beside arrows

They are neighbours in one table, not rivals. Rivas and Jaskelioff put
monads, applicatives and arrows in one frame — a monad is a monoid in
endofunctors, an arrow a strong monoid in profunctors — and an optic
is not a monoid there at all: it is a Tambara module, an *action*.
This library writes both on the same `Profunctor` to say so.

In practice that means they meet by passing functions, not by
composing with each other:

```scala
val describe = ((l: Line) => l.qty) &&& ((l: Line) => l.sku.length)
eachLine.foldMap((l: Line) => Vector(describe(l)))(order)
// Vector((2, 3), (0, 3))
```

The arrow built the function; the optic walked with it. Neither knows
about the other.

**An optic composes with `andThen`, and there is no `>>>` for it.**
That is deliberate: a second spelling of one idea is what the glyph
policy refuses, and an optic is not an `Arrow` instance anyway, so the
glyph would be a pun.

## Optics and streams do NOT meet, and that is a decision

There is no lens over a `Source` and there will not be one. An optic
is a function from a whole to a whole and needs the whole in hand; a
stream does not give you a whole, it gives you a continuation. A
traversal over a Vector builds another Vector — over an endless stream
there is nothing to build.

The arrow half is the answer for streams, and it is above.

## The applicative slot

The one piece of real magic, and it follows from a signature rather
than from machinery. `traverseOf` asks for an `Applicative` and
nothing else, so whatever you put in that slot decides what the walk
*means*:

```scala
eachLine.traverseOf(check)(order)
// Invalid(Vector("ink: 0"))  — every bad line, not the first
```

Put `Par` in the slot and the foci are visited at once. Put `Static`
in it and the walk becomes a value you can interrogate before running
— `leaves` lists the operations it *would* perform. An optic you can
ask what it will do, from one slot and no special code.

## Continuations: where an optic turns out to be the machinery

A typestate program — one that changes the TYPE of its state as it
runs — is `Cont[X, B => R, A => R]`: it computes an `X` and takes the
state from `A` to `B`. Read that as `P[A, B]` and it is a profunctor
in its own state, and a `Strong` one. So this:

```scala
PState.zoom(item)(parse)   // item: Lens[Box[String], Box[Int], String, Int]
```

is not a special function. It is `l[Zooming[X, R]](m)` — an ORDINARY
optic, run at a carrier that happens to be a continuation. The lens
says which part of the state; the answer-type indices line up on their
own.

**And then it stops, in a way worth knowing.** Composition is
writable: two typestate programs run in order, threading `A -> B -> C`.
The identity is not:

```scala
// exists: hand it the answer
def idGiven[X, R, A](x: X): Zooming[X, R][A, A] = shift(k => a => k(x)(a))

// does not: where would the X come from?
def id[X, R, A]: Zooming[X, R][A, A] = shift(k => a => k(???)(a))
```

The identity must compute an `X` while leaving the state alone, and
`X` is universally quantified — only the inner program can make one.
A category without an identity is a **semigroupoid**, and that is
exactly what this carrier is.

`Choice` fails for the SAME reason one step along: on the case that is
not there, `right` must still answer the program's `X` and has no
program to get it from. So a prism cannot zoom a typestate program
without changing what it answers, and the door that exists says so in
its type — `PState.zoomCase` answers `Option[X]`.

**One cause, two refusals**, and that is the interesting part rather
than either refusal alone:

| | |
|---|---|
| `Strong` | yes — every lens zooms a program |
| composition | yes — programs sequence |
| identity | **no** — needs an `X` from nowhere |
| `Choice` | **no** — same |

`Strong` survives precisely because `first` and `lens` NEVER invent an
`X`: they always run the inner program. The moment an operation must
answer without running anything, it becomes impossible. That is
parametricity — the type says an answer belongs to whoever computed
it — and no cast buys it. `TestContSemigroupoid` runs the three
claims.

## Direct style

Optics work inside a `direct` block: reads, writes, composed lenses,
traversals, and both arrows. And a block with an optic in it stays
**applicative** — two `reflect`s with a lens between them still
collect every error rather than stopping at the first.

The one refusal: a `.reflect` inside the focus function does not
compile. A mark under a lambda is a corner `direct`'s v1 declines by
design, so an optic and a `reflect` meet at the call rather than
inside the focus.

Non-direct code gets something direct style cannot: `Static`. Inside a
block you are already running; the value that says what a walk *would*
do only exists outside one.

## Where to go next

- [optics](optics.md) — the paths, with the code they replace beside
  them and what each costs
- [theory ch. 10](theory/10-optics.md) — profunctors, Tambara modules,
  and the table arrows and optics share
- `TestArrowsGuide` (okay-lex) — this page, executed
- `TestArrowGlyphs` — every glyph against the named method it spells
