# Optics: naming a path once

An optic is a path into a value, written down as a value itself. You
compose it, you hand it around, and then you read through it, write
through it, walk it with an effect or ask it what it would do.

This page is pairs: the code a person writes today on the left, the
optic that replaces it on the right. Every block below is run by a
test, which asserts the two sides answer the same thing — so if the
library moves, the page fails rather than lying. The pairs are
`TestOpticsGuide`; the program-zooming section at the end is
`TestContProfunctor`, both in `src/test/scala-cross/`. The theory is
[chapter 10](theory/10-optics.md); the design record, with every
refuted alternative, is `specs/optics.md`.

## 1. Set a field one level down

The nested `copy` names the path three times — once to read the
middle, once to rebuild it, once to rebuild the outside:

```scala
t.copy(probe = t.probe.copy(taxon = taxon))       // okay-intent, WordTfIdf.against
```

Named once, as a value:

```scala
val probeTaxon = Lens[Trained](_.probe).andThen(Lens[Probe](_.taxon))
probeTaxon.set(taxon)(t)
```

`Lens[S](_.field)` is a macro that reads the selector and writes
`Lens(get, set)` — the field is ordinary code, so the IDE completes
it, renames it and refuses a wrong name with the compiler's own
error. There is also `Lens.field[Trained]("probe")`, by name, checked
at compile time against the `Mirror`; see the price table below
before reaching for it.

## 2. Read through an absence — and write through it

A read chain and a write chain are two different expressions of one
path. The read is short:

```scala
p.address.map(_.city)
```

The write is not, and it repeats every name the read just used:

```scala
p.copy(address = p.address.map(a => a.copy(city = a.city.capitalize)))
```

One optic is both. A lens composed with `Prism.some` is an *affine* —
zero or one focus — and the type says so without anyone declaring it:

```scala
val city = Lens[Person](_.address).andThen(Prism.some).andThen(Lens[Address](_.city))

city.preview(here)                  // Some("Wrocław")
city.preview(nowhere)               // None
city.modify(_.capitalize)(here)     // the write, same path
city.modify(_.capitalize)(nowhere)  // absent: answers `nowhere`, not a crash
```

The composition took the *intersection* of what a lens needs and what
a prism needs. Nobody wrote a table of family pairs; the type checker
did it.

## 3. Every element

```scala
order.copy(lines = order.lines.map(bump))
```

```scala
val eachLine = Lens[Order](_.lines).andThen(Traversal.each[Line, Line])
eachLine.modify(bump)(order)
```

The `copy` gives you the write and nothing else. The traversal is
also a read:

```scala
eachLine.toVector(order)                    // every line
eachLine.foldMap((l: Line) => l.qty)(order) // their quantities, summed
```

And it is where effects enter — `traverseOf` asks for an
`Applicative` and nothing more, so `Validated` collects every bad
line, `Par` visits them at once, and `Static` says what the walk would
do before it does it. That is [tutorial §23](tutorial.md), with the
worked code in `TestOpticCarriers`.

## 4. One case of a sum, inside a collection

```scala
shapes.map {
  case c: Circle => c.copy(r = c.r * 2)
  case s => s
}
```

The `case s => s` is the part that rots: it is the promise that every
other shape passes through untouched, and it is re-made by hand at
every call site. A prism is that promise, once:

```scala
val radii = Traversal.each[Shape, Shape]
  .andThen(Prism.of[Shape, Circle])
  .andThen(Lens[Circle](_.r))

radii.modify(_ * 2)(shapes)   // circles doubled, squares untouched
radii.toVector(shapes)        // the radii that were there: Vector(1.0, 3.0)
```

## 5. Where an optic does NOT win

Two fields of the *same* sub-record. This is one `copy`:

```scala
t.copy(span = t.span.copy(offset = t.span.offset + delta,   // okay-parse, Parse.rebase
                          line = t.span.line + lineDelta))
```

Through optics it is two chains, one per field. The answers agree —
`TestOpticsGuide` pins that — and the `copy` is still the right code,
for a reason worth stating precisely rather than by feel
([benchmarks §9b](benchmarks.md)):

| lane | ns/op | B/op |
|---|---|---|
| `p.copy(age = n, name = "x")` | 1.48 | 24 |
| `p.copy(age = n).copy(name = "x")` | 3.61 | 24 |
| two fused optic `set`s | 3.94 | 24 |

The *allocation* is the same in all three — the intermediate record
escapes nothing, so the JVM scalar-replaces it. What the second write
costs is work: it reads and writes every field again. So an optic
names one path, and when you are writing two fields of one record at
once, the record's own constructor is the thing that names them
together and does the job in one pass.

Two footnotes, both honest. That gap is a missing rewrite, not a law:
`set ∘ set` into one `copy` is filed as `optic-law-rewrites` with
these numbers attached. And the measurement is a JVM one — Scala
Native and Scala.js have no escape analysis of that quality, so the
allocation column above is not claimed for them.

Two more honest limits. A bottom-up rewrite is not a traversal (it
needs a monad, so it is a fold — `Ui.everywhere` is top-down and says
so). And a "creating" lens that inserts missing parents inside `set`
is not a lens at all until the absence moves into the focus, which is
what `Iso.non(default)` does.

## What it costs

Measured, matched pairs, one run each (`specs/optics.md`, the verdict;
`docs/benchmarks.md` for the lanes). Nanoseconds, lower is better:

| what you write | optic | hand-written | ratio |
|---|---|---|---|
| `o.get(s)`, one field | 0.431 | 0.433 (`p.age`) | 1.00x |
| `o.preview(s)`, an affine | 3.370 | 4.172 (`p.f.map(_.g)`) | 0.81x |
| `o.set(b)(s)`, one field | 1.803 | 1.543 (`copy`) | 1.17x |
| `o.set(b)(s)`, lens ∘ prism ∘ lens | 4.212 | 4.666 (nested `copy`) | 0.90x |
| `Traversal.each.modify` over 1000 | 2063 | 1961 (`Vector.map`) | 1.05x |

**There are two roads, and the difference is when the path is known.**

Name the path in code — a literal optic, an `inline def`, a chain of
them — and the compiler emits the update a person would write. The
allocation is identical to the hand-written nested `copy`, byte for
byte. That is the table above.

Choose the optic at run time — held in a `val` the planner cannot
read, picked by a branch, passed in as a parameter — and the
interpretation runs: one object per level, a `Forget` per read. Still
correct, still composable, no longer free. The same holds for
`Lens.field[S]("name")`, whose expansion the planner cannot read
today.

So: a path you write down is free; a path you choose is a small
interpreter. Both are honest trades, and the only mistake is not
knowing which one you are in.

## The families

| you want | the type | what it needs |
|---|---|---|
| a reversible renaming | `Iso` | nothing |
| a field that is always there | `Lens` | `Strong` |
| one case of a sum | `Prism` | `Choice` |
| a focus that may be absent | `Affine` | both, by composition |
| every element of a shape | `Traversal` | `Traversing` |
| many wholes, one answer | `Kaleidoscope` | `Reflecting` |
| put by an algebra over all wholes | `AlgebraicLens` | `Classifying` |

`Affine` has no constructor of its own in most code: compose a lens
with a prism and the type appears. The last two aggregate rather than
iterate — they are how "decide what this is, given everything seen"
is written — and they live in `TestAggregationOptics`.

## An optic can zoom a PROGRAM, not just a value

Everything above focuses inside a value. The same optic also focuses
inside a *stateful program*, and this is the part with no counterpart
in the hand-written column — there is nothing short to compare it to.

A typestate program `Cont[X, B => R, A => R]` computes an `X` and
moves the state from `A` to `B`. Read as `P[A, B]` that is a
profunctor, so a lens onto part of the state turns a program over the
part into a program over the whole:

```scala
PState.zoom(item)(parse)     // item: Lens[Box[String], Box[Int], String, Int]
```

The program never mentions the whole. The lens says which part, the
compiler carries the state's TYPE change through — `Box[String]`
becomes `Box[Int]` because its item did, and asking for the old type
back does not compile. Any `Strong` optic works here, including an
iso and a composed chain.

**A prism cannot do this, and the reason is worth knowing.** If the
case is not there, the zoomed program must still answer the `X` the
inner program would have produced — and the only thing that can make
an `X` is the program the missing case says not to run. So there is
no lawful instance, and the door that exists says its price in its
type:

```scala
PState.zoomCase(prism)(m)    // answers Option[X]
```

Present case, the program runs and the answer is `Some`. Absent case,
nothing runs at all, the state passes through, and the answer is
`None`.

## Where to go next

- [tutorial §23](tutorial.md) — one optic run at `Validated`, `Par`
  and `Static`, which is optics meeting the effect carriers
- [theory ch. 10](theory/10-optics.md) — profunctors, Tambara
  modules, why `first` is the primitive, and the two places the
  theory said no
- `specs/optics.md` — the staged record: what was built, what was
  measured, what was declined and why (a compiled optic is *slower*
  than the optic; the fusion moved into the compiler)
- `JsonOptic` in okay-codec and `Ui.everywhere`/`key`/`path` in
  okay-ui — the two places this library uses its own optics on a real
  tree
