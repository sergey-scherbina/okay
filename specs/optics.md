# Optics on profunctors

## Overview

The operator's direction (2026-09-09): lenses on profunctors. The
repository already holds every part an optic is made of and four
places that reimplement optics by hand. `Monoid` (Fold.scala) and
`Applicative` over `!` (Monad.scala) are the interpretations'
requirements. `Schema` is an optic algebra that has not been named:
`SIso` IS an iso, `SProduct(parts, make)` is a lens per field,
`SSum(caseOf, cases)` a prism per case, `SOption` an affine, `SList`
and `SVector` traversals. `Form.edit` routes dotted string paths over
Json through `Path.parse` and `editAt`; `Ui.foldLocal`, `Ui.submit`,
GTK's `nodeAt` and the Kotlin and Swift clients' `Tree.at` each walk
the tree for "the widget with this key" or "the node at this path";
`State % S` has no way to run a program written against `State % A`
for a part `A` of `S`. Each of those is an optic and a use of one.

The design was settled by two prototypes compiled before this spec
was written (scratchpad `optics/`): the encoding, and the field
selector. What they settled is recorded under Decisions; what they
could not settle — the cost — is a gate, below.

**An optic is a function polymorphic in a profunctor, and its
CONSTRAINT is a type parameter:**

```scala
trait Optic[C[_[_, _]], S, T, A, B]:
  def apply[P[_, _]](p: P[A, B])(using C[P]): P[S, T]
  def andThen[C2[_[_, _]], A2, B2](o: Optic[C2, A, B, A2, B2])
    : Optic[[P[_, _]] =>> C[P] & C2[P], S, T, A2, B2]
```

The families are the constraint: `Iso = Optic[Profunctor, …]`,
`Lens = Optic[Strong, …]`, `Prism = Optic[Choice, …]`,
`Traversal = Optic[Traversing, …]`, and an affine traversal is
`Optic[[P] =>> Strong[P] & Choice[P], …]`. Composition takes the
intersection of the constraints, which is the meet of the lattice, so
`lens andThen prism andThen lens` HAS the affine type without anyone
writing the table of family pairs Monocle keeps — and an
interpretation satisfies the meet by subtyping, because `Traversing`
extends both `Strong` and `Choice`. Verified: the prototype composed
the three and set through the miss unchanged.

**Three interpretations, one of them ours.** `Function1` is
`Traversing`, and gives `modify` and `set`. `Forget[R]` is `Strong`,
and with a `Monoid[R]` is `Traversing`, and gives `get`, `preview`,
`foldMap`, `toVector`. `Star[F]` is `Traversing` for any
`Applicative[F]`, and gives `traverseOf` — and `F = [x] =>> x ! Row`
makes every traversal an EFFECTFUL traversal in the effect row, which
is the thing a library with an effect system can say and Monocle
cannot: `each.traverseOf(a => State.modify(_ + a))`.

**The field selector is code, not a string.** `Lens[Person](_.age)`
is a macro that reads ONLY the selector's tree: the lambda itself is
the getter (no cast anywhere; the focus type comes from the type
checker, not from match types), the setter is built from the Mirror,
anything that is not `_.f` is refused at compile time with a message,
and a wrong field is refused by the type checker itself, with its
"did you mean". `Lens.field[S]("name")` stays beneath it, typed by
the Mirror through match types, for the place where a name IS the
data: the Schema-derived optics of stage 1, where the JSON field name
and the case-class field name are the same string by derivation.

## Interface

```scala
// the lattice (okay.Optic.*)
trait Profunctor[P[_, _]]:  def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]
trait Strong[P[_, _]] extends Profunctor[P]:  def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
trait Choice[P[_, _]] extends Profunctor[P]:  def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]]
type Walk[S, T, A, B] = [F[_]] => Applicative[F] ?=> (A => F[B]) => S => F[T]
trait Traversing[P[_, _]] extends Strong[P] with Choice[P]:  def wander[S, T, A, B](w: Walk[S, T, A, B])(p: P[A, B]): P[S, T]

// the families
type Iso[S, T, A, B]       = Optic[Profunctor, S, T, A, B]
type Lens[S, T, A, B]      = Optic[Strong, S, T, A, B]
type Prism[S, T, A, B]     = Optic[Choice, S, T, A, B]
type Affine[S, T, A, B]    = Optic[[P[_, _]] =>> Strong[P] & Choice[P], S, T, A, B]
type Traversal[S, T, A, B] = Optic[Traversing, S, T, A, B]

// constructors
Iso(to: S => A, from: B => T)
Lens(get: S => A, set: (S, B) => T)
Lens[S](_.field)                       // the selector macro (Focus.scala)
Lens.field[S]("name")                  // by name, typed by the Mirror, no macro
Prism(preview: S => Either[T, A], review: B => T)
Prism.some[A, B];  Prism.of[S, A <: S: ClassTag]   // Option's Some; one case of a hierarchy
Traversal(walk);  Traversal.each[A, B]  // Vector;  Traversal.eachList

// operations, as extensions on any optic whose constraint the interpretation meets
o.modify(f: A => B): S => T    o.set(b: B): S => T
o.get(s): A                    o.preview(s): Option[A]
o.foldMap[R: Monoid](f: A => R)(s): R    o.toVector(s): Vector[A]
o.traverseOf[F[_]: Applicative](f: A => F[B]): S => F[T]
```

## Behavior

Stage 0 — the core (optics-core, LANDED 2026-09-09):
- [x] the lens laws — GetPut, PutGet, PutPut — hold on generated
      values for a `Lens(get, set)`, a `Lens.field[S]("name")` and a
      `Lens[S](_.name)`
- [x] the prism laws — `preview` after `review` is `Some`, `review`
      after a `Some` preview is the identity — for `Prism.some` and
      `Prism.of` over a sealed hierarchy; a miss leaves `modify`'s
      argument unchanged
- [x] the traversal laws — `modify(identity)` is the identity,
      `modify(f andThen g)` is `modify(f) andThen modify(g)` — for
      `Traversal.each`; `toVector` lists the foci in order
- [x] composition across families types itself: lens ∘ prism ∘ lens
      is an `Affine` and sets through a miss unchanged; every pair of
      families is accepted by the `Function1` interpretation, with no
      composition table anywhere
- [x] a traversal run with `Star[[x] =>> x ! State % Int]` visits the
      foci in order and threads the state through them: the
      effectful traversal is the same optic under another
      interpretation
- [x] `Lens[S](_.f)`: the lambda is the getter, the focus type is the
      field's; `_.f + 1` is refused at compile time with the spec's
      message; `_.nosuch` is refused by the type checker
- [x] `Lens.field[S]("nosuch")` does not compile
- [x] the laws run on the JVM, on JS and on Native from one file
      (`src/test/scala-cross`) — the macro included
- [x] an interpretation's direct road agrees with the derived default
      it replaces: `lens`, `prism` and `eachVector` on `Function1`,
      `Forget` and `Star`, against the textbook `dimap(first(p))`,
      `dimap(right(p))` and `wander(vectorWalk)`, on generated values
- [x] THE GATE, measured in `compare` (JMH, per-lane minimum of
      three forks, on a quiet box): `set` through a composed lens
      against a hand-written nested `copy`, and `modify` through
      `Traversal.each` against `Vector.map`. The bar the operator
      accepted is 1.5x. CLEARED for the traversal (1.00x), MARGINAL
      for a one-field lens (1.7x, 1.2 ns absolute) and a fold (1.7x),
      NOT cleared for a composed optic (3.5x) and `Lens.field` (2.4x).
      So stage 2 keeps `Ui.patch`'s navigation; `Ui.path` and
      `Ui.key` are a convenience over it. The numbers, and what the
      first measurement taught, are in Results.

Stage 1 — from the Schema (optics-schema):
- [ ] `Json.field(name)`, `Json.index(i)` and `Json.case(name)` as
      optics over `Json`, composable into the dotted paths `Form.edit`
      routes today
- [ ] the drift law of the second order: for a derived schema, the
      value lens and the Json lens commute with the codec —
      `jsonField(n).set(encode(v))(encode(a)) == encode(field(n).set(v)(a))`
- [ ] a prism per case of a derived sum, on the value (by the Mirror's
      ordinal) and on the Json (`{"Case": {...}}`), commuting likewise
- [ ] `Form.edit` routes through the optic path rather than
      `Path.parse`, and every form test passes unchanged

Stage 2 — the tree (optics-ui):
- [ ] `Ui.key(k)`: an `Affine[Ui, Ui, Ui, Ui]` to the widget with a
      key; `Ui.path(is)`: the node at an index path; `Ui.everywhere`:
      the traversal `Ui.map` is
- [ ] `foldLocal` and `submit` are written with them; the Kotlin and
      Swift `Tree` documents say which optic each function is
- [ ] `Ui.patch` navigates with `Ui.path` IF the gate allowed it; the
      diff-then-patch law is unchanged either way

Stage 3 — the state (optics-state):
- [ ] `zoom(lens)(program: X ! State % A): X ! State % S` — a program
      over a part runs over the whole
- [ ] the type-changing version over `PState`: a `Lens[S1, S2, A1, A2]`
      zooms a typestate program, the four-parameter optic being
      exactly Atkey's parameterised state (theory textbook ch. 3)

## Design

**Why profunctor, not van Laarhoven or concrete pairs.** A concrete
`Lens(get, set)` and `Prism(preview, review)` do not compose across
families without a table; the van Laarhoven `(A => F[B]) => S => F[T]`
composes but cannot express `review` (a prism's constructor) and
needs a functor constraint lattice of its own. The profunctor form
composes by function composition, expresses every family as one
constraint on the profunctor, and — the reason that matters here —
gives the effectful traversal for free through `Star[!]`.

**The constraint as an intersection type.** Purescript spells the
same design with constraint kinds; Scala 3 has no constraint kinds,
but it has intersection types and a class hierarchy, and a given of
type `Traversing[P]` satisfies `Strong[P] & Choice[P]` by subtyping.
The prototype's first attempt, an `Optic` that was a transparent
alias for a polymorphic function value, could not have its constraint
inferred by an extension method (the higher-kinded parameter is not
found in the expanded type); a nominal trait fixes it and costs one
object per optic.

**Tambara.** `Strong.first` is the structure map of a Tambara module;
profunctor optics are the natural transformations between Tambara
modules, and the existential representation
`∃M. (S → M×A) × (M×B → T)` and the profunctor one coincide by Yoneda
(Pickering, Gibbons, Wu, *Profunctor Optics: Modular Data Accessors*,
2017; Boisseau & Gibbons, *What you needa know about Yoneda*, 2018;
Clarke et al., *Profunctor optics, a categorical update*, 2020; Riley,
*Categories of optics*, 2018). The four-parameter optic is
type-changing update, which is what stage 3's zoom over `PState` is:
Atkey's parameterised state and the type-changing lens are one
picture, and chapter 3 of the theory textbook already reads `Cont`
"through Atkey's lens". A chapter 10 is filed, not written.

## Out of scope
- indexed optics, `at`/`ix`, `plated`, getters and setters as families
  of their own: each by a consumer, none has one yet
- optics over `Chunk`/`Source`: a stream is not a value
- an optics module of its own: the core is ~200 lines beside `Monoid`
  and `Applicative`, and `State.zoom` wants it in the core

## Decisions
- **A second macro, and the policy restated** — specs/codecs.md said
  "the ONE macro this library allows itself reads what the compiler
  already wrote". The selector macro is of the same kind: it reads
  the selector's tree and generates nothing but a call to
  `Lens(get, set)`. The policy is now "a macro only reads; it never
  writes", stated in codecs.md, and both macros meet it.
- **`Lens[S](_.f)` over `Fields[S].f`** — a `Dynamic` with a
  transparent inline `selectDynamic` gives the member syntax without
  a macro (prototyped, works), but `f` does not exist to the IDE:
  no completion, no navigation, no rename. The macro's lambda is
  ordinary code, and its wrong-field error is the compiler's own.
- **Names.** `Optic`, `Iso`, `Lens`, `Prism`, `Affine`, `Traversal`
  are top-level in `okay`; the profunctor classes and the
  interpretations live in `object Optic`, imported by whoever writes
  an interpretation, which applications do not.
- **`Monoid[Vector[A]]` joins `Monoid`'s companion** so that
  `toVector` needs no import; `First[A]` (first-wins) is
  `preview`'s monoid and lives with the optics.
- **Derived operations with lawful defaults, overridable** — the
  first measurement put a one-field `set` at 12x a `copy`, and
  decomposing it showed two costs: the textbook `dimap(first(p))`
  builds a tuple per set, and the Mirror's `fromProduct` walked an
  iterator, an array and a Tuple. So `Strong` DERIVES `lens` from
  `first` (the textbook formula is the default), `Choice` derives
  `prism` from `right`, `Traversing` derives `eachVector` from
  `wander`, and an interpretation may override each with its direct
  road — `Function1`'s lens is `s => set(s, p(get(s)))`. The
  abstraction is intact (the default IS the law), a test holds every
  override to it, and the macro now generates `s.copy(…)` rather than
  going through the Mirror. This is how a profunctor library gets to
  be fast without ceasing to be one.
- **The gate before the hot path** — as handler-fusion did: no optic
  under `Ui.patch` until the number says the tuple `first` builds is
  cheap enough. Measured, not assumed.

## Results

Stage 0 (optics-core) landed 2026-09-09: `src/main/scala/Optic.scala`
(~230 lines), `Focus.scala` (the selector macro, ~45), `TestOptics`
(7, run on the JVM, JS and Native), `OpticsBenchmark` in compare,
`Monoid`'s companion gained `Monoid[Vector[A]]`, and `Monad[Option]`
became a global instance at the operator's ask (three direct-style
suites had each carried it locally; theirs still win by scope).

THE GATE, ns/op, per-lane minima over the rounds on a quiet box
(load 6.6; a round at load 60 was discarded — its 1000-element lanes
ran 6x slower for lane and control alike, which is the machine, not
the code):

| lane | optic | control | ratio |
|---|---|---|---|
| one field, `Lens[S](_.f).set` | 3.1 | `copy` 1.8 | 1.7x |
| one field, `Lens(get, set)` with a hand `copy` | 3.0 | 1.8 | 1.6x |
| one field, `Lens.field[S]("f")` (Mirror) | 4.3 | 1.8 | 2.4x |
| lens ∘ prism ∘ lens `.set` | 14.9 | nested `copy` 4.3 | 3.5x |
| `Traversal.each.modify` over 1000 | 1382 | `Vector.map` 1382 | 1.00x |
| `Traversal.each.foldMap` over 1000 | 2323 | `foldLeft` 1348 | 1.7x |

What the FIRST measurement taught (before the direct roads): 12x,
17x, 30x, 5x, 5x. Decomposed, two costs, neither the profunctor idea:
the textbook `dimap(first(p))` builds a tuple per set, and the Mirror
setter walked an iterator, an array and a `Tuple.fromArray`. The
derived-with-default operations (Decisions) and a macro that
generates `s.copy(…)` took the one-field lens from 37 ns to 3.1; the
Mirror route's array became a one-element-replaced VIEW of the
original product (7.8 → 4.3). What remains is real and named: a
composed optic re-interprets itself on every `set` — one closure per
optic in the chain — and `set(b)` with a varying `b` cannot be hoisted
(`modify(f)` with a fixed `f` can: the interpretation is built once).
The fold's 1.7x is the `Monoid` dictionary call per element against
an inlined `+`.

Verdict: the bar was 1.5x; the traversal clears it, the one-field
lens sits on it, composition does not. Optics are a convenience and
a derivation layer (stages 1–3), not a hot-path primitive; `Ui.patch`
keeps its navigation. A lane to cache a composed optic's
interpretation (an affine as `preview`/`set` pair, built once) is
filed as optics-fast, by a consumer that needs it.
