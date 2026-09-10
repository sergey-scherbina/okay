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

**The Json optics live in their own object.** The interface below
said `Json.field(name)`; they are `JsonOptic.field(name)`, because
`Json.scala` is the parser, printer and codec and depends on nothing
of the core, while an optic is the core's. A separate file keeps that
boundary, and Scala 3 cannot reopen an object to blur it.

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
Affine(preview: S => Either[T, A], set: (S, B) => T)   // the affines that are not a composition

// over Json (okay-codec, stage 1)
JsonOptic.at(name): Lens[Json, Json, Option[Json], Option[Json]]   // the lawful one
JsonOptic.field(name);  JsonOptic.index(i);  JsonOptic.caseOf(name)   // affines over it
JsonOptic.values;  JsonOptic.entries                                  // traversals
JsonOptic.path(schema, "addr.city"): Option[Affine[Json, Json, Json, Json]]

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
      THOSE NUMBERS ARE THE INTERPRETATION'S, and stages 8-11 have
      since removed most of what they measured — see "The verdict, and
      what changed it" at the end of this file.
      So stage 2 keeps `Ui.patch`'s navigation; `Ui.path` and
      `Ui.key` are a convenience over it. The numbers, and what the
      first measurement taught, are in Results.

Stage 1 — from the Schema (optics-schema, LANDED 2026-09-09):
- [x] optics over `Json` in okay-codec: `JsonOptic.at(name)` is THE
      lawful one (its focus an `Option[Json]`: absent is None,
      `set(None)` removes), and `field`, `index`, `caseOf` are affines
      over it, `values` and `entries` traversals — the `at`/`ix` pair,
      arrived at for the reason every library arrives at it (below)
- [x] `JsonOptic.path(schema, key)` reads a form's dotted key as an
      optic AGAINST THE SCHEMA — which is what tells a sum from a
      product, since `{"Case": {...}}` has a level the key does not
      mention; a key the schema does not write answers `None`
- [x] the drift law of the second order: for a derived schema, the
      value optic and the Json optic commute with the codec — a
      field, a NESTED field through the composition, and a list
      through the traversal on both sides
- [x] a prism per case of a derived sum: `Prism.of[Shape, Circle]` on
      the value and `JsonOptic.caseOf("Circle")` on the Json preview
      exactly together, set commutes, and a miss leaves both wholes
      alone. (By the ClassTag, not the Mirror's ordinal: `Prism.of`
      from stage 0 already IS that prism, and a second spelling would
      have been a second thing to keep correct.)
- [x] `Form.edit` routes through the optic path rather than
      `Path.parse`: SETTLED 2026-09-10 by stage 6, and settled as a
      REFINEMENT rather than as the rewrite. Half the reason below has
      dissolved — `Iso.non` makes the creating half lawful, and
      `JsonOptic.creating` reproduces `Form.edit` exactly, defaults
      and all (TestFormOptic). The other half stands and is why Form
      keeps its router: interpreting a text by the field's schema,
      growing a list, swapping a sum's case are not navigation. What
      was written here before, kept because it was half right:
- [x] (the original reasoning) —
      `Form.edit` CREATES missing parents on the way down, which is
      exactly the unlawful lens `JsonOptic` refuses to have, and
      interprets the Edit at the leaf, which is not navigation at all.
      `TestFormOptic` asserts what the rewrite was after and gets it
      without one: where both are defined, `Form.edit` touches
      EXACTLY the focus `JsonOptic.path` names and nothing else; and
      where they differ — a missing parent — the test names the
      difference.

Stage 2 — the tree (optics-ui, LANDED 2026-09-09):
- [x] `Ui.key(k)` — every node a key names, a TRAVERSAL rather than
      the affine this list first said: a key is unique on a
      well-formed tree and `Ui.keys` reads keys as a set, but nothing
      enforces it, and `foldLocal` rewrote every match with `map`. A
      traversal keeps that exactly instead of quietly picking one.
- [x] `Ui.path(is)` — the affine at an index path, index for index
      with `Ui.patch`'s own walk, so `path(p).preview` names exactly
      the node a `Patch` at `p` touches (asserted against `Ui.patch`)
- [x] `Ui.everywhere` — every node; and `Ui.shown`, which was not in
      the plan and had to exist: `submit` must not find a form inside
      a hidden tab, and `keys`/`forms`/`focusable` already read the
      tree that way
- [x] `foldLocal`, `submit` and `tabOf` are written with them (86
      okay-ui tests unchanged); the Kotlin and Swift `Tree` files name
      the optic each of their functions is
- [x] `Ui.patch` KEEPS its navigation, as stage 0's gate decided
      (3.5x); the diff-then-patch law is untouched

Stage 3 — the state (optics-state, LANDED 2026-09-09):
- [x] `State.zoom(lens)(p: X ! State % A + F): X ! State % S + F` — a
      program over a part runs over the whole, and touches nothing
      else; the rest of the row passes through (a Writer beside the
      State is asserted); a program that does not write is the
      identity on the state
- [x] `PState.zoom(lens)` — one `shift`, and the stage's whole
      argument: a `Lens[S1, S2, A1, A2]` turns a part's transition
      A1 -> A2 into the whole's S1 -> S2, so `Box[String]` becomes
      `Box[Int]` because the part did, and asking for the old type
      back does not compile
- [x] on the JVM, JS and Native, from one file

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

## optics-fast — built, measured, declined (2026-09-10)

The operator asked for the lane the spec had filed "by a consumer
that needs it; none does". Built, measured, and the premise refuted —
which is the lane's result and is worth more than the fast path would
have been.

The idea was sound and is chapter 10's isomorphism made executable:
the CONCRETE representation of an optic is itself a profunctor
(`Optic.Market` for the affine pair, `Optic.Shop` for the lens pair
without an `Either`), so running an optic at it once yields that
optic's own pair, and the chain of interpretations is paid once
instead of per call. `o.compiled` and `o.compiledLens` do exactly
that, with `TestOptics` holding every compiled form to the optic it
came from, on every family and every operation.

It is slower, and the FIRST account of why was wrong. What follows is
the second, taken with `-prof gc`, which is what settled it — the
bytes are the evidence the times alone could not give.

| lane | ns/op | B/op |
|---|---|---|
| `copy` by hand | 1.8 | 32 |
| `Lens[S](_.f).set` | 3.3 | 48 |
| the same, compiled without `Either` (`Shop`) | 4.1 | 48 |
| the same, compiled through `Market` | 9.8 | 48 |
| nested `copy` by hand | 4.5 | 72 |
| lens ∘ prism ∘ lens `.set` | 15.3 | 176 |
| the same, compiled | 33.0 | 296 |

**The retracted account.** This section first said "the `Either`
costs more than the chain", from the times alone (3.9 against 8.0 for
one lens with the pair's Either and without). The bytes refute it:
both allocate 48 B/op, the same as the live optic. The `Either` never
reaches the heap there — escape analysis removes it — so its cost is
instructions, not allocation, and calling it the expensive part was
reading one number and inventing a mechanism for it.

**What the bytes say instead.** Compiling the COMPOSED optic adds 120
bytes per operation: 176 live, 296 compiled. One cause covers both
rows of the table. A live optic held in a `val` inlines into the call
site whole, so escape analysis flattens every intermediate — the
prism's `Either`, the lens's tuple — and they never allocate. A
compiled optic is a field holding a lambda; the JIT does not inline
through it, the same intermediates escape, and they become real
allocation. It is not Either-versus-chain. It is inlined-versus-not:
the `Either` is free while the chain inlines and expensive when it
does not, which is why the single lens loses instructions (0.8 ns for
the indirect call, 6.5 more through `Market`'s plumbing) while the
composed one loses bytes.

**So the JIT already does this compilation, and better.** That was the
other half of the first account and it survives, now with the
allocation figures behind it rather than an assertion.

What ships: the code stays, the way `Fused` stayed after
handler-fusion's gate — the artifact a measurement was taken on,
lawful and tested, so the number can be taken again and so that the
isomorphism chapter 10 cites can be run. Its doc comment says, in the
first screen, that it is not a fast path. Nothing in the library uses
it, and nothing should for speed.

## optics-fuse — the fusion moved into the compiler (2026-09-10)

The operator read optics-fast's answer correctly: if the indirection
is the problem, remove it at COMPILE time. `Fuse.set(optic)(b)(s)` and
`Fuse.modify` read the optic expression — a chain of `Lens(get, put)`
and `Prism.some` joined by `andThen` — and emit the nested update,
beta-reducing every lambda, so no optic and no intermediate survives.

It reaches the hand-written update exactly:

| lane | ns/op | B/op |
|---|---|---|
| `copy` by hand | 1.46 | 24 |
| `Fuse.set`, one field | 1.53 | **24** |
| the live optic, one field | 2.78 | 40 |
| nested `copy` by hand | 4.25 | 64 |
| `Fuse.set`, lens ∘ prism ∘ lens | 3.93 | **64** |
| the live optic, composed | 15.04 | 168 |

The bytes are the proof rather than the times: 24 and 64 are the
hand-written figures to the byte, so the emitted code IS the update a
person would write. The composed set goes 15.0 -> 3.9 ns, 168 -> 64
B/op.

**What it reads, and the limitation that is not going away.** The
halves must be EXPLICIT — `Lens(_.f, (s, v) => s.copy(f = v))`, named
by an `inline def` or written at the call site. `Lens[S](_.f)` is
itself a macro, and an inline argument is captured BEFORE a nested
macro in it expands, so a macro cannot see through another macro's
call. This was not deduced: with selector-built lenses every call fell
back and the benchmark returned the live optic's own figure to the
byte (168 B/op), which is how it was found. Following the reference
through an `inline def` was the other half of the same lesson — a
reference arrives unexpanded, and without following it nothing fused
at all.

Everything it cannot read falls back to the ordinary optic, so
correctness never depends on the fusion; a test asserts the fused and
the unfused answers agree on every shape, including the ones that
fall back.

## Results

Stage 3 (optics-state) landed 2026-09-09: `State.zoom` and
`PState.zoom`, `TestZoom` (6, all three platforms). `State.zoom` is
not a handler but an INTERPRETATION of one effect into another — every
`Get` on the part is a `Get` on the whole read through the lens, every
`Set` a read, a lens `set` and a write — written as `handle`'s split
loop.

One thing the loop cannot say, and the way round it: in the lone-
operation arm the GADT refinement gives `A <: X`, not `A = X`, and
`Free` is invariant in its answer, so `A ! row` is not `X ! row`
without a cast. A LONE OPERATION IS A BIND WITH A PURE CONTINUATION,
and the Bind arm already handles it — so that case delegates instead
of casting, at one node's cost for a shape that is rare anyway. (The
repo's rule: no casts without necessity.)

`PState.zoom` is the stage's argument in one line:

```scala
shift(k => (s1: S1) => (m / (x => (a2: A2) => k(x)(l.set(a2)(s1))))(l.get(s1)))
```

Read the part out of the whole to start the inner program, put the
part back to finish it, and the types line up on their own: the whole
changes type exactly when the part does. That is the sense in which
the four-parameter lens and Atkey's parameterised state are the same
picture, and it is why `theory-optics` (chapter 10) is filed beside
chapter 3 rather than on its own.

Stage 2 (optics-ui) landed 2026-09-09: `Ui.everywhere`, `Ui.shown`,
`Ui.key`, `Ui.path` in okay-ui, `foldLocal`/`submit`/`tabOf` written
with them, `TestUiOptic` (5, JVM — okay-ui's shared tests are JVM by
its own build). Two things the plan had wrong, both found by writing
it:

**A bottom-up rewrite is not a traversal.** `Ui.map` rewrites children
first and then applies `f` to the REBUILT node; a traversal has only
an `Applicative`, and applying `f` to a rebuilt node is a bind. So
`everywhere` is top-down — `f` sees the node, the children it had are
traversed and put back into what `f` answered — and the two agree on
every `f` that keeps a node's children, which is every call site. The
test asserts the agreement and names an `f` for which they differ.

**`shown` had to exist.** The plan named `everywhere`; `submit` needs
the other reading, because a form inside a hidden tab must not be
submittable, and `keys`/`forms`/`focusable` already walk that way. So
there are two traversals with two meanings, and the test asserts the
capability rule through them.

Stage 1 (optics-schema) landed 2026-09-09:
`okay-codec/src/main/scala/okay/codec/JsonOptic.scala` (~150 lines),
`TestJsonOptic` (6, on the JVM, JS and Native), `TestFormOptic` (2,
okay-ui), and `Affine(preview, set)` added to the core for the
affines that are not a lens ∘ prism — an array index among them.

TWO FINDINGS, both recorded rather than smoothed over:

**`at`'s PutPut holds modulo field order.** `set(w) ∘ set(v)` equals
`set(w)` exactly, EXCEPT when `v` is `None`: a removal loses where the
field was, and the next insert appends. JSON objects are unordered by
RFC 8259 and `JObj` keeps a Vector because the codec's field order is
worth preserving; those two facts meet here. The test asserts the
exact law where nothing is removed, the ordered-reading law
otherwise, and names the pair it differs on. Every other law, and the
drift law itself, is exact.

**`Form.edit` keeps its router.** The plan was to route it through
the optic path. It creates missing parents on the way down — the
unlawful lens, which `JsonOptic` refuses to have — and interprets the
Edit at the leaf, which is not navigation. Rewriting it would have
cost the optics their laws to make a router shorter. Instead
`TestFormOptic` asserts the statement the rewrite was after: where
both are defined, `Form.edit` touches exactly the focus the optic
path names and nothing else; and it names the one place they differ.

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
original product (7.8 → 4.3). What remains is real, and what it is was WRONG here until optics-fast
measured it (below): this said the cost was a composed optic
re-interpreting itself on every `set`. It is not — the JIT flattens
that. The cost is the allocation the composed shape still leaves after
inlining (176 B/op against a nested copy's 72), and `set(b)` with a
varying `b` cannot be hoisted.
The fold's 1.7x is the `Monoid` dictionary call per element against
an inlined `+`.

Verdict: the bar was 1.5x; the traversal clears it, the one-field
lens sits on it, composition does not. Optics are a convenience and
a derivation layer (stages 1–3), not a hot-path primitive; `Ui.patch`
keeps its navigation. A lane to cache a composed optic's
interpretation was filed as optics-fast, and is now measured and
declined — see below.

## Stage 4 — the aggregating families (optics-arc-2, LANDED 2026-09-10)

Operator's direction after the survey: take all three openings.

A traversal walks a shape and returns it. Aggregation is a different
family, catalogued in Clarke, Elkins, Gibbons, Loregian, Milewski,
Pillmore and Roman (Compositionality 2024) and shipped here:

- [x] `Reflecting` — lift a profunctor through ANY Applicative, which
      is what collapses many focuses into one answer. `Kaleidoscope`
      is its optic; `Kaleidoscope.each[F]` is the family.
- [x] `Classifying` — an algebraic (classifying) lens, whose put is an
      algebra over every whole rather than a value. Given as its
      constructor: the paper says the laws of a mixed optic are not
      settled outside particular cases, so nothing is claimed here
      that is not tested.
- [x] `Aggregating` interprets BOTH in one instance, because the
      composite asks for the intersection. Deliberately not `Strong`:
      `first` would have to answer a C from a Vector of Cs, so an
      ordinary lens does not reach this road at all — asserted by a
      compile-time test that also asserts it fails for the missing
      instance rather than for a typo.
- [x] `aggregateWith` takes okay's own `Aggregator`, which was already
      a Moore machine with an applicative `zip` and a semigroup
      `merge`.
- [x] aggregate-then-classify, the literature's own example, as one
      composite optic (TestAggregationOptics).

Found: the ZIP applicative needs an INFINITE `pure`
(`pure(f) <*> xs == fmap(xs)(f)` fails for any finite one), so the
lawful zip applicative on a strict sequence does not exist and
`Optic.zipLazy` is a LazyList. That is why the literature's is a
ZipList too, and it is not a Scala accident.

## Stage 5 — a scanner is an arrow (optics-arc-2, LANDED 2026-09-10)

- [x] `Optic.Category` and `Optic.Arrow` beside `Profunctor`, with
      `Strong.second` derived by swapping. Rivas and Jaskelioff (JFP
      2017) put arrows in the same table as monads and applicatives —
      a strong monoid in the category of profunctors — where an optic
      is not a monoid but a Tambara module.
- [x] `Mealy` in okay-lex with Category, Strong and Choice, a driver,
      and `ofScan` as the door from `Scan`.
- [x] the two things that could not be written before: a scanner
      followed by a token step in ONE pass, and two scanners over one
      input by `fanout` (TestMealy).
- [x] the arrow laws observed the only way a machine's equality can be
      observed, over an input.

The cost is stated where it is paid: a step here allocates the next
machine, which is the per-character object `scan-step-allocation`
removed from the lexing road. This is the composition layer;
`Scan.all` and `Scan.chunks` remain the road.

## Stage 6 — creating a missing parent, lawfully (optics-arc-2, LANDED 2026-09-10)

- [x] `Iso.non(d)` — Kmett's: absence reads as `d`, and writing `d`
      back is absence again. An iso modulo one normalisation, and the
      test names it: `Some(d)` and `None` are the same point.
- [x] `JsonOptic.creating(steps)` — the same composed down a path,
      with a per-level default because what an absent field means is a
      question only the schema can answer.
- [x] it produces EXACTLY what `Form.edit` produces at one level of
      absence and at two (TestFormOptic), which is what the stage-1
      rewrite wanted and could not have while the creation lived
      inside `set`.

Two findings, both from the tests rather than from the design:
- writing the default PRUNES the whole spine, because absence and the
  default are one point at every level. Creation downwards and pruning
  upwards are one law read in two directions; a sibling anywhere stops
  it exactly there.
- the law's domain has an edge, inherited from `at`: a SCALAR where a
  parent belongs refuses the write rather than clobbering it, so
  PutGet is claimed where every named level is an object or absent,
  and the excluded case has its own test.

## Stage 7 — the textbook (optics-arc-2, LANDED 2026-09-10)

- [x] docs/theory/10-optics.md gains three sections — aggregating is
      not iterating, arrows are the other row of the same table, and
      origami (the plate is a traversal, the rewrite is a fold over
      it, and the optic's residual is McBride's derivative) — and the
      creating-lens refusal is rewritten to record how it dissolved.
- [x] the bibliography goes from 10 entries to 25, in four groups:
      the families beyond lens/prism/traversal, arrows and monoids,
      origami and the residual, and effects/grades/indices.

## Stage 8 — the idiomatic lens fuses (optics-zero-tax, 2026-09-10)

The operator asked why an optic cannot simply be expanded at compile
time into the code a person would write. It can, and `Fuse` already
did — for a lens whose halves are written out. The shape it could not
read was `Lens[S](_.f)`, which is the idiomatic way to build a lens
here, so in practice the fusion was off for most code that wanted it.

The reason on record was "a macro cannot see through another macro's
captured call". True, and beside the point: `Focus.impl` is an
ordinary compile-time function over trees, so `Fuse.plan` does not
need the compiler to expand it. It calls it, with the selector and
the Mirror it finds in the tree.

- [x] `plan` reads `Lens[S](_.f)`, alone and composed, directly and
      through an `inline def`.
- [x] `Focus.impl` loses the `S <: Product` bound it never used, so
      no cast is needed to call it with types recovered from a tree.
      The bound stays on the class, where the caller writes it.
- [x] a test that can SEE the fusion, not just its answer: the
      fallback needs the `Function1` interpretation and fused code
      never asks for it, so a poisoned instance in scope tells them
      apart at run time. Watched it fail with the case switched off
      before it was kept — which is the check the first lane did not
      have, and why the fusion could be measured doing nothing for a
      whole lane while every test stayed green.

MEASURED (one round, load 16-25, `-prof gc`; allocation bars ±0.008 B):

| lane | ns/op | B/op |
|---|---|---|
| `copySet` — the hand-written `p.copy(age = n)` | 1.488 | 24 |
| **`fusedSelectorSet`** — `Fuse.set(Lens[P](_.age))` | **1.479** | **24** |
| `fusedLensSet` — the same with the halves written out | 1.524 | 24 |
| `lensSet` — the live optic, as before this lane | 2.600 | 40 |
| `nestedCopy` — the hand-written nested update | 3.713 | 64 |
| **`fusedSelectorComposed`** — a selector chain through `Some` | **4.213** | **64** |
| `fusedComposedSet` — the same, halves written out | 4.132 | 64 |
| `composedSet` — the live composed optic | 16.547 | 168 |

One field: the idiomatic lens, fused, IS the hand-written `copy` — the
same 24 bytes and 1.479 against 1.488 ns, which is inside the bars. The
tax it used to pay was 2.600 ns and 40 bytes, and it is gone.

Composed: the allocation is identical to the hand-written nested
update, 64 bytes both, and the time is 4.2 against 3.7 ns — half a
nanosecond above hand-written and 3.9x below the live optic's 16.5.
Where that half nanosecond is has not been chased and is not claimed.

So the answer to "why can it not just be expanded into the code a
person would write" is that it is, for `set` and `modify` on a path
known at compile time, and now for the way people write the path.

Still unread, and the reason is the same shape rather than a
mystery: `Lens.field[S]("name")` expands to a BLOCK with a statement
in it (`val i = constValue[...]`), and `plan` takes `Block(Nil, _)`
only. It is also the slowest of the lens forms and the least
idiomatic, so it waits for a caller who wants it.

## Stage 9 — fusing by default (optics-fuse-by-default, 2026-09-10)

The operator's next question was the right one: why does `Fuse` have
to be called by name? It does not.

- [x] `set` and `modify` are the EXTENSIONS now, inline, with an
      inline receiver and the same planner as their body. `Fuse.set`
      stays as the form that takes the whole and needs no lambda.
- [x] the planner follows a plain `val`, which is how an optic is
      actually stored. The soundness argument is the planner itself:
      it succeeds only on pure optic CONSTRUCTIONS, so following a
      definition can only emit what that expression means. Excluded
      is anything the call site's type does not fix — a `var`, an
      overridable member — and the test states that boundary by what
      throws under a poisoned interpretation rather than in words.
- [x] the fallbacks go straight to the interpretation instead of back
      through `.set`, which is now this macro.

| lane | before | now | hand-written |
|---|---|---|---|
| `lensSet` — `age.set(n)(p)`, the optic in a `val` | 2.600 ns / 40 B | **1.803 / 24** | `copySet` 1.543 / 24 |
| `composedSet` — a lens ∘ prism ∘ lens chain, all `val`s | 16.547 / 168 | **4.212 / 64** | `nestedCopy` 4.666 / 64 |
| `fieldSet` — `Lens.field[S]("age")`, still unread | 4.3 / — | 3.919 / 56 | 1.543 / 24 |
| `traversalOver` over 1000 | 1.00x of `map` | 2063 / 18 648 | `vectorMap` 1961 / 18 648 |

Composed optics are where it lands: 16.5 ns and 168 bytes become 4.2
and 64, which is a hand-written nested `copy`. One field keeps a
lambda that `Fuse.set`'s whole-taking form does not (1.803 against
1.615), and its allocation is the hand-written 24 either way.

**The compile-time price was the thing to fear, and it is not
measurable.** Alternating A/B, a fresh sbt per run, `okayJVM/clean;
Test/compile`: 27/16/17 s with the change against 20/17/22 s without.
Minima 16 and 17, ranges overlapping. An earlier reading of 61 against
32 was JIT warm-up in one sbt and is not evidence of anything — the
first compile in a cold sbt is always the slow one.

## Stage 10 — the read side fuses too (optics-fuse-reads, 2026-09-10)

Stage 9 said `get`, `foldMap` and `traverseOf` were not fused, simply
not attempted. The operator said attempt them.

- [x] `emitGet` — a lens chain's `get` IS the projection `s.a.b`, with
      every lambda beta-reduced away. What it replaces is not free:
      the interpretation allocates a `Forget` per level and runs the
      composed optic to read one field.
- [x] `get`, `preview`, `toVector` and `foldMap` are inline, bodied by
      it. For a readable plan `preview` is `Some(...)` and `toVector`
      is `Vector(...)` with no test at all, because a plan the read
      side can emit has no absence in it.
- [x] `traverseOf` — `fmap(f(s.a.b), b => put(s, b))`: the read this
      stage emits, the effect, and the write stage 8 already emitted.
      It falls back when `Applicative[F]` cannot be summoned at the
      call site, which is the honest condition rather than a guess.
- [x] the same poisoned-instance test, on `Forget` this time: a fused
      read never asks for the interpretation, so an instance that
      throws tells the two apart at run time.

| lane | ns/op | B/op |
|---|---|---|
| `directAgeGet` — the plain field read `p.age` | 0.433 | ~0 |
| **`lensGet`** — `age.get(p)`, fused | **0.431** | ~0 |
| `lensGetInterpreted` — the SAME lens through `Forget` | 0.832 | ~0 |
| `fieldGet` — `Lens.field[S]("age")`, still unread | 0.826 | ~0 |

The matched pair is the middle two rows: one lens, read two ways. The
fused read IS the field read, 0.431 against 0.433, and the
interpretation is 1.9x that. `fieldGet` is deliberately NOT the
comparison — it differs in the getter as well (a Mirror's
`productElement` against `_.age`), which would be two changes in one
row, and Lane Rule 2 forbids that.

Allocation is ~0 on every row, and that is worth stating plainly: the
JIT's escape analysis already removes the `Forget` this fusion
deletes, so on the JVM the win is TIME and not bytes. Scala Native and
Scala.js have no such analysis, so the bytes should be real there;
not measured, so not claimed.

A PRISM IN THE CHAIN FALLS BACK, and the reason is not a limitation to
fix later: an absent focus is not a value, so there is no `get` to
emit. `preview` on an affine still answers through the interpretation,
which knows what absence means. Fusing THAT is a real opening —
`if (s.f.isDefined) Some(...) else None` is emittable — and it is
filed rather than done here, because okay-codec's JSON paths are
affines and it wants their workload to measure against.

Also needed on the way: `lambdaIn`, the lambda builder with the owner
named, because `traverseOf` builds a lambda INSIDE another lambda's
body and the inner one must be owned by the outer.

## Stage 11 — an affine's preview fuses (optics-fuse-affine-preview, 2026-09-10)

Stage 10 filed this and the operator said do it. It is the read this
library actually does through an optic: `JsonOptic.field` is
`at ∘ some`, so every path built from it is an affine, and until now
every one of them went through the interpretation.

- [x] `emitPreview`, which unlike `emitGet` knows what to do with
      absence. Three shapes, each what a person would write: a lens
      step is the projection wrapped once at the end; previewing
      `some` on an `Option` IS that Option; and a `some` in the middle
      is the test, with the rest emitted inside it.
- [x] `get` still refuses a prism, and that has not changed for a
      reason — an absent focus is not a value, so there is nothing to
      emit. Only `preview` can say "or nothing".
- [x] the poisoned instance is `Forget[First[A]]` this time, and the
      test was watched failing with the new emitter switched off.

| lane | ns/op | B/op |
|---|---|---|
| `affinePreviewByHand` — `p.address.map(_.zip)` | 4.172 ± 1.332 | 16 |
| **`affinePreview`** — `personZip.preview(p)`, fused | **3.370 ± 0.299** | **16** |
| `affinePreviewInterpreted` — the SAME optic through `Forget[First]` | 35.655 ± 14.101 | 232 |

The fused preview allocates what the hand-written `map` allocates, 16
bytes — the `Some` box, and nothing else. Its time reads below the
hand-written lane, but that lane's bars are ±1.3, so the honest claim
is PARITY and not a win over hand-written code.

The number that is not close is the third row: the same optic through
the interpretation costs 10.6x the time and 14.5x the allocation. That
is what every `JsonOptic.field` path was paying to read one field.

Still on the interpretation for an affine: `foldMap` and `toVector`
(they need the Monoid's `empty` for the absent branch, which the
macro would have to summon) and `traverseOf` (it needs the
applicative's `pure`). Named rather than discovered later.

## The verdict, and what changed it (optics-verdict-refresh, 2026-09-10)

The verdict recorded above was measured before any fusion existed:
"the bar was 1.5x; the traversal clears it, the one-field lens sits on
it, composition does not — optics are a convenience and derivation
layer, not a hot-path primitive". Four lanes on 2026-09-10 made it
false, and a false verdict is worse than no verdict, so here is the
one the measurements support now.

| what you write | optic | hand-written | ratio | measured in |
|---|---|---|---|---|
| `o.get(s)`, one field | 0.431 | 0.433 (`p.age`) | 1.00x | §9e |
| `o.preview(s)`, an affine | 3.370 | 4.172 (`p.f.map(_.g)`) | 0.81x | §9f |
| `o.set(b)(s)`, one field | 1.803 | 1.543 (`copy`) | 1.17x | §9d |
| `o.set(b)(s)`, lens ∘ prism ∘ lens | 4.212 | 4.666 (nested `copy`) | 0.90x | §9d |
| `Fuse.set(o)(b)(s)`, one field | 1.479 | 1.488 | 0.99x | §9c |
| `Traversal.each.modify` over 1000 | 2063 | 1961 (`Vector.map`) | 1.05x | §9e |

Every row is a MATCHED PAIR from one run: the optic and its control
measured together, same box, same minute. They are not one table from
one run — the sections say which — and a full-table re-measure was
attempted for this entry and is not here for a reason worth writing
down. It died at 82% with exit 143, the RAM guard's SIGTERM, on a box
carrying four other builds; the numbers it did produce are unusable
because the CONTROL was inflated with everything else (`copySet` read
3.410 ns where the same lane reads 1.488 in a quiet window). A run
whose control moves 2.3x cannot price anything.

**What an optic costs today: nothing you can see, where the path is
known when the code is compiled.** The read is the field read. The
affine preview is the hand-written `map`. A composed update is the
hand-written nested `copy`, allocation identical to the byte. The
one-field `set` keeps a lambda `set(b)` has to answer, worth about a
quarter of a nanosecond, and `Fuse.set` — which takes the whole
directly — does not even keep that.

**Where the old verdict still holds, exactly.** An optic chosen at RUN
time cannot be fused: nothing is known to emit. There the
interpretation's prices stand, and they are the numbers above the
line. The same is true for `Lens.field[S]("name")`, whose expansion
the planner still cannot read, and for `foldMap`, `toVector` and
`traverseOf` through an affine.

So the guidance is no longer "optics are a convenience layer". It is:
name the path in code and the optic is free; choose it at run time and
you are paying an interpreter, which is the honest trade and always
was.
