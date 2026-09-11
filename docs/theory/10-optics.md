# 10. Optics on profunctors

## The problem is composition, not access

Reading a field out of a record and writing one back are not hard.
What is hard is that there are *several* ways a part can sit inside a
whole — a field is always there, a case of a sum is sometimes there,
every element of a list is there many times — and that programs need
to compose those ways: the city of the address of the person, if the
person has an address, for every person in the list.

Give each shape its own pair of functions and nothing composes.
A lens is `(S => A, (S, B) => T)`, a prism is
`(S => Either[T, A], B => T)`; there is no way to write `∘` for the
pair without a table of which family a lens-then-prism is, which
family a prism-then-traversal is, and so on for every pair. Monocle
keeps that table. Okay does not have one, and this chapter is why.

## Three encodings

**Concrete.** The pairs above, or the existential that unifies them:
an optic is a residue `M` and a pair
`S => (M, A)`, `(M, B) => T` \[[Pickering, Gibbons & Wu 2017](#ref-pickering-2017)\].
Honest, first-order, and uncomposable without the table.

**Van Laarhoven.** `(A => F[B]) => S => F[T]`, quantified over `F`
with a class constraint — `Functor` gives a lens, `Applicative` a
traversal \[[O'Connor 2011](#ref-oconnor-2011)\]. This composes by ordinary function
composition, which is the great trick, but it cannot express a
prism's `review` (`B => T`, with no `S` to read) and so needs a second
mechanism for half the families.

**Profunctor.** An optic is a function `P[A, B] => P[S, T]`,
polymorphic in `P`, where the *constraint* on `P` says which family
it is \[[Pickering, Gibbons & Wu 2017](#ref-pickering-2017); [Boisseau & Gibbons 2018](#ref-boisseau-2018)\]. All
families are one type, composition is `∘`, and `review` is expressible
because `P` is contravariant in its first argument as well as
covariant in its second. This is the encoding Okay uses.

## The constraint is a type parameter

Purescript writes the profunctor encoding with *constraint kinds*: an
optic is a type synonym whose constraint slot is filled by `Strong`,
`Choice` or `Wander`. Scala 3 has no constraint kinds — and does not
need them, because it has intersection types and a class hierarchy:

```scala
// Optic.scala:28
trait Optic[C[_[_, _]], S, T, A, B]:
  def apply[P[_, _]](p: P[A, B])(using C[P]): P[S, T]
  // Optic.scala:33
  def andThen[C2[_[_, _]], A2, B2](o: Optic[C2, A, B, A2, B2])
    : Optic[[P[_, _]] =>> C[P] & C2[P], S, T, A2, B2]
```

The family *is* the constraint — `Lens = Optic[Strong, …]`,
`Prism = Optic[Choice, …]`, `Traversal = Optic[Traversing, …]` — and
composition takes the **intersection**, which is the meet of the
lattice. A lens composed with a prism has type
`Optic[[P] =>> Strong[P] & Choice[P], …]`, the affine traversal,
computed by the compiler rather than looked up in a table; and an
interpretation satisfies the meet by *subtyping*, because
`Traversing` extends both `Strong` and `Choice` (`Optic.scala:81`).
The table Monocle keeps is, here, the subtyping relation the language
already has.

## Tambara modules, and why `first` is the right primitive

What makes a profunctor able to carry a lens is a structure map

```scala
// Optic.scala:59
trait Strong[P[_, _]] extends Profunctor[P]:
  def first[A, B, C](p: P[A, B]): P[(A, C), (B, C)]
```

— "whatever `p` does to `A`, it does to an `A` sitting beside an
untouched `C`". A profunctor with a coherent `first` is a **Tambara
module** over the monoidal structure of products \[[Pastro & Street 2008](#ref-pastro-2008)\];
`Choice`'s `right` is the same thing over coproducts. The theorem that
makes the encoding respectable is that the two representations agree:
the existential form `∃M. (S => (M, A), (M, B) => T)` and the
profunctor form `∀P. Tambara[P] => P[A, B] => P[S, T]` are naturally
isomorphic, by the Yoneda lemma \[[Boisseau & Gibbons 2018](#ref-boisseau-2018); [Clarke et al. 2020](#ref-clarke-2020); [Riley 2018](#ref-riley-2018)\].
That is not decoration: it is the licence to treat a polymorphic
function as if it were a get/set pair, which every use of `get` and
`set` in this library silently does.

## An interpretation is a choice of profunctor

The optic is one function; what it *does* is chosen by the `P` it is
instantiated at. Three choices, three vocabularies:

| `P` | what the optic becomes | where |
|---|---|---|
| `Function1` | `modify`, `set` | `Optic.scala:147` |
| `Forget[R]` (`A => R`, ignoring `B`) | `get`; with a `Monoid[R]`, `preview`, `foldMap`, `toVector` | `Optic.scala:165` |
| `Star[F]` (`A => F[B]`) | `traverseOf` for any `Applicative[F]` | `Optic.scala:182` |

`Forget` is the constant functor in disguise, which is why a `Monoid`
turns it from a lens's reader into a traversal's fold; `Star` is the
Kleisli arrow, which is why it needs exactly what a traversal needs.
And here the library has a sentence a standalone optics library cannot
say: **`F` may be the effect row**. `Star[[x] =>> x ! State % Int]`
makes every traversal an effectful traversal in the row of chapter 5,
so `each.traverseOf(a => State.modify(_ + a))` is a traversal and a
program at once. Optics and effects are usually two libraries that
have never met; here the traversal's applicative slot is the effect
system's carrier, and nothing had to be built for it.

## Type-changing optics *are* parameterised state

Chapter 3 read `Cont[A, S, R]` "through Atkey's lens" as a figure of
speech. It is not one.

A four-parameter optic `Lens[S1, S2, A1, A2]` is a *type-changing*
update: the whole moves `S1 -> S2` exactly when the part moves
`A1 -> A2`. Atkey's parameterised monad `M[A, S, R]` is a computation
indexed by an arrow `S -> R` in a category of states
\[[Atkey 2009](#ref-atkey-2009)\], and `PState` is that instance in this
repository (chapter 3). Put them together and the zoom writes itself:

```scala
// State.scala:197
inline def zoom[S1, S2, A1, A2, X, R](l: Lens[S1, S2, A1, A2])
                                     (m: Cont[X, A2 => R, A1 => R]): Cont[X, S2 => R, S1 => R] =
  shift(k => (s1: S1) => (m / (x => (a2: A2) => k(x)(l.set(a2)(s1))))(l.get(s1)))
```

One `shift`: read the part out of the whole to start the inner
program, put the part back to finish it. Nothing in the body chooses
the types — `l.get` produces the `A1` the program needs, `l.set`
consumes the `A2` it leaves and produces `S2`, and the answer-type
indices line up. A `Box[String]` becomes a `Box[Int]` because its item
did; asking for the old type back does not compile
(`TestZoom.scala`). The lens and the parameterised state are two
readings of the same arrow, and this line is where they meet.

The ordinary, type-preserving version is an effect *interpretation*
rather than a handler (`State.scala:132`): every `Get` on the part is
a `Get` on the whole read through the lens, every `Set` a read, a lens
`set` and a write, and the rest of the row passes through untouched.

## Aggregating is not iterating

A traversal walks a traversable *shape* and hands it back with the same
shape. Nothing in that machinery lets you take many focuses and answer
one thing, and the reason is visible in the classes: `Traversing.wander`
is applicative-polymorphic in the effect but structure-preserving in
the value. The families that do aggregate were catalogued only
recently \[[Clarke et al. 2020](#ref-clarke-2020)\], and okay has two
of them.

A **kaleidoscope** lifts a profunctor through *any* `Applicative`
(`Optic.scala:144`):

```scala
trait Reflecting[P[_, _]] extends Profunctor[P]:
  def reflected[F[_], A, B](p: P[A, B])(using Applicative[F]): P[F[A], F[B]]
```

Read `reflected` beside `wander` and the difference is the whole
family: one needs a shape to walk and returns it, the other needs an
applicative and returns one answer. Which answer depends on what that
applicative's `app` *means* — with the zip applicative it is
position-wise, so three rows of measurements aggregate to the columns'
means; with a cartesian one it is every combination. Same optic, and
the applicative is the reading.

The zip applicative is where a small law bites. `pure` for zipping has
to be the infinite repeat, because `pure(f) <*> xs == fmap(xs)(f)`
fails for any finite `pure` the moment `xs` is longer than it. So the
lawful zip applicative on a strict sequence does not exist, and okay's
is a `LazyList` (`Optic.scala:180`). That is not a Scala accident: it
is the reason `ZipList` is the one in the literature too.

An **algebraic**, or classifying, lens puts by an algebra rather than
by a value (`Optic.scala:159`). A lens's `set` takes the new focus; this
takes the whole dataset alongside it and *decides*. The literature's
example is a measurement classified against everything measured
before, and okay's test is the same example in miniature: aggregate
the columns, then ask what the aggregate is nearest to.

The two compose, and the composite is one optic:

```scala
measured.andThen(eachColumn).aggregate(mean)(dataset)
```

which reads: view every point's measurements, average them
position-wise, classify the average. Both halves are optics, and the
composition is `andThen` like any other.

Two things about this are worth keeping. First, the aggregating
interpretation satisfies **both** classes in one instance
(`Optic.scala:352`), because the composite asks for their intersection
and an intersection is met by one value — the same mechanism the
lattice has used since stage 0, with nothing added for the new
families. Second, it is deliberately **not** `Strong`: `first` would
have to answer a `C` from a `Vector[C]`, and there is no honest choice.
So an ordinary lens does not reach this road at all, and the
classifying lens is what stands in its place. A compile-time test
asserts exactly that, and asserts it fails for the missing instance
rather than for a typo.

## Arrows are the other row of the same table

The question "what else is a profunctor" has a tidy answer that is not
about optics at all. Rivas and Jaskelioff put three notions of
computation in one frame \[[Rivas & Jaskelioff 2017](#ref-rivas-2017)\]:
a monad is a monoid in endofunctors, an applicative is a monoid for
Day convolution, and an **arrow is a strong monoid in the category of
profunctors**. An optic is not a monoid there at all. It is a Tambara
module, an *action* of a monoidal category on a profunctor. Arrows and
optics are neighbours in one table, not rivals for one job, and okay
writes both on the same `Profunctor` (`Optic.scala:93`) to say so.

The payoff is concrete. `okay-lex`'s `Scan` is a Mealy machine with the
state written out as a value, which is what makes it fast and
incremental — and what stopped it composing. `Mealy`
(`Mealy.scala:19`) is the same machine written so that it composes:

```scala
final case class Mealy[-A, +B](run: A => (Mealy[A, B], B))
```

with `Category`, `Strong` and `Choice`, and `ofScan`
(`Mealy.scala:50`) as the door. A scanner followed by a token step in
one pass, or two scanners over one input by `fanout`, are now ordinary
expressions. The price is stated where it is paid: a step here
allocates the next machine, which is the very per-character object
`scan-step-allocation` removed from the lexing road, so this is the
composition layer and `Scan.all` stays the road.

## Origami: the plate is a traversal, the rewrite is a fold over it

The oldest connection here is also the one most likely to be missed,
partly because of a name. In *Bananas, Lenses, Envelopes and Barbed
Wire* \[[Meijer, Fokkinga & Paterson 1991](#ref-meijer-1991)\] the
"lenses" are the anamorphism brackets. They are not these lenses, and
noticing that is the beginning of the real connection rather than the
end of it.

The real connection is Uniplate \[[Mitchell & Runciman 2007](#ref-mitchell-2007)\].
Its one primitive answers a node's immediate children *and* a function
to put them back — which is precisely a traversal of the self-similar
children, and the lens libraries say so by shipping it as one. Given
that single optic, every recursion scheme follows: `universe` is the
fold, `transform` is the bottom-up rewrite, `descend` is one level.
The optic is the base functor's door.

Which is where okay's own refusal lands, sharpened. `Ui.everywhere`
(`Ui.scala:372`) is top-down, and the reason given above is that a
bottom-up rewrite binds the effect and so needs a monad. Say it in
origami's words instead: `transform f = f . over children (transform
f)`, a catamorphism *built from* the traversal. The traversal is the
plate; the rewrite is a fold over it. Not the same thing, and Uniplate
keeps them apart under two names for the same reason.

There is one more piece of old machinery in this chapter's own
definition. An optic is `(l : S → M ⊗ A, r : M ⊗ B → T)`, where the
`M` is called the residual. For a lens into a product, that residual is
everything except the focus: the one-hole context. McBride showed that
the type of one-hole contexts is the *derivative* of the type
\[[McBride 2001](#ref-mcbride-2001)\], and Huet's zipper
\[[Huet 1997](#ref-huet-1997)\] is that context carried around with a
focus. So a lens's residual is a derivative, and okay's `Ui.path`
navigation, which walks a tree carrying where it came from, is the
same idea written by hand.

## Where the theory said no

Two refusals, both recorded in `specs/optics.md` and both instructive.

**A creating lens is not a lens — and then it is, once absence moves
into the focus.** A form starts from a partial value, so the router
that folds an edit into it must create missing parents on the way
down. Written as a lens on JSON that is `get(absent) = empty; set(v)`
— and it breaks GetPut: `set(get(s))(s)` inserts an empty object where
there was none, so it is not `s`. The lawful decomposition is the
`at`/`ix` pair every optics library arrives at: `at(name)` has an
`Option[Json]` in the focus and is a genuine lens
(`JsonOptic.scala:38`), and `field(name)` is `at(name) ∘ some`
(`JsonOptic.scala:53`), an affine that refuses where the field is
absent.

That was the whole story for a stage, and it was half the story. What
makes the creating router unlawful is not the creating: it is that the
creation happens *inside* `set`, behind the caller's back, so `get`
cannot see it coming. Kmett's `non` (`Optic.scala:471`) moves absence
into the focus — an absent value reads as a default, and writing that
default back is absence again — and then `at(name) ∘ non(d)` creates
on the way down while staying a lens. Composed down a path
(`JsonOptic.scala:69`), it produces *exactly* what okay's hand-written
form router produces, defaults and all, which a test now asserts at
one level of absence and at two.

Two consequences the tests found rather than assumed. Writing the
default **prunes the whole spine**: absence and the default are one
point at every level, so removing a leaf makes its parent the default,
which removes the parent, and a sibling anywhere stops it exactly
there. Creation downwards and pruning upwards are one law read in two
directions. And the law has a stated edge: a *scalar* where a parent
belongs refuses the write rather than clobbering it, which is `at`'s
older choice, so PutGet is claimed on the domain where every named
level is an object or absent.

`non` is an iso modulo one normalisation, and the test says which:
`Some(d)` and `None` are the same point, so the round trip sends
`Some(d)` to `None`. Where the default *means* absence — an empty
object in JSON — that is the intended reading rather than a wart.

Okay's form router still keeps its own walk, for the reason that
survives: interpreting a text by the field's schema, growing a list,
swapping a sum's case are not navigation, and no optic should pretend
they are.

(One law survives only in a weaker reading, and the test says so:
`at`'s PutPut is exact unless the first put *removed* the field, since
a removal loses where the field was and the next insert appends. JSON
objects are unordered \[[Bray 2017](#ref-bray-2017)\]; the representation keeps order
because the codec's field order is worth preserving. The two facts meet
exactly there.)

**A bottom-up rewrite is not a traversal.** `Ui.map` rewrites a tree's
children first and then applies `f` to the *rebuilt* node. A traversal
has only an `Applicative`, and applying `f` to a rebuilt node means
binding the effect — `F[Ui] >>= f` — which is a monad. What that
operation actually is, is a catamorphism: a fold over the tree's
functor \[[Meijer, Fokkinga & Paterson 1991](#ref-meijer-1991)\], and folds are not
traversals. So `Ui.everywhere` (`Ui.scala:372`) is *top-down* — `f`
sees the node, the children it had are traversed and put back into
what `f` answered — the two agree on every `f` that keeps a node's
children, which is every call site, and the test names an `f` for
which they differ rather than claiming an equality that does not hold.

## What the laws cost, measured

The house rule of chapter 6 applies here too: the general form stays,
and the fast path exists because a benchmark said so.

The textbook lens is `dimap(first(p))(s => (get(s), s), (b, s) =>
set(s, b))` — a tuple allocated on every `set`. Measured against a
hand-written `copy`, that first version cost 12x for one field and
30x for a composed optic. So the classes *derive* their operations
with the textbook formula as the **default** (`Optic.scala:61`,
`Optic.scala:67`), and an interpretation may override with a direct
road — `Function1`'s lens is `s => set(s, p(get(s)))`
(`Optic.scala:153`) — with a test holding every override to the
default it replaces. After that: one field 1.7x of a `copy` (1.2 ns),
a traversal 1.00x of `Vector.map`, a composed optic 3.5x. The bar was
1.5x, so optics were a convenience and derivation layer in this
library and not a hot-path primitive.

**That verdict is no longer true, and the reason is the rest of this
section.** Those numbers price the INTERPRETATION — the profunctor
chain run at `Function1` or `Forget`, with its closures and its
`Forget` per level. Where the optic's shape can be read at compile
time, none of that is emitted any more: `set` and `modify` are inline
extensions bodied by a planner that emits the update a person would
write, and `get`, `preview`, `toVector`, `foldMap` and `traverseOf`
emit the projection. Matched pairs, each measured with its own control
in one run:

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

So the honest shape of the thing has changed shape. The abstraction is
free where the path is named in code, and it costs an interpreter
where the path is chosen at run time — which is not a tax on optics,
it is what interpreting anything costs. The places where it is free
are known by measurement, and the places where it is not are written
down rather than hidden behind an `inline`.

## References

- <a id="ref-pickering-2017"></a>Matthew Pickering, Jeremy Gibbons, Nicolas Wu.
  *[Profunctor optics: modular data accessors.](https://doi.org/10.22152/programming-journal.org/2017/1/7)*
  The Art, Science, and Engineering of Programming 1(2), 2017 — the
  encoding this chapter uses, with the concrete and existential forms
  it replaces.
- <a id="ref-boisseau-2018"></a>Guillaume Boisseau, Jeremy Gibbons.
  *[What you needa know about Yoneda: profunctor optics and the Yoneda
  lemma.](https://doi.org/10.1145/3236779)* ICFP 2018 — the isomorphism
  between the existential and profunctor representations.
- <a id="ref-clarke-2020"></a>Bryce Clarke, Derek Elkins, Jeremy Gibbons,
  Fosco Loregian, Bartosz Milewski, Emily Pillmore, Mario Román.
  *[Profunctor optics, a categorical update.](https://arxiv.org/abs/2001.07488)*
  2020 — the general account, optics as morphisms between Tambara
  modules.
- <a id="ref-riley-2018"></a>Mitchell Riley.
  *[Categories of optics.](https://arxiv.org/abs/1809.00738)* 2018 —
  the categorical setting, and lawfulness as a coherence condition.
- <a id="ref-pastro-2008"></a>Craig Pastro, Ross Street. *Doubles for
  monoidal categories.* Theory and Applications of Categories 21(4),
  2008 — Tambara modules, of which `Strong` is the instance over
  products.
- <a id="ref-oconnor-2011"></a>Russell O'Connor. *[Functor is to lens as
  applicative is to biplate.](https://arxiv.org/abs/1103.2841)* 2011 —
  the van Laarhoven encoding and its constraint ladder.
- <a id="ref-foster-2007"></a>J. Nathan Foster, Michael Greenwald,
  Jonathan Moore, Benjamin Pierce, Alan Schmitt. *[Combinators for
  bidirectional tree transformations.](https://doi.org/10.1145/1232420.1232424)*
  TOPLAS 29(3), 2007 — where the lens laws come from, and why
  well-behavedness is the point rather than the accessors.
- <a id="ref-atkey-2009"></a>Robert Atkey. *[Parameterised notions of
  computation.](https://bentnib.org/paramnotions-jfp.html)* JFP
  19(3–4):335–376, 2009 — chapter 3's paper, and the other half of
  this chapter's punchline.
- <a id="ref-meijer-1991"></a>Erik Meijer, Maarten Fokkinga, Ross Paterson.
  *[Functional programming with bananas, lenses, envelopes and barbed
  wire.](https://maartenfokkinga.github.io/utwente/mmf91m.pdf)* FPCA
  1991 — catamorphisms, which is what the bottom-up rewrite is.
- <a id="ref-bray-2017"></a>Tim Bray (ed.). *[The JavaScript Object
  Notation (JSON) Data Interchange Format.](https://www.rfc-editor.org/rfc/rfc8259)*
  RFC 8259, 2017 — objects are unordered, which is the weaker reading
  one law needs.

### The families beyond lens, prism and traversal

- <a id="ref-roman-2020"></a>Mario Roman. *[Profunctor optics and
  traversals.](https://arxiv.org/abs/2001.08045)* MSc dissertation
  (Oxford, supervised by Gibbons), arXiv:2001.08045, 2020 — why the
  traversal did not fit the plain Tambara scheme, and the existential
  that makes it fit.
- <a id="ref-penner-2020"></a>Chris Penner. *[Intro to Kaleidoscopes:
  optics for aggregating data through
  Applicatives.](https://chrispenner.ca/posts/kaleidoscopes)* 2020 —
  the practitioner's account, with the `Reflector` class and the
  aggregate-then-classify example this chapter reproduces.
- <a id="ref-abousaleh-2016"></a>Faris Abou-Saleh, James Cheney, Jeremy
  Gibbons, James McKinna, Perdita Stevens. *[Reflections on monadic
  lenses.](https://arxiv.org/abs/1601.02484)* In *A List of Successes
  That Can Change the World*, LNCS 9600, 2016 — the effectful `put`,
  and their own verdict that combining bidirectional transformations
  with effects is "surprisingly subtle". The paper to read before
  putting a monad in a lens.
- <a id="ref-kmett-lens"></a>Edward Kmett et al. *[lens: Control.Lens.Iso.non,
  Control.Lens.At.](https://hackage.haskell.org/package/lens)* — `non`,
  `at` and `ix`: the pair that makes "create the missing parent"
  lawful, which is stage 6 of `specs/optics.md`.

### Arrows, monoids, and what a profunctor is for

- <a id="ref-rivas-2017"></a>Exequiel Rivas, Mauro Jaskelioff.
  *[Notions of computation as monoids.](https://www.fceia.unr.edu.ar/~mauro/pubs/Notions_of_Computation_as_Monoids.pdf)*
  JFP 27, 2017 — monads are monoids in endofunctors, applicatives are
  monoids for Day convolution, arrows are strong monoids in
  profunctors. The one table this chapter's neighbours all live in.
- <a id="ref-hughes-2000"></a>John Hughes. *[Generalising monads to
  arrows.](https://doi.org/10.1016/S0167-6423(99)00023-4)* Science of
  Computer Programming 37, 2000 — where arrows come from, and the
  first-class notion of a computation with a static part.
- <a id="ref-kmett-machines"></a>Edward Kmett. *[Moore for
  less](https://www.schoolofhaskell.com/user/edwardk/moore/for-less)*
  and the `foldl` library's `Control.Foldl` / `Control.Scanl` — a
  Moore machine is a profunctor with an Applicative and a Comonad, a
  scan is a Category. okay's `Aggregator` and `Scan` are those two,
  written before the names were noticed.
- <a id="ref-jaskelioff-2015"></a>Mauro Jaskelioff, Russell O'Connor.
  *[A representation theorem for second-order
  functionals.](https://doi.org/10.1017/S0956796815000088)* JFP 25,
  2015 — the traversal's representation, and the other half of why
  `wander` is the primitive it is.

### Origami, recursion schemes, and the shape of the residual

- <a id="ref-mitchell-2007"></a>Neil Mitchell, Colin Runciman.
  *[Uniform boilerplate and list processing.](https://ndmitchell.com/downloads/paper-uniform_boilerplate_and_list_processing-30_sep_2007.pdf)*
  Haskell Workshop 2007 — the `uniplate` primitive: a node's immediate
  children and how to put them back, which is a traversal of the
  self-similar children. Every recursion scheme in the library is
  derived from that one optic, and `transform` is the bottom-up
  rewrite kept carefully separate from it.
- <a id="ref-gibbons-2003"></a>Jeremy Gibbons. *[Origami
  programming.](https://www.cs.ox.ac.uk/jeremy.gibbons/publications/origami.pdf)*
  In *The Fun of Programming*, Palgrave, 2003 — folds and unfolds as
  the structuring principle, which is the discipline the previous
  entry mechanises.
- <a id="ref-mcbride-2001"></a>Conor McBride. *[The derivative of a
  regular type is its type of one-hole
  contexts.](http://strictlypositive.org/diff.pdf)* Unpublished, 2001 —
  the residual `M` in `(S -> M (x) A, M (x) B -> T)` is a derivative.
- <a id="ref-huet-1997"></a>Gerard Huet. *[The
  zipper.](https://doi.org/10.1017/S0956796897002864)* JFP 7(5), 1997 —
  the context carried beside the focus; a lens's residual made into a
  data structure.

### Effects, grades and indices

- <a id="ref-katsumata-2014"></a>Shin-ya Katsumata. *[Parametric effect
  monads and semantics of effect systems.](https://doi.org/10.1145/2535838.2535846)*
  POPL 2014 — graded monads: a monad indexed by a monoid of effects,
  which is what an effect row is.
- <a id="ref-orchard-2020"></a>Dominic Orchard, Philip Wadler, Harley
  Eades III. *[Unifying graded and parameterised
  monads.](https://arxiv.org/abs/2001.10274)* MSFP 2020 — grades and
  Atkey's parameters as one thing, which is why `State.zoom` and an
  effect row belong in the same chapter.
- <a id="ref-grenrus-2017"></a>Oleg Grenrus. *[Indexed profunctor
  optics.](https://oleg.fi/gists/posts/2017-04-26-indexed-poptics.html)*
  2017 — the index as a parameter of the profunctor itself, which is
  where a path or a key belongs when it should travel with the optic
  rather than beside it.

---

← [9 · Conditions: resumable exceptions](09-conditions.md) · [Contents](index.md)
