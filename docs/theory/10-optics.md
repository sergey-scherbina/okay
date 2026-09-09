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

## Where the theory said no

Two refusals, both recorded in `specs/optics.md` and both instructive.

**A creating lens is not a lens.** A form starts from a partial value,
so the router that folds an edit into it must create missing parents
on the way down. Written as a lens on JSON that is
`get(absent) = empty; set(v)` — and it breaks GetPut: `set(get(s))(s)`
inserts an empty object where there was none, so it is not `s`. The
lawful decomposition is the `at`/`ix` pair every optics library
arrives at: `at(name)` has an `Option[Json]` in the focus and is a
genuine lens (`JsonOptic.scala:38`), and `field(name)` is
`at(name) ∘ some` (`JsonOptic.scala:53`), an affine that refuses where
the field is absent. Okay's form router therefore keeps its own walk,
and a test asserts the two agree wherever both are defined — rather
than making the optic dishonest to shorten a router.

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
1.5x, so optics are a convenience and derivation layer in this
library and not a hot-path primitive; the UI's patch application keeps
its own navigation, and the optic that names the same path exists
beside it, tested against it.

That is the honest shape of the thing. The abstraction is not free,
the places where it is free are known by measurement, and the places
where it is not are written down rather than hidden behind an
`inline`.

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

---

← [9 · Conditions: resumable exceptions](09-conditions.md) · [Contents](index.md)
