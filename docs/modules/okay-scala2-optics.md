# okay-scala2-optics

okay-optics for **Scala 2.13**. okay's optics are profunctor optics, built
from Scala 3 features that do not reach Scala 2. Here the five kinds are
Scala 2 classes. Each is a shell over okay's own optic, so building,
composing and every operation are okay's:

| | |
|---|---|
| `Lens[S, A](get, set)` | a part that is always there: `get`, `set`, `modify` |
| `Prism[S, A](preview, review)`, `Prism.subtype`, `Prism.some` | one case of a sum: `preview`, `review` |
| `Affine[S, A](preview, set)` | at most one part (a lens then a prism) |
| `Traversal[S, A](parts, rebuild)`, `Traversal.each` | every part, in order: `toVector`, `modify` |
| `Iso[S, A](to, from)` | two views of one thing: `get`, `reverseGet` |

`andThen` composes any two, answering the kind the optics lattice gives.

The walkthrough is section 8o of
[okay from Scala 2.13](../scala2.md#8o-optics-lens-prism-affine-traversal-iso), and the signatures are in
[okay-scala2](okay-scala2.md#api-reference).
