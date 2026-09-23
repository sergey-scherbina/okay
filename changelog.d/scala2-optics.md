## scala2-optics - okay-optics from Scala 2.13: Lens, Prism, Affine, Traversal, Iso

Stage 15.6 of specs/scala2-facade.md (operator: "делай всё что возможно
чтобы работало в скале 2").

- Probed first: nothing of okay-optics reaches Scala 2 (constraints are
  type lambdas over intersections, the kinds are top-level aliases, a
  traversal is a polymorphic function type, every operation an `inline`
  extension).
- The new module okay-scala2-optics has the five kinds as monomorphic
  Scala 2 classes, each a shell over okay's own optic. Building calls
  okay's constructor, `andThen` is okay's composition (15 overloads, one
  per pair, each answering the lattice's kind), and get/set/modify/
  preview/toVector are okay's operations, so the laws are okay's.
- A prism's `review` and an iso's `reverseGet` travel beside the optic
  and are composed explicitly. A first draft derived them by `set` on a
  `null` source, which is wrong for any preview that inspects its
  argument; it was replaced before the first test ran.
- `TestOpticsFromScala2` has 5 tests: lens laws on a composed lens, a
  subtype prism then a lens, lens-then-prism as an affine, a traversal
  through `each` and one from two functions, and an iso then a prism.
- Docs: section 8o of docs/scala2.md (copied from the probe, citing
  Pickering, Gibbons and Wu 2017), the module page, the API reference,
  and the spec's stage 15.6.
- Gated: the full matrix, GREEN (6870 tests, cold, no warnings), on the
  tree before master gained 0958374a/1077747b (ts-types-ts-to-scala,
  okay-codec). After rebasing onto them, the re-gate was scoped to where
  the two lanes meet, `okayScala2Probe/test; okayCodecJVM/test` with the
  probe compiled cold: GREEN, 398 tests. That lane's own gate covered
  every other module against its codec change.
