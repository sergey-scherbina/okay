## typed-zipper-elem-siblings - `left`/`right` where the types allow it

The operator asked for "the ten lines" the zipper spec had twice set
aside (2026-09-23). On FIELDS the refusal stands — the field beside
`customer` is a `Vector[Line]`, not a `Customer`. On the ELEMENTS of a
`Vector` focus every sibling has the focus's type, and that is built:

- `TypedZipper.Elem[S, B, Z]`: the frame `at(i)` now answers, which
  remembers its index — `up`, `index`, `left`, `right` (a sideways
  move commits the focus to the parent first, so an edit survives it),
  `asAffine` pointing at the element. Eleven lines.
- `TestTypedZipper` 13 (+2): ends answer `None`, `right.left` is the
  identity, a walk without edits keeps `root eq` the input, an edit
  survives `right`, `asAffine` and `at(i)` agree with the walk.
- specs/zipper.md stage 4; theory ch. 10 and guide §10 gain the
  sentence.

Gate `affected master` green, no warnings.
