## optics-arrows-effects — the questions the closed arc leaves (specs/optics.md stage 12, operator's ask 2026-09-18)

The operator asked what optics, profunctors and arrows can do together
with the monads, applicatives, effects and continuations already here,
and what is missing for that to be convenient. Stage 12 of the spec
records what the tree ALREADY answers (one traversal runs at
`Validated`, `Par`, `Static` and an effect row with no code in the
optics; `PState.zoom` is the lens-meets-continuation seam; `Arrow` has
one instance, `Mealy`), and leaves these. Each is one lane with one
test; none is promoted until its spec item is read first.
