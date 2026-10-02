## cats-kernel-bridge — Semigroup, Monoid, Group across cats and okay

Operator ask, 2026-10-02 (the cats-depth audit, backlog okay-cats). Both
`Validated` instances of okay-cats now combine with EITHER library's
semigroup (`Combine`, okay's first, in the default import); the general
bridges are `FromCatsKernel` / `ToCatsKernel`, one import each. Folding
them into FromCats was tried and refuted: beside `okay.given` it made
`okay.Group[Int]` ambiguous. cats-kernel-laws' monoid rules on a bridged
okay monoid. specs/cats-kernel-bridge.md.
