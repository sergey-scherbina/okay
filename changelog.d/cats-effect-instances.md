## cats-effect-instances — an okay program as cats-effect's `F`

Operator ask, 2026-10-02 (the cats-depth audit's main gap). Code written
`F[_]: Async`/`Concurrent`/`Temporal` runs at `CatsEffect.Program`, an
opaque `A ! CatsFx + Async`: okay's binds, cats-effect's primitives as
one effect in the row, run by `CatsEffect.toIO` as ONE IO fiber so
masking and cancellation are IO's own. cats-effect-laws' `AsyncTests`
(110 properties) hold over programs generated through our instance; a
Resource releases on cancel, `canceled` defers to `poll`, race/Ref and
timeout work, okay's own `await` cancels through its canceller. Refuted
on the way: a `NotGiven` row test on the default program monad (broke
cats' unification), a nearer import (still ambiguous), and `foldMap` as
the runner (its trailing `flatMap(pure)` broke one law).
specs/cats-effect-instances.md, docs/interop-classes.md.
