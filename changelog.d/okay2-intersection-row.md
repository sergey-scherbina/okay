## okay2-intersection-row - okay2's row is an intersection: `+` is `with`, and the row witnesses are gone

- Operator: "начнем с исправления в окей2 алиаса +". `+` was a sealed
  trait that neither commuted nor associated, so okay2 carried
  `Member` (six rules), `Sub`, `NotPure`, `Remove` + `Aux`, coercing
  `.at`/`.plus`/`.bind`, and an `…At` twin of every handler. Now
  `type +[F, G] = F with G`, `Free[-R <: Row, +A]`, `type Pure = Row`
  (spec stage 8): widening is subtyping, the order of a row does not
  matter, and a handler takes `Free[State[S] with R, A]` and infers the
  rest `R` — all six orders over a three-effect program with no
  annotation. The witnesses are deleted; `.at` is the identity.
- Three Scala 2 facts decided the shape, each measured: an
  intersection's `#Op` is its LAST parent's (reading an operation at it
  is a ClassCastException), so `Inject` holds its operation as `Any`
  and a typed view exists only at one signature (`Split.split`,
  `splitBoth`, `only`; `Handler.Of`, `Into.Of`); scalac does not look
  through `!`/`+` to solve a row variable, so every row-generic
  parameter is spelled with `Free`; and an implicit rule over `F + G`
  diverges on every type, so `Replayable` is derived by a small
  blackbox macro over the row's parents (same whitelist, a user effect
  still refused) and `Into.union`/`IntoZ.union` are explicit.
- `Handler`/`Into`/`IntoZ` stay invariant: covariant, the documented
  `implicit val h: Handler[F + G] = Handler.union[F, G]` resolved to
  itself.
- 303 okay2 tests green, cold — including stage 7 (Choice, Delim.Stacked, SharedOnce), which landed on the old row while this lane was open and was moved onto the new one here. `TestRow` rewritten to pin the new
  discipline (six orders, subtyping both ways, the `#Op` trap, no
  `TypeableK` for a row); docs/okay2.md sections 2-4 and 8 rewritten,
  every example still pinned by `TestDocSnippets`.
