## foreign-effects-in-tree — foreign values as effects in the tree, ZIO's `for`, and row variance answered

Operator, 2026-10-04: keep a cats/ZIO/kyo value in the tree as an effect
named as what it wraps; a ZIO-natural `for`; is a covariant `Free` needed
at all? Probes and a spec, no main code (specs/foreign-effects-in-tree.md).

- The foreign type IS the row member (`Int ! IO`, `Int ! ZIO[Db, DbErr, *]`,
  `Int ! UIO`); only kyo gets an alias, `Kyo[S]`. A handler reading `R`
  and `E` off the row by subtyping answers `ZIO[Db & Log, AppErr, Int]`,
  the type ZIO's own `for` gives.
- A `for` over different effects comes from a bind writing its row as a
  union, which works under today's invariance (on the real core: real
  handlers infer their rest exactly, abstract rows compile). Wiring it
  in as `flatMap` is refuted twice: as an extension (a lexical given's
  `flatMap` wins inside package okay; `Monad` instances resolve to
  themselves) and with a `Join` given (dotty's row-membership-crash).
- `Free` stays invariant: covariance widens a handler's inferred rest to
  `Object & Enum` once two members remain; `Precise` fixes it only as an
  experimental import that infects callers.
- Backlog: foreign-effects-members (stages 1, 2, 4), union-bind-flatmap.
