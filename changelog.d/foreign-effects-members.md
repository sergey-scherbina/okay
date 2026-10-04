## foreign-effects-members — IO, ZIO and kyo values as effects of the tree

Operator, 2026-10-04 (specs/foreign-effects-in-tree.md, stages 1 and 2).

- The row member is the foreign type itself: `Int ! IO`, `Int ! ZIO[Db, DbErr, *]`
  (or ZIO's aliases), `Int ! <[*, S]` for kyo. `perform` builds an IO/ZIO operation,
  `KyoEffect.perform` a kyo one. Building runs nothing; the handler chooses the runtime.
- `p.toIO`: IO and okay's `Async` as ONE IO. `p.toZIO`: a row of ZIO members as one
  ZIO, `R` and `E` read off the row by subtyping — `ZIO[Db & Log, AppErr, Int]`, the
  type ZIO's own `for` gives. `provideEnvironment` and `mapError` on such a row.
- `p.via[M]`: each `M` step lowered by the library's `ForeignEffect[M]` (`Async` for
  IO and Task), the rest of the row kept; one extension in okay-async, so the name does
  not split per module. `asOkay` agrees with `perform` then `via[IO]`.
- kyo rows are lowered whole (`KyoEffect.run`): `A < S` is opaque and no runtime test
  tells its operations apart.
- Tests: TestIOMembers 6, TestZioMembers 5, TestKyoMembers 1. Docs: docs/effect-interop.md,
  "A foreign value as an effect of the tree". Left: backlog foreign-effects-members-rest.
