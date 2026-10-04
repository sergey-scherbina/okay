- [ ] foreign-effects-members — specs/foreign-effects-in-tree.md stages 1,
      2 and 4 (operator, 2026-10-04). cats `IO`, ZIO (`ZIO[R, E, *]` and its
      aliases) and kyo (`Kyo[S]`) as row members of their own name;
      `toIO`, `toZIO` (R and E read off the row by `Row.Sub` — probe D
      gave `ZIO[Db & Log, AppErr, Int]`, ZIO's own `for` type), `viaAsync`;
      `asOkay` as perform + viaAsync; ZIO's narrowing handlers by ZIO's
      names (`provide`, `catchAll`, `mapError`). Stage 4, `direct`'s `.?`
      putting an IO/ZIO in the tree instead of awaiting it, is a semantic
      change: ask the operator first.
