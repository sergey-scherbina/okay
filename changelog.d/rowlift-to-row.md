## rowlift-to-row - `okay.RowLift` is `okay.Row`: `Row.at`, `Row.In`, `Row.Sub`, `Row.Has`, `Row.into`

- Operator: "в скале 3 переименуем RowLift в просто Row, тогда все будет
  так же" as okay2's `Row`. `src/main/scala/RowLift.scala` is
  `Row.scala`, `object RowLift` is `object Row`, and every Scala 3
  source, test and doc follows (99 files). `RowLift` stays as a plain
  alias, `val RowLift: Row.type = Row`, so okay-watch's
  `import okay.RowLift.plus` compiles unchanged. `RowLiftBenchmark`
  keeps its name: `src/jmh/history.tsv` cites it.
- The risk that was measured first — a new top-level name in package
  `okay` shadowing another (the Gen/Uid.Gen E177 of the generators arc)
  — showed up as 33 `E226` warnings, all one kind: a TYPE PARAMETER
  named `Row` (Staged.scala's `Handled[Row[+_], R, A]` and `Stager`,
  okay-direct's `staged`/`stagedImpl`) now shadows `okay.Row`. Renamed
  to `F` and `Sig`. A local `type Row = …` alias, which the docs and
  tests write everywhere, does not warn; `okay.sql.Row` lives in its own
  package and is unaffected. A user's own type parameter named `Row`
  under `import okay.*` gets the same warning.
