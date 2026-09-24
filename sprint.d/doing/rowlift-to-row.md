- [ ] rowlift-to-row — operator 2026-09-24: "в скале 3 переименуем RowLift
      в просто Row, тогда все будет так же" as okay2's `Row`. `object
      RowLift` (src/main/scala/RowLift.scala: at, plus, In, Sub, Has,
      into) becomes `object Row`; every Scala 3 source, test, benchmark
      and doc follows; `RowLift` stays as a plain alias of `Row` so
      okay-watch's `import okay.RowLift.plus` compiles unchanged. The
      risk measured first, by a full compile: a new top-level name in
      package `okay` has shadowed nested names before (Gen/Uid.Gen,
      E177), and `okay.sql.Row` exists. (2026-09-24)
