## stack-safety-okay2-catch-up - a recursive row type is refused, not a stack overflow; okay2's catch-up rows closed

The ten okay2 rows the stage-9 guard named after the inventory, audited —
and one real defect found in BOTH cores on the way. `Typed.shapeOf`
matched the Schema tree directly, and a derived schema of a recursive
type (`Tree(label, kids: Vector[Tree])`, or `A ↔ B`) is a cycle of lazy
thunks: `Query.field[Tree, String]("label")` was a StackOverflowError
where "not row-shaped" was the promise. `TestTypedRecursive` is red first
in okay-sql and okay2-sql; a product met again on its own path is refused
by name at construction now, and every other Typed walk is over the
finite Shape that builds.

The rest are written bounds, on the okay2 rows and their Scala 3 twins
(which had none): `Typed.fits` per level of a SqlType the row's type
gives; jdbc `valueOf`/`arrayOf` per dimension of a database array, which
the DDL declares and Postgres caps at MAXDIM = 6; `jdbcOf` per level of a
parameter the program built; SparkSchema's four walks per level of a
`ColType` whose recursive products Columns already cuts to one column.

The fs2 and zio interop `again` loops were never stack recursion: they
recurse through fs2 `++`/`flatMap` and `ZIO.flatMap`, the library's own
lazy bind. `scripts/recscan.py` now treats fs2 `Stream`, `ZIO` and cats
`IO` binds as deferring, and no longer reads an implicit evidence fetched
between a thunk and its call (`NotGiven.default`) as the thunk's
consumer. Five okay2 rows are paid and deleted; the Scala 3 inventory is
unchanged under the new rule (307 rows, checked over the whole tree).
No UNAUDITED row is left in okay2; the core has one, `ContMacro.rewrite`.
