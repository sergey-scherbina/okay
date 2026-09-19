## pwc-sql-seam - Sql.query as Source[Chunk[Vector[SqlValue]]]

Stage 2 of producer-to-writer-carrier, the sql family in one lane —
one seam, three drivers, one typed layer, six drains, and every test
that walked the stream by hand. `Sql.query` (okay-sql) was
`Chunk[Vector[SqlValue]] ! (Produce + Async)`, a chunk in the answer
position; it is `Source[Chunk[Vector[SqlValue]]]` now, and so are
`JdbcSql`, `R2dbcSql`, `PgSql` (each emit `effect[F, Unit](Writer(c))`
where it was `effect[F, Chunk[..]](c)`, the end `pure(())` where an
empty chunk stood in for it), `JdbcInterop.query`, `Typed.rows`/
`rowsOf` (`Source[Chunk[Either[Bad, A]]]`, the decode loop on
`Writer.uncons`) and `Typed.Db.query`.

The drains that `Producer.concat` served — jdbc `Poll`, `SqlStore`,
`Migrate`, rag `PgVector` — call its writer twin, `Source.concat`
(new, okay-stream Source.scala: `Writer.collect(...).map(_._1.flatten)`,
the one place the E092 caveat is silenced for this shape); `Writes.
countRows` and outbox `Rows.all` fold with `Writer.fold` over a
`Fold` given. Twenty test files carried their own copy of one
hand-rolled `collectChunks`/`drain` over `Produce + Async`, a cast per
chunk; each is now one line — the writer stream's own `iterator` for
the eager JVM walkers, `Source.concat` for the program-shaped ones
(TestPgNode on JS among them), and a pure `Say`-matching walk in
TestSqlPure, which runs on all three platforms and asserts no Async
happened. Two consumers the survey missed, okay-cache's
`TestWriteThrough` and okay-delta's `TestDelta`, were found by the
repo-wide `Test/compile` and moved the same way.

Docs: specs/sql.md, specs/jdbc.md, docs/modules/okay-jdbc.md, and two
mentions in docs/modules/okay-persist.md the previous lane left.

Files: okay-stream Source.scala; okay-sql Sql.scala, Typed.scala;
okay-jdbc JdbcSql, JdbcInterop, Poll, SqlStore, Migrate, Writes;
okay-r2dbc R2dbcSql; okay-pg PgSql; okay-rag PgVector; okay-outbox
Rows; tests in okay-sql, okay-jdbc, okay-pg, okay-r2dbc, okay-cache,
okay-delta, okay-docs-dynamo; specs and docs as named.
