# okay-jdbc

> Result sets as chunked async streams, constant memory at any size;
> connections under the Resource region — closed on the scope's end,
> handled aborts included. And, since sql-seam: the first DRIVER of
> the `Sql` seam (docs/modules/okay-sql.md) — `JdbcSql(conn)` puts
> the typed layer (rows/params/verify/transact) on top of any JDBC
> connection, exercised on H2 as a user with no DDL rights.

Depends on: `okay`, `okay-sql` (JVM); tests run on H2.

## Guide

**Streaming reads.** `query(conn, sql)(row)` opens the statement in
its first Async operation and then reads FETCH-SIZE rows per chunk,
each read inside one Async op — a virtual thread parks in the driver,
memory stays constant for any result size. The statement closes
itself at exhaustion.

**Scope the CONNECTION, not the statement.** An abandoned stream (one
you stopped pulling) leaks its statement by construction — the region
that matters is the connection's: `connection(url)` lives under
`Resource`, and the region closes it at the scope's end, on handled
aborts (`runEither` INSIDE the region), and on mid-step exceptions.
Statement leakage ends with the connection.

**Batched writes.** `batch(conn, sql)(bind)(chunk)` — one chunk, one
`executeBatch`: the chunk is the unit of writing, as everywhere in
the family.

## Tutorial

```scala
import okay.jdbc.JdbcInterop.*

val program = Resource.run:
  for
    c <- connection("jdbc:h2:mem:demo")
    _ <- !.widen(async(execute(c, "create table nums(n int)")))
    _ <- !.widen(batch(c, "insert into nums values (?)")(
           (st, n: Int) => st.setInt(1, n))(chunk))
    sum <- !.widen(foldRows(query(c, "select n from nums")(_.getInt(1)))(
           using Fold.sum[Int]))
  yield sum
// the connection is closed here — normal exit, abort, or exception
```

## API reference

| member | signature | meaning |
|---|---|---|
| `connection` | `(url, user?, pass?) => Connection ! Resource...` | a connection in the region |
| `query` | `(conn, sql, fetchSize?)(row: ResultSet => A) => Chunk[A] ! (Produce + Async)` | fetch-size rows per chunk |
| `batch` | `(conn, sql)(bind)(chunk) => Unit ! Async` | one chunk, one executeBatch |
| `execute` | `(conn, sql) => Unit ! Async` | DDL and one-offs |

## Gotchas

- Never scope a statement outside its connection's region: abandoned
  streams leak statements BY DESIGN (documented trade-off), the
  connection region is the cleanup boundary.
- `runEither` goes INSIDE `Resource.run` (the region must see the
  abort to release); `!.widen` builds the union program.
- The driver's own fetch-size semantics apply (some drivers ignore
  it without further connection flags — check yours).
