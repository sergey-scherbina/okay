# okay-pg

> The Postgres v3 wire, natively: the direct road the Sql seam
> exists for — no java.sql, no driver dependency, the protocol
> itself behind the same trait, so the typed layer
> (rows/verify/params/transact) runs over it unchanged
> (specs/sql.md).

Depends on: the core and `okay-sql`. JVM leg (a blocking socket
behind `Async.Run` — virtual threads make it honest); the Node leg
arrives with a consumer.

## Guide

**Connect.** `PgSql.connect(host, port, user, password, database)`
— startup then SCRAM-SHA-256, including the half most clients
skip: the server's signature is VERIFIED (mutual authentication),
and a server nonce that does not extend ours refuses. md5 and
cleartext are deliberately not spoken.

**Portals are the fetch-size story.** The extended query protocol
streams natively: `Execute(maxRows)` + `Flush`, rows until
`PortalSuspended` — one chunk; `CommandComplete` ends it. Chunked
reads at constant memory with no driver in between, which is the
whole point.

**The typed layer just works.** `Typed.rows/verify/update/transact`
over this driver as over any other; `describe` consults
`pg_attribute` for nullability (RowDescription does not carry it),
so `verify` is as strict here as on JDBC. The acceptance test runs
one typed program over PgSql and JdbcSql/H2 and asserts ONE equal
answer — only the SQL strings differ (`$n` vs `?`), which
bind-don't-model already decided belongs to the dialect.

**Errors survive.** A backend error is drained to `ReadyForQuery`
before the throw, so the session keeps working; `cancel()` (the
region's sync brake) rolls back on the spot.

One family, one implementation: everything speaking the pg wire —
CockroachDB, TimescaleDB, Materialize, Neon, pgvector — is now a
connect call away.
