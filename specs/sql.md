# The relational seam: SQL without a mandatory JDBC

## Overview

specs/jdbc.md got the CONTRACT right — bind-don't-model, Schema as
the row and parameter codec, verify-at-startup, the transaction
region, the no-DDL posture — and made one coupling that must be
undone while it is still cheap: the contract is written against
`java.sql.Connection`. That binds the whole typed layer to the JVM
and to one (blocking) driver architecture, when nothing in the
contract itself is JVM- or JDBC-shaped.

This spec cuts the seam one level lower. The typed layer — rows,
params, verify, transact — is defined ONCE against a small driver
trait (`Sql`), and drivers plug in underneath:

- **JDBC** — the first driver, JVM, wrapping the blocking calls the
  way `JdbcInterop` already does. It keeps everything specs/jdbc.md
  promised and stays the right choice wherever a JDBC driver is the
  best-maintained way in (warehouses, DuckDB, H2, Oracle...).
- **Native wire protocols** — the direct road the seam exists for:
  a database's own protocol spoken over this stack's cross-platform
  Async I/O (the transport okay-cluster proved on JVM and Node).
  Postgres first, because its v3 protocol is open, well-documented,
  and one implementation covers a whole family (Postgres,
  CockroachDB, TimescaleDB, Materialize, QuestDB, Neon, pgvector —
  the specs/data.md vector adapter arrives on this road too).
- **R2DBC** — the JVM reactive driver ecosystem, as an OPTIONAL
  interop hatch behind the same trait, with the honest note below
  on what it does and does not buy.

The precedent is already house practice three times over: okay-http
serves one contract over NIO/Netty/Jetty backends; okay-ui renders
one tree over Host backends; okay-persist stores one log over
memory/file/interop engines. SQL gets the same shape: one contract,
many roads in.

## The driver seam

Small on purpose — a driver is a way to move statements, values and
row frames, nothing more. Everything smart (typing, totality,
verify, the region) lives ABOVE it, written once:

```scala
package okay.sql

/** neutral values — what a row cell or a parameter can be.
 * The driver maps these to its wire/API; Schema binds case classes
 * to them. No java.sql types anywhere above the driver. */
enum SqlValue:
  case Null
  case Bool(v: Boolean)
  case I32(v: Int); case I64(v: Long); case F64(v: Double)
  case Text(v: String)
  case Bytes(v: Array[Byte])
  case Num(v: BigDecimal)              // numeric/decimal, EXACT (pg-scalar-types)
  case Arr(elems: Vector[SqlValue])    // a SQL array (pg-composite-decode)
  case Row(fields: Vector[SqlValue])   // a composite / ROW() value

/** the column types verify speaks; Arr/Row mirror the values, so a
 * Vector field or a nested case class has a column type to be
 * checked against (sql-schema-composite) */
enum SqlType:
  case Bool, I32, I64, F64, Text, Bytes
  case Num                             // numeric/decimal: exact, arbitrary precision
  case Other(name: String)             // a vendor type, by name
  case Arr(elem: SqlType)              // an array column
  case Row(fields: Vector[SqlType])    // a composite column

/** column description, the verify input — driver-neutral */
final case class Col(label: String, tpe: SqlType, nullable: Boolean)

/** the driver: statements in, metadata and row frames out.
 * Everything speaks Async — a wire driver is async by nature, the
 * JDBC driver wraps blocking calls in Async.Run (virtual threads
 * make that honest on the JVM). */
trait Sql:
  def describe(sql: String): Vector[Col] ! Async
  def query(sql: String, params: Vector[SqlValue])
  : Chunk[Vector[SqlValue]] ! (Produce + Async)      // frames, chunked
  def update(sql: String, params: Vector[SqlValue]): Long ! Async
  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async
  def begin(isolation: Isolation): Granted ! Async
  def commit(): Unit ! Async
  def rollback(): Unit ! Async
  /** the emergency brake — the ONE deliberately synchronous method:
   * called from a Resource finalizer when a transaction scope
   * unwinds without a commit (an exception, a handled abort), and
   * finalizers are sync by design. After a commit it is a no-op.
   * The JDBC driver rolls back on the spot; a wire driver may
   * simply close the connection — every serious server rolls back
   * an abandoned transaction, which is why this is honest. */
  def cancel(): Unit
```

Around the trait, the small vocabulary the sketch above implies:
`SqlType` mirrors `SqlValue`'s cases (plus `Other(name)` for vendor
types verify can then name), `Isolation` is ReadCommitted /
RepeatableRead / Serializable, and `begin` answers
`Granted(requested, granted)` — engines interpret levels
differently, so the caller sees what was actually granted and can
refuse a downgrade rather than assume.

The typed layer of specs/jdbc.md re-targets to this seam verbatim:
`rows[A: Schema]` decodes `Vector[SqlValue]` frames by label through
`Col`s; `verify[A]` compares `Schema[A]` against `describe`;
`transact` is the same region over begin/commit/rollback with the
granted isolation exposed; params bind from a product to
`Vector[SqlValue]`. Nothing the caller sees changes except the
import — and the platforms it runs on.

## The drivers

- **okay-jdbc becomes the JDBC DRIVER of this seam** (plus its
  existing raw streaming interop, unchanged for those who want
  ResultSet in hand). JVM; blocking calls on virtual threads via
  `Async.Run` — which is the honest performance story: on JDK 21+
  a parked virtual thread costs what a reactive callback costs,
  and the mature JDBC drivers are the most battle-tested code in
  this landscape. This driver is not a legacy shim; it is the
  right road for half the classes in specs/data.md.
- **okay-pg — the Postgres wire protocol, natively.** Startup +
  SCRAM-SHA-256, the extended query protocol (Parse/Bind/Execute
  with portals — which IS chunked streaming at the protocol level,
  our fetch-size story without a driver in between), and COPY for
  the bulk-load posture. Cross-platform over the Async transport:
  JVM and Native, Node where a consumer appears — a Native binary
  or a Node process talking to Postgres with NO java.sql anywhere.
  This stack already speaks SSE, JSON-RPC, MCP and (planned) RESP;
  the Postgres protocol is bigger but of the same nature, and one
  implementation unlocks the whole pg family including pgvector.
  TLS rides the one transport seam (specs/tls.md) — the driver owns
  only pg's SSLRequest handshake dance and speaks the `sslmode`
  vocabulary that spec adopts stack-wide, `verify-full` by default.
  **`PgTarget`** (`okay-pg`, JVM leg — `TlsConfig` is JVM-only)
  parses the connection string operators actually write,
  `postgres://user:pass@host:port/db?sslmode=…&sslrootcert=…`, into
  `(host, port, user, password, database, tls: Option[TlsConfig])` —
  pure, total (`Either[String, PgTarget]`, never a throw), so "does
  this URL configure TLS the way I meant" is testable with no
  server. Extracted 2026-09-02 from `okay-demo` (round-two reusable-
  module pass, BACKLOG.md) — it had zero demo dependencies from the
  start, a pure move to sit beside the driver it configures.
- **okay-r2dbc — the hatch, honestly framed.** R2DBC (Reactive
  Relational Database Connectivity — likely what "rjdbc" refers
  to; RJDBC proper is an R-language package and not relevant here)
  is a JVM API where drivers are non-blocking by construction.
  What it buys us: maintained async drivers for engines whose wire
  protocol we will not write (MSSQL, Oracle, MySQL before okay-my
  exists). What it does not buy: on virtual threads, JDBC-behind-
  Async.Run already fails to block the carrier — so R2DBC is not a
  performance unlock, it is a driver-availability unlock. Filed
  low-priority; the operator named it needed on 2026-09-02 and it
  landed as okay-r2dbc (the box below).

## What changes where (the surgery, minimal)

- specs/jdbc.md: the contract sections (typed rows, verify,
  transact, no-DDL posture, the write bridge) stay THE contract —
  restated to bind against `Sql`; the module section names
  okay-jdbc as the first driver. The raw `JdbcInterop` streaming
  stays as-is.
- specs/data.md: the "rows + SQL" seam row points here; the lake
  (DuckDB), warehouse and pgvector roads note which driver serves
  them (DuckDB/warehouses → JDBC driver; pgvector → either, pg
  wire preferred).
- specs/persist.md: untouched by this seam — but the same openness
  question is answered there in the same turn (see persist-wire in
  the staging note): the log's own wire protocol, so a non-JVM
  client reaches a persist node directly, no JDBC and no JVM in
  between. Store engines and Sql drivers are the two halves of
  "максимально открытый": open above (any consumer, any platform)
  and open below (any engine, any road in).

## Behavior

- [x] the typed layer (rows/params/verify/transact) compiles and
      tests against `Sql` with ZERO references to java.sql above
      the driver line (asserted structurally: okay-sql cross-builds
      for JS and Native, platforms where java.sql does not exist —
      a stronger check than any import grep)
- [x] the JDBC driver passes the specs/jdbc.md behavior list
      through the seam (H2, no-DDL user; the write-bridge and
      poll-source items ride their own filed slugs)
- [x] the pg driver: startup + SCRAM auth, extended-protocol query
      with portal streaming at constant memory, params and rows
      through SqlValue, transact begin/commit/rollback with granted
      isolation — against a real Postgres (live-suite pattern:
      skips where the endpoint is absent) — proven on Postgres
      17.11: SCRAM-SHA-256 with the server-signature verification,
      500 rows at 64-row portal Executes, a bad password refuses,
      an error reaches quiet and the session survives
- [x] the R2DBC hatch (sql-r2dbc, operator: needed): `R2dbcSql(conn)`
      over any `io.r2dbc.spi.Connection` — query as PULLED chunks (a
      demand-driven Subscriber: request(fetchSize), park behind
      Async.Run until they arrive), update/batch summing every
      Result's count, transact with the granted isolation read back,
      the same SqlValue/Col vocabulary; the typed layer runs over it
      unchanged (rows/params/verify) on H2 through r2dbc-h2 and on the
      dockerized Postgres through r2dbc-postgresql. Honestly framed
      where the SPI is thinner than JDBC: metadata exists only WITH a
      row, so `describe` runs the statement for one row and reads its
      metadata; a parameterised statement or an empty result describes
      as EMPTY and verify names every column missing — stated, not
      guessed. Nullability is the driver's word: H2's says it,
      r2dbc-postgresql answers UNKNOWN for every column (pg's
      RowDescription carries none and the hatch has no catalog road
      as okay-pg does), so verify names each non-Option column there
      as "nullable" rather than inventing a promise. Composite params
      are refused as on JDBC
- [x] the pg driver decodes COMPOSITE / ROW() and ARRAY values into
      structure, not raw text (pg-composite-decode): an array whose
      element OID the driver knows (int2/4/8, text/varchar, bool,
      float4/8, numeric, bytea) parses to `SqlValue.Arr` with each
      element typed via the ordinary `valueOf`, nested arrays
      recursed, a `NULL` element as `Null`; `record`/ROW() (oid 2249)
      parses to `SqlValue.Row` with fields split and unescaped (a
      record's per-field types are not on the wire without a
      describe, so fields arrive as `Text`; an empty unquoted field
      is `Null`). One text parser reads BOTH pg escaping conventions
      (`""` and `\"`/`\\`), so quoting and embedded commas survive;
      `textOf` re-encodes an `Arr`/`Row` to the pg literal, so a
      decoded value round-trips through copy/bind. Proven live over
      Postgres (TestPgComposite, 8)
- [x] a NAMED composite column's fields are TYPED, not text
      (pg-composite-fields-typed): the driver PRELOADS every named
      composite type's ordered field OIDs at connect — a single simple
      query in the ready state, because a mid-query catalog lookup
      would corrupt the open extended-protocol portal — and `dataRow`
      types a composite column's fields from that cache with no extra
      round trip (`select row('main st', 90210, true)::addr` ->
      `Row(Text, I32, Bool)`; a NULL field is `Null`). Anonymous
      `record`/ROW() stays fields-as-text: its field types are genuinely
      not discoverable (no typrelid). A composite type created AFTER a
      connection is unknown to it until reconnect (stated)
- [x] an ARRAY of a named composite decodes to Arr of typed Row
      (pg-composite-array): the connect preload also maps each
      composite-array type OID to its element composite OID
      (pg_type.typelem where the element is `relkind='c'`), and the
      cell decode became a connection-aware `decodeCell` that uses
      ITSELF as the array element decoder — so `array[row(..)::addr,
      ..]` yields `Arr(Row(...))`, nested and NULL elements included.
      Typing a table's ROW-type selected whole was deferred here on
      the preload cost; the operator named the need and it is the
      next box
- [x] a TABLE'S row type selected whole is a typed Row
      (pg-composite-rowtype, operator: needed): the connect preload
      joins tables, views, materialized views and partitioned tables
      (relkind r/v/m/p) to the named composites (relkind c), in the
      USER namespaces only (pg_catalog, information_schema and toast
      excluded — the catalog's own thousand columns are the cost that
      deferred this), so `select t from t` decodes to `Row` with typed
      fields, `describe` names it, `Typed.rows` reads it into a nested
      case class, and an array of a row type is `Arr(Row(...))`. The
      preload stays ONE simple query at connect; its cost is measured
      on the test database and stated in Results. A table created
      after connect is unknown until reconnect, as for composites.
      A whole-row column has no table column behind it, so `describe`
      reports it nullable (an outer join nulls the whole row) — the
      field is `Option[Row]`, or verify names the drift
- [x] numeric is EXACT and the vendor scalars are NAMED
      (pg-scalar-types). `numeric`/`decimal` decoded to F64 was a
      stated v1 loss; it silently rounded money. Now both drivers
      carry `SqlValue.Num(BigDecimal)` under `SqlType.Num` (pg 1700
      from its text form, NaN/±Infinity falling to F64 since a
      BigDecimal has none; JDBC NUMERIC/DECIMAL via getBigDecimal/
      setBigDecimal), and bind `Num` params. The Schema layer: a
      `Double` field still reads a Num column (lossy BY THE FIELD'S
      CHOICE — the v1 consumers keep working, the loss is now in the
      user's type, not the driver's); a `String` field reads it as its
      exact decimal text; `given Schema[BigDecimal]` (okay.sql, a
      wrap over String so it also travels as text in JSON/CBOR) gives
      the exact typed field, and verify passes it against Num.
      The pg scalars that fell through to `Other("oid:N")` — uuid,
      json, jsonb, timestamp, timestamptz, date, time, timetz,
      interval, inet, cidr, macaddr, xml, money — are now NAMED
      `Other("uuid")` etc. in describe; their VALUES stay pg's
      canonical text (`Text`) — bind-don't-model, and no java.time in
      a JVM/JS/Native module. A `String` field (or any wrapper over
      String: `Schema.wrap(UUID.fromString, _.toString)` on the JVM)
      fits ANY `Other` column with a clean verify: the text IS what
      the wire carries. Proven live on pg and over H2
- [x] the Schema layer binds Arr/Row (sql-schema-composite): a
      case-class field typed `Vector[T]`/`List[T]` decodes from
      `SqlValue.Arr` (elements recursed through the same shape, so
      `Vector[Option[Int]]`, `Vector[Vector[Int]]` and a Vector of
      nested case classes all read) and a NESTED case class decodes
      from `SqlValue.Row` by POSITION (a composite carries no field
      names on the wire; the arity must match exactly, a mismatch is
      `Bad` naming the column). The encode side mirrors it: a Vector
      param binds as `Arr`, a nested product as `Row`, and the driver
      renders them (pg's `textOf` literal; JDBC's `Object[]` for
      arrays). `verify` speaks the same types — `SqlType.Arr(elem)` /
      `SqlType.Row(fields)` — recursively; a driver that cannot name
      an array's element type (JDBC metadata: `Arr(Other(vendor))`)
      passes verify and leaves the element check to decode, which is
      total anyway. Proven live: a `Vector[Int]` field over H2 and
      over pg, and a nested `Addr` case class from a pg named
      composite (`okay_addr`) — through the SAME `Typed.rows` call
- [x] the SAME typed test program runs unmodified over the JDBC
      driver and the pg driver against equivalent schemas (the
      cluster acceptance-test move, applied to SQL) — one function
      of the trait alone, two drivers, ONE equal answer; only the
      SQL strings differ ($n vs ?), which bind-don't-model already
      decided is the dialect's to show
- [x] `Placeholders.numbered` (demo-pg-backend): the ONE mechanical
      dialect difference bind-don't-model itself introduced — `?` on
      JDBC, `$n` on the pg wire — gets a pure renumbering helper in
      the neutral seam (`?` outside quoted literals/identifiers
      becomes `$1..$n`; a program that uses pg's own `?` operators
      does not ask for it). Not a dialect layer: the strings stay
      visible and the DBA's; only the placeholder spelling moves.
      Proven by the first `?`-written program running unchanged over
      the pg driver (SqlMatch, okay-match)
- [x] verify through `describe` catches the same four drifts on
      both drivers, naming the column (pg's RowDescription carries
      no nullability, so describe asks pg_attribute — the catalog
      answers and a clean verify needs no Option-everything
      concession)
- [x] COPY-based bulk load with a load id through the pg driver
      (the specs/data.md warehouse posture, exercised on the free
      engine first) — copyIn speaks the simple-protocol COPY dance
      (CopyInResponse/CopyData/CopyDone) with the text format's
      escapes proven round-trip (tab, newline, backslash, NULL);
      where a warehouse has per-file load history, plain Postgres
      gets a loads REGISTRY whose row commits IN ONE TRANSACTION
      with the data — the retry answers AlreadyLoaded, and a crash
      between COPY and commit rolls back claim AND data together,
      so the retry lands exactly once overall (tested by killing
      the connection mid-load)
- [x] a Native-image (or Node) consumer queries Postgres through
      okay-pg with no JVM/JDBC present (the openness acceptance) —
      okay-pg cross-built JVM+JS; the message pump PULLS bytes
      through the Net seam as a sequential Async program, so the
      SAME driver runs over a blocking socket and over Node's
      buffered net; SCRAM rides a per-platform crypto given
      (JCA / node:crypto); TestPgNode: a Node process speaks SCRAM
      and portals to the dockerized Postgres and gets 42 back, with
      a wrong password refused by SCRAM itself — no JVM, no JDBC in
      the process
- [x] the SCRAM primitives are the SHARED crypto seam, not a private
      copy (security-crypto-split, landed): a crypto-only module
      `okay-crypto` (hmac/sha256/pbkdf2/random, JCA and node:crypto
      givens) resting on NOTHING — so okay-pg depends on it without
      cycling through okay-security's okayHttp (the JWKS road). The
      local PgCrypto retired; Scram/connect take `okay.crypto.Crypto`;
      the four primitives are pinned to published vectors (TestCrypto)
      and the live SCRAM handshake proves the seam end to end. The
      signing surface (RSA/ECDSA, JWT key handles) stays in
      okay-security, which owns those heavier concerns
- [x] `PgTarget.parse` round-trips every field a `postgres://` URL
      names (user/pass/host/port/db), the `sslmode` ladder
      (disable/require/verify-ca/verify-full) maps onto `TlsConfig`,
      `sslrootcert` carries through to `verify-ca`/`verify-full`,
      defaults hold (port 5432, plaintext, db = user) — the tests
      that already proved this in okay-demo, moved

## The brake runs on a failing statement (resource-async-failure)

Found through pgjdbc (jdbc-tails, 2026-09-09): a SQL statement that
THROWS inside a region — an `Async.Run` the outer handler executes —
escaped `Resource.run` with the scope's residual abandoned, so the
region's brake never ran and the connection stayed in the transaction
(the next `begin` refused as "nested"). Every earlier region test threw
from a pure step or aborted through Throws, both of which the scope
catches. The fix is in the core: `Resource.run` forwards an Async
operation GUARDED — a `Run` whose thunk throws and an `Await` whose
callback answers Left release the finalizers first. First cut: one
cast in `Resource.guardAsync`, argued in place. Then (resource-guard,
the operator's call): the hook became a typeclass, `okay.Failing[F]`,
`Resource.run` taking `(using Failing[F])`. Its instances are anchored
on `Async` at either side of `+` and split by Async's own `TypeableK`
through the kernel `<|>` — an UNANCHORED `Failing[F + G]` is selected
by dotty but cannot pin `F` (ambiguous `TypeableK[F]`).

**Corrected the same day (row-typeclass-recipe).** That lane claimed
the anchored set covered `(Async + F) + G` and `(F + G) + Async` too.
It does not: `summon` succeeded there, but what answered was the
low-priority IDENTITY, so a deeper row was silently unguarded — the
defect this hook exists to prevent, reintroduced by a probe that read
"an instance was found" as "the instance guards". The default is now
TOTAL (`Failing.anyRow`: the operation's own class, one cast,
documented in docs/typepedia.md's cast list) and correct for every
nesting; `TestFailing` walks the shapes and asserts each is guarded.
The anchored ROW instances were then deleted as decoration
(failing-simplify) — a total default answers those shapes identically
— leaving two instances: the typed `Failing[Async]` for the
single-effect row most call sites pass, and the total default.

- [x] core: a forwarded `Async.Run` that throws, and an `Await` that
      answers Left, both release (TestResource); on a ROW `Async +
      Throws` the Async half is found through `Failing` and released
      (resource-guard)
- [x] every nesting is guarded, by the typed instance or the total
      fallback: `Async`, `Async + G`, `G + Async`, `(Async + S) + P`,
      `(S + P) + Async`, `(S + Async) + P` and a right-nested row; an
      Async-free row is returned untouched (TestFailing,
      row-typeclass-recipe — the shapes are the test, so a future
      instance that moves the boundary moves it visibly)
- [x] H2: a failing statement inside a region — the brake runs,
      autocommit is restored, the insert before it is gone, the next
      region begins
- [x] pgjdbc: write skew under Serializable, raw — the loser names
      40001 and the connection is usable after; and `transactRetry`
      lands it on run 2 (Live)

## The typed region (sql-typestate)
`transact` refuses a nested begin at RUNTIME (specs/jdbc.md names the
failure mode: the rollback that quietly does not roll back). The
typed region lifts the protocol into the types — PState's typestate
(Atkey's parameterised monad; theory textbook ch. 3) in its two-state
form: `Typed.Db[S]` carries the transaction state as a phantom,
`Typed.region` demands `Db[Tx.No]` and hands the body `Db[Tx.Yes]`,
and there is no begin/commit on the handle at all — the region IS
them. A nested region does not compile; the test proves it with
`compileErrors`. Runtime behavior is exactly `transact`. The full
PState embedding (state type threaded through Cont's answer type) was
declined for v1 — same guarantee, plus a Free<->Cont bridge per step.

- [x] `region` commits on success with the same semantics as
      `transact` (proven on H2 through the JDBC driver)
- [x] a nested `region` is a COMPILE error, and the error names the
      state (`Tx.Yes` where `Tx.No` is demanded)
- [x] a statement fails inside a region, the program HANDLES it, and
      the region reaches COMMIT: Postgres answers the command tag
      `ROLLBACK` with no ErrorResponse, and the region must FAIL, not
      report success (sql-commit-tag, 2026-09-09). Watched failing
      first on the dockerized pg: "expected exception of type PgError
      but body evaluated successfully" while the insert was gone. The
      pg wire driver now reads COMMIT's tag and throws on ROLLBACK
      (`inTx` cleared either way — the server's transaction is over).
      The same probe through r2dbc-postgresql passes because that
      driver refuses such a COMMIT itself. The JDBC road has no pg
      case in the tree (pgjdbc is not a dependency; H2 and SQLite keep
      a transaction usable after a failed statement, so the aborted
      state does not exist there) — when a pgjdbc consumer appears,
      run this probe through it before trusting `commit()`.
- [x] (jdbc-tails, 2026-09-09) the probe ran through pgjdbc 42.7.3 and
      FAILED: pgjdbc's `commit()` on the aborted transaction reports
      success. `JdbcSql` now remembers a statement that failed inside
      the open transaction (`guarded`) and, at commit, PROBES with
      `select 1` first: on pg the probe answers 25P02, the region rolls
      back and fails with SQLSTATE 40000; H2/SQLite answer the probe and
      commit as before. Measured green through pgjdbc (Live).

## Serialization failures are retried (sql-serialization-retry)

Under RepeatableRead and Serializable the engine may choose a
transaction to LOSE — Postgres's SSI answers `40001
serialization_failure`, a deadlock `40P01` — and the only correct
response is to run the transaction again from `begin`. Before this
lane no code in the tree looked at a SQLSTATE (0 hits), so a program
that asked for Serializable saw an exception where the engine meant
"try again". A transact body is a program VALUE, re-runnable by
construction; the region is where the retry belongs.

- `Sql.sqlState(t)`: the driver names the engine's SQLSTATE behind a
  failure (pg: the ErrorResponse's `C` field, now carried on
  `PgError.code`; JDBC: `SQLException.getSQLState`; R2DBC:
  `R2dbcException.getSqlState`). `Sql.retryable(state)` is the class:
  40001 and 40P01, nothing else.
- `Async.attempt(prog)`: the program's failure as data, one fiber,
  every platform — the effect-world try/catch the retry needs and the
  core did not have.
- `Typed.transactRetry(db, isolation, Retry(attempts, backoff))(body)`:
  each run is a full `transact` (begin, body, commit; the cancel brake
  rolls back a failed run), a retryable failure pauses and runs again,
  anything else propagates at once, and the answer `Retried(value,
  attempts)` carries how many runs it took — the number to watch. The
  body is `Resource + Async`: the attempt must run the body's effects,
  so a Throws-aborting body runs its Throws inside and answers the
  Either (an abort is a decision, not a conflict).

- [x] a driver that fails a run with 40001 N times and then succeeds:
      `Retry(N+1)` answers the value with `attempts = N+1`, every
      failed run was rolled back (the brake), and `Retry(N)` propagates
      the N-th failure; a non-retryable failure propagates on the
      first run (H2 through JDBC with a failing decorator)
- [x] write skew on the dockerized pg under Serializable: two
      connections read, both write, the second COMMIT fails with 40001
      through the wire driver — and `transactRetry` on the loser
      re-runs it and lands the write (Live)

## Temporal, uuid and json values (sql-temporal-types)

Before this lane a `timestamptz`, `date`, `time`, `uuid`, `json`/
`jsonb` column travelled as `Text` under `SqlType.Other(name)`: a
String field fit it and every `created_at` was hand-parsed per field,
in the engine's own print form (pg in the SESSION's zone with an
offset, H2 `+00`). The seam now has the values, platform-neutral on
purpose — this module runs on JVM, JS and Native and java.time is
JVM-only (okay-intent's `Temporal` made the same choice):

- `SqlValue.Timestamp(micros)` — microseconds since the epoch, UTC. A
  `timestamptz` exactly; a `timestamp` WITHOUT zone is read as UTC,
  stated, on every driver (the JDBC reads go through a UTC calendar so
  the JVM's default zone never enters). `Date(days)` since the epoch,
  `Time(micros)` into the day, `Uuid(java.util.UUID)` (the JDK class
  every platform has), `Json(text)` untouched. `SqlType` mirrors them.
- `okay.sql.Temporal` renders ISO 8601 UTC and parses what engines
  print (`2026-09-02 06:00:00.123456+02`, `+02:30`, `T…Z`, no zone);
  Hinnant's civil-date arithmetic, exact over the Int range of days.
  A form the parser does not know stays `Text` — loud in the type,
  never a wrong number.
- Fields: `given Schema[UUID]` everywhere; `Schema[Instant]`,
  `Schema[LocalDate]`, `Schema[LocalTime]` on the JVM (ISO text in
  JSON/CBOR). The typed layer knows them by the IDENTITY of the given
  (`Typed.Known`: the given is a stable val, so `eq` is the proof of
  the type — the one cast in the module, isolated in `Known.find`),
  and the platform hands over its table (`JavaTime.known`, empty off
  the JVM). A String field still fits every one of these columns and
  reads the ISO text — the lossless fallback, its print form now
  canonical rather than the engine's.
- Params bind natively, and on JDBC by the DECLARED type: a Timestamp
  goes in as an `OffsetDateTime` at UTC where the parameter is a
  `timestamp with time zone`, as the UTC wall-clock `LocalDateTime`
  where it is a `timestamp` (`ParameterMetaData`, asked once per
  statement and only when a temporal param is bound); reads mirror it
  by the column's JDBC code. The Calendar road (`setTimestamp(ts,
  utcCalendar)`) is deliberately NOT used: MEASURED on H2 2.3, a UTC
  calendar's 06:00 into a `timestamp with time zone` is stored as
  `06:00:00+02` — the session's offset stamped onto the calendar's
  wall clock, a two-hour error that the isolated suite hid because
  the Calendar read made the same mistake in reverse; the full
  matrix (DuckDB's suite first in the same JVM) exposed it. A driver
  without JDBC 4.2 `getObject(Class)` or parameter metadata (SQLite)
  falls back to ISO text through `Temporal`. R2DBC: `OffsetDateTime`
  at UTC, `LocalDate`, `LocalTime`, `UUID`; the pg wire renders ISO
  text and the server types it from the column. Json binds as its
  text — a `jsonb` column through JDBC/R2DBC wants the DBA's
  `?::jsonb` in the statement (bind-don't-model); the wire driver
  needs nothing. okay-delta maps Timestamp/Date to Delta's own
  TimestampType (micros) and DateType (days) — the same units.

- [x] `Temporal` round-trips civil dates over centuries, parses pg's,
      H2's and ISO's forms with offsets applied, truncates nanos to
      micros, floors before the epoch, and renders ISO UTC (TestSqlPure,
      JVM + JS + Native)
- [x] H2 through JDBC: timestamptz/timestamp/date/time(6)/uuid read into
      Instant/LocalDate/LocalTime/UUID fields, `describe` names the
      kinds, verify is clean, a microsecond and an 1899 date survive the
      bind-and-read round trip; the String field reads the ISO text
- [x] pg over the wire, session zone Europe/Kyiv: the same six plus
      jsonb and a `timestamptz[]` into `Vector[Instant]`, typed reads,
      verify clean, exact bind back (Live)
- [x] R2DBC on H2 and pg: timestamptz/date/time/uuid typed both ways
      (Live for pg)
- [x] (jdbc-tails) pgjdbc reports a `timestamptz` parameter and column
      as plain `Types.TIMESTAMP`; binding the UTC wall clock into it
      shifted by the session zone (three hours under Europe/Kyiv,
      measured). `zonedCode` reads the vendor TYPE NAME too (`…tz`,
      `with time zone`) for both reads and binds; the round trip is
      exact through pgjdbc, jsonb through `?::jsonb` (Live)
- [x] (jdbc-tails) SQLite: its driver TAKES `getObject(LocalDateTime)`
      and then fails to parse ISO text with its own format, a
      DateTimeParseException, not a SQLException — the fallback now
      catches any non-fatal failure; declared timestamp/date columns
      read and bind through the ISO text road

## Read-only regions, and the isolation restored (sql-readonly-region)

The FOREIGN posture's honest declaration: where the DBA gave us reads,
the region says so and the engine enforces it. `Sql.begin(isolation,
readOnly)` and `Typed.transact/region/transactRetry(..., readOnly =
true)`; `Granted.readOnly` is what the engine GRANTED, read back rather
than assumed — pg's `SET TRANSACTION ... READ ONLY` then `SHOW
transaction_read_only` (a write inside answers `25006`); JDBC's
`setReadOnly` is a hint and `isReadOnly` reports it (H2 ignores the
hint and reports false, which the grant then says); R2DBC's
`TransactionDefinition` with `READ_ONLY` (r2dbc-postgresql takes it,
a driver that refuses definitions gets the plain begin and grants no
read-only). And the leak the audit found: `JdbcSql` restored autocommit
after a region but not the isolation level, so every autocommit
statement after a `transact(Serializable)` ran Serializable; the level
and the read-only flag are saved at `begin` and restored with
autocommit on commit, rollback and the brake.

- [x] pg over the wire: a READ ONLY region is granted and read back, a
      write inside fails with 25006, a plain region grants none, and
      writes work again after (Live)
- [x] H2 through JDBC: after `transact(Serializable, readOnly = true)`
      the connection's isolation, autocommit and read-only flag are
      what they were before; the grant reports H2's refusal of the
      hint honestly
- [x] r2dbc-postgresql: the SPI definition is taken, the write inside
      refuses with 25006, the plain begin grants none (Live)

## The pool (sql-pool)

One `Sql` is one connection and one thread of control (the driver
contract), and the audit found no pool anywhere. `okay.sql.Pool[C <:
Sql](size, acquireTimeoutMillis)(open)(close)` is the one that wraps
any driver as a Resource-shaped value, every platform: `borrow(use)`
runs a program on one connection and returns it after, value or
failure; `pinned` hands a connection to the enclosing Resource scope,
whose end returns it; `stats` (size, idle, busy, waiting, created,
closed) is a plain value for /metrics; `close()` disposes idle
connections now and busy ones as they return, and fails the waiters.
State is one `TRef` cell modified atomically; waiters are callbacks;
the hand-off is cancel-safe — a waiter whose timeout fired and a grant
that races it settle on one CAS, and the loser passes the connection
to the next waiter, so a timeout never leaks a connection. A returned
connection goes through the BRAKE first: an open transaction (a raw
`begin` the borrower never closed) rolls back, a no-op costs nothing —
the health probe this stack already had. Hikari behind JDBC is `open =
() => JdbcSql(dataSource.getConnection)` and a close that hands back;
`JdbcSql.close` exists for the plain case.

- [x] eight borrowers on a pool of two: never more than two connections
      open, none reopened, every insert lands, idle equals opened after,
      close disposes them all (H2)
- [x] a borrower leaves a raw `begin` open: the brake rolls it back on
      return, and the next borrower sees no leftover on the same
      connection
- [x] exhausted: the second borrow on a held pool of one fails with
      `Exhausted` within its timeout, leaves the queue, and succeeds once
      the first returns — on the one connection
- [x] `pinned` holds one connection across statements for the scope and
      returns it at the end; a borrow after `close` refuses with `Closed`
- [x] (persistence-e2e) a Pool over the pg wire lends two connections
      to a real write skew; `transactRetry` through `borrow` lands on
      run 2; the pool is whole after; `Pool.Stats` is a Schema value and
      okay-ops renders it as `okay_pool_*` rows (Live)

## Out of scope

- writing MySQL/MSSQL/Oracle wire protocols — R2DBC or JDBC are
  those roads until a need proves otherwise; pg-wire's leverage
  justified itself by the family it unlocks, theirs does not yet
- connection pooling in the seam — a pool wraps `Sql` like any
  Resource; an interop (Hikari) stays possible behind the JDBC
  driver
- SQL dialect abstraction — the strings stay visible and the
  DBA's; bind-don't-model already decided this and the seam does
  not reopen it

## Decisions

- **Cut at the driver, not at the dialect** — the contract
  (typing, totality, verify, region) is what okay owns; moving
  statements and frames is what drivers own; dialects stay in the
  SQL strings. Rejected: a dialect-abstracting layer (an ORM
  through the back door).
- **Placeholder renumbering is not dialect abstraction** — `?` vs
  `$n` is the one difference the seam's own bind-don't-model rule
  created between its two drivers, so a pure `?`→`$n` rewrite lives
  in the seam; everything else in a statement stays the string the
  DBA reads. Rejected: a per-driver `placeholder` field on `Sql`
  (a trait change for a string function) and a SqlMatch-private
  copy (the next `?`-program would write it again).
- **Neutral SqlValue/Col instead of java.sql types in the
  contract** — the whole point; also what makes the typed layer
  cross-platform for free. Rejected: typed layer parameterized
  over driver-native types (N type mappings instead of one).
- **Async in the driver trait** — a wire driver cannot be sync,
  and JS cannot park; the JDBC driver wraps blocking into
  Async.Run exactly as JdbcInterop does today, so the JVM loses
  nothing. Rejected: a sync seam with async bolted on per driver.
- **Postgres wire first, R2DBC as a hatch, both behind one trait**
  — pg-wire buys platform freedom and a whole engine family;
  R2DBC buys driver availability on the JVM only; virtual threads
  already neutralized its performance argument. Rejected:
  R2DBC-first (adds a dependency ecosystem without adding a
  platform); rejected: everything-native-first (three wire
  protocols before one consumer).
- **okay-jdbc is a driver, not a legacy layer** — stated so nobody
  "migrates off" it needlessly: for warehouses, DuckDB and
  anything JVM-side with a good driver, it remains the honest
  default. Rejected: deprecating JDBC (fashion, not engineering).
- **`cancel()` is sync, and it is the only sync method** — rollback
  on an unwinding scope must run inside a Resource finalizer, and
  finalizers are sync by design (the same reasoning that made
  `Topic` a sync engine SPI: an async signature there would be
  theater). The JDBC driver's rollback is genuinely synchronous
  under the Async wrapper; a wire driver closes the connection and
  lets the server roll back the abandoned transaction — the
  behavior every deployment already relies on when a client dies.
  Rejected: an async rollback in the finalizer path (needs a
  blocking handler exactly where JS cannot have one); rejected:
  no brake at all (a handled abort crossing the scope would leave
  the transaction open — the rollback that quietly does not roll
  back, the exact ORM bug jdbc.md refuses).
- **A transact program is one-shot** — the region ties driver
  state (begin..commit/cancel) to one run; replaying it through a
  multi-shot handler is not meaningful and is stated rather than
  defended against.

## Results

The seam and its first driver landed (sql-seam, 2026-09-01).

- **okay-sql** (cross-built JVM/JS/Native — the no-java.sql
  assertion IS the JS and Native compile): `SqlValue`, `SqlType`
  (+`Other(name)` so verify can name a vendor type), `Col`,
  `Isolation`, `Granted(requested, granted)` with `downgraded`,
  `trait Sql` (describe/query/update/batch/begin/commit/rollback +
  the sync `cancel()` brake), `Drift`, `Bad(column, error, row)`.
- **The typed layer** (`Typed`, `Params`): field↔column by name
  (camelCase→snake_case, case-insensitive, exact-name fallback);
  the frame decoder resolves ONCE against `describe` and then
  every row decodes totally — `Bad` carries column and row
  position; `verify` reports dropped/renamed (absent), retyped and
  nullability drifts naming the column — a cast expression drifts
  twice, honestly (type + lost NOT NULL, H2 metadata agrees);
  params bind positionally via the driver's prepared path only, so
  injection stays unrepresentable; `transact[A, G]` is generic in
  the rest of the row, so a Throws abort can cross the scope and
  the region still rolls back via `cancel()` in the Resource
  finalizer.
- **JdbcSql** (okay-jdbc): the JdbcInterop chunk shape behind the
  seam; java.sql.Types→SqlType (NUMERIC/DECIMAL→F64 v1, stated — retired by pg-scalar-types: Num, exact);
  begin refuses nested loudly, reads back the granted level;
  commit/rollback/cancel restore autocommit.
- **A core find, fixed where it lives**: `Resource.run`'s residual
  applies the continuation `k(y)` at the OUTER handler's call
  site, outside the region's try — a `.map` that throws after a
  forwarded effect skipped the finalizers (the transact
  rollback-on-exception test caught it). Fixed in Resource.scala,
  pinned by a core test ("a throw in the continuation AFTER a
  forwarded effect still releases").
- **Tests**: 4 in okay-sql per platform (name mapping, positional
  bind incl. Option→Null, non-row-shaped refusal, downgrade flag);
  13 in okay-jdbc/TestTyped over H2 as the no-DDL `app` user —
  rows by label, NULL-in-non-Option as data with row position,
  reordered SELECT, the four verify drifts, positional params,
  transact commit/exception/abort with autocommit restored, nested
  refusal, granted isolation, chunked streaming inside a
  transaction (64×7+52 over 500 rows), DDL refusal.
- **SQLite proves the embedded corner** (sql-sqlite, user ask,
  same day): the whole typed battery over xerial sqlite-jdbc
  (test-scope) against a FILE database — metadata honest enough
  for a clean verify, both isolation levels granted, the Writes
  bridge in its spec-preferred `ON CONFLICT DO NOTHING` spelling,
  and the READ-ONLY open mode standing in for the no-DDL posture
  (an embedded db has no users; "their database" is a file you
  were handed, possibly read-only — reads full, every write
  refuses).
- **The pg wire driver landed** (sql-pg-wire, same day): okay-pg,
  ~400 lines for the whole road — startup, SCRAM-SHA-256 (client
  nonce extension checked, server signature VERIFIED; md5 and
  cleartext deliberately not spoken), the extended protocol with
  portals as the chunk mechanism (Execute maxRows + Flush;
  PortalSuspended = next chunk), text format both directions v1,
  errors drained to quiet before the throw so the session
  survives, describe consulting pg_attribute for nullability.
  Live suite (8 tests incl. the two-driver acceptance) against
  the dockerized Postgres 17.11; skips where absent.
- **COPY landed** (sql-pg-copy, same day): see the checked box —
  copyIn + Load with the one-transaction registry claim; the
  crash-retry battery runs live against the dockerized Postgres.
- **sql-pg-node landed** (same day): the last box checked. The
  pump restructure onto Net (the JVM live battery green THROUGH the
  new async-pull pump is itself the acceptance that nothing
  regressed), okay-pg cross-built, SCRAM as phase objects over the
  per-platform crypto given. Every behavior box of this spec is now
  checked; the pg family is a connect call away from a Native
  binary or a Node process too.
- **pg-scalar-types landed** (2026-09-02): `SqlValue.Num(BigDecimal)`
  / `SqlType.Num` on both drivers; pg names uuid/json/jsonb/xml/
  timestamp/timestamptz/date/time/timetz/interval/inet/cidr/macaddr/
  money in describe (values stay text). Typed: `fits` gained
  `F64←Num` (lossy by the field), `Text←Num` (exact text) and
  `Text←Other(_)` (a String reads any vendor type); decode renders
  `Num` into Double or String accordingly; `given decimalSchema:
  Schema[BigDecimal]` (a refine over String — `import okay.sql.given`)
  is the exact typed field and travels as a string in JSON/CBOR (the
  Writes journal derives Schema[SqlValue] through it). Finds:
  sqlite-jdbc's `getBigDecimal` does not mark the column, so a
  following `wasNull` throws "column -1 out of bounds" — the Num read
  decides nullness from the value (SqlStore's `SUM(expr)` over H2 is
  the consumer that arrives as Num now). pg `'NaN'::numeric` has no
  BigDecimal — falls to `F64(NaN)`, stated. Tests: okay-sql 10/10 on
  three platforms; TestTyped 16/16 (decimal(30,9) exact into
  BigDecimal, rounded into Double, param round-trip, timestamp as
  text with a clean verify); TestPgComposite 15/15 live (numeric(30,9)
  exact via Typed.rows, describe names uuid/jsonb/timestamptz, String
  fields verify clean, Num param lands exact, numeric[] → Arr(Num)).
- **sql-schema-composite landed** (2026-09-02): `Typed`'s field
  shape became a recursive `Shape` (Prim/Opt/Iso/Arr/Row) read off
  the Schema once — decode and encode are two folds over it, so the
  old per-field `into`/`outof` closures went away rather than growing
  two more cases. `SqlType.Arr(elem)`/`Row(fields)` join the verify
  vocabulary; `fits` recurses and accepts `Arr(Other)` (JDBC names
  ARRAY, not the element). Drivers: JdbcSql maps `Types.ARRAY`,
  reads `getArray` (java boxes → SqlValue, nested arrays included)
  and binds an `Arr` as `Object[]` (H2 and pgjdbc take it; a `Row`
  param is refused loudly — JDBC has no neutral composite); PgSql's
  `describe` types columns through the same composite/array caches
  `decodeCell` reads, so `Col.tpe` says `Row(Text, I32, Bool)` for
  `okay_addr`. Tests: 4 new in okay-sql on all three platforms (a
  one-frame fake driver — bind, decode, verify, damage naming column
  and row); H2 array column read/verify/bind round-trip; live pg:
  `Person(id, Vector[Int], Addr, Vector[Addr], Option[Addr])` from a
  table through `Typed.rows` with a clean verify and a `Wrong` shape
  drifting on `home`, and `rowsOf` binding `Vector`+`Addr` params
  read back typed. Refuted: keeping the flat `Field(into, outof)` and
  special-casing arrays — the composite-of-arrays-of-composites case
  needs the recursion anyway. Composite fields bind by POSITION: pg
  puts no field names on the wire; by-name would need the preload to
  carry attname too — deferred until a consumer reorders a type.
- **security-crypto-split landed** (same day): PgCrypto retired into
  `okay-crypto`, a crypto-only module (hmac/sha256/pbkdf2/random,
  JCA + node:crypto) that rests on nothing, so okay-pg stands on the
  SHARED seam without cycling through okay-security's okayHttp. The
  four primitives are pinned to published vectors (TestCrypto); the
  live SCRAM battery proves the seam end to end. The signing surface
  stays in okay-security. The crypto seam is now available to any
  future consumer that needs primitives without the http drag.
demo-pg-backend (2026-09-02): the first `?`-written program crossed
to the pg driver — okay-match's SqlMatch, 60-odd statements, changed
in exactly two places: `DOUBLE` became the portable `DOUBLE
PRECISION`, and the statements pass through `Placeholders.numbered`
(a constructor seam, identity by default). The engine suite that
proved sqlite runs verbatim against live Postgres. So the acceptance
line "only the SQL strings differ ($n vs ?)" now has its mechanical
half in the seam and the seam's Out-of-scope line (no dialect
layer) still holds — nothing but the placeholder spelling moved.
pg-composite-rowtype (2026-09-02, operator: needed): the connect
preload now joins tables, views, matviews and partitioned tables
(relkind r/v/m/p) to the named composites, in user schemas only —
one simple query, measured at 6.5 ms for the whole connect on the
test database (a handful of user tables; the catalog's own columns,
the cost that deferred this, are excluded by the namespace filter).
`select p from okay_people p` decodes to a Row whose fields are
themselves typed — an int[], a named composite, an array of
composites, a NULL — because `parseCompositeTyped` now decodes
fields with the connection-aware `decodeCell` (it used the static
scalar map before, so a composite inside a composite stayed text;
found by this test, fixed). `describe` names the nested type,
`Typed.rows[Wrap(p: Option[Person])]` reads it, verify is clean; the
strict `p: Person` drifts with found "nullable" because a whole-row
column has no table column behind it. A table created after connect
is unknown to that connection: its cell is the raw text until
reconnect, as for composites.
sql-r2dbc (2026-09-02, operator: needed): okay-r2dbc, the hatch
landed as framed — `R2dbcSql(conn)` over `io.r2dbc.spi.Connection`,
query as PULLED chunks through a forty-line demand-driven Subscriber
(request(fetchSize), park behind Async.Run), update/batch/transact,
the same SqlValue/Col vocabulary; the typed layer runs unchanged on
H2 (r2dbc-h2) and on the dockerized Postgres (r2dbc-postgresql), the
same suite on both. Two things the SPI taught: a Result must be
consumed before the next is asked for — collecting Results first
hangs against the Postgres driver (H2 is eager and hid it), so the
driver walks them one at a time and keeps the results publisher open
under a streaming Result until its rows end; and metadata exists only
with a row, so `describe` reads the first row's, an empty result
describes as EMPTY, and nullability is the driver's word — H2 states
it, r2dbc-postgresql answers UNKNOWN and verify names every non-Option
column there. A NULL parameter has no type in the seam: the untyped
bind is tried, the String one is the fallback H2's driver wants. The
verdict of the framing held: nothing here is faster than JDBC behind
Async.Run; what arrived is the DRIVER seat for engines okay will not
speak natively.

## Cast-free (2026-09-02, cast-free-typed)
Typed's `Shape` mirrored the Schema untyped — `Iso(Any => Either
[String, Any])`, `Arr(Vector[Any] => Any)`, value casts by SqlType on
the encode side — eleven casts. Now `Shape[A]` is a GADT: `Prim[A]`
carries its own typed decode (with the column widenings I32 → I64,
Num → F64/Text) and encode, `Opt`/`Iso`/`Arr` carry their element
types, `Row[A]` carries the field shapes by position for decoding
(the Mirror's `make` takes them erased, so no kernel is needed) and
the `Schema.SProduct` itself for encoding, whose `eachField` hands
every value over at its own type; a nested product's field shape is
recomputed from its Schema at encode time (a small tree walk per
parameter binding, not per row). `decode[A](sh: Shape[A], v)` and
`encode[A](sh: Shape[A], v: A)` are written by matching, binding the
element types (`case o: Shape.Opt[a]`). `encodeParams[P](s: Schema[P],
p: P)` is typed too. Zero casts left in Typed.scala; the sql suites on
JVM/JS/Native, jdbc, r2dbc and match unchanged and green.

