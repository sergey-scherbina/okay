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
- **okay-r2dbc — the hatch, honestly framed.** R2DBC (Reactive
  Relational Database Connectivity — likely what "rjdbc" refers
  to; RJDBC proper is an R-language package and not relevant here)
  is a JVM API where drivers are non-blocking by construction.
  What it buys us: maintained async drivers for engines whose wire
  protocol we will not write (MSSQL, Oracle, MySQL before okay-my
  exists). What it does not buy: on virtual threads, JDBC-behind-
  Async.Run already fails to block the carrier — so R2DBC is not a
  performance unlock, it is a driver-availability unlock. Filed
  low-priority; adopted when a deployment names one of its
  engines.

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
- [x] the SAME typed test program runs unmodified over the JDBC
      driver and the pg driver against equivalent schemas (the
      cluster acceptance-test move, applied to SQL) — one function
      of the trait alone, two drivers, ONE equal answer; only the
      SQL strings differ ($n vs ?), which bind-don't-model already
      decided is the dialect's to show
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
- [ ] a Native-image (or Node) consumer queries Postgres through
      okay-pg with no JVM/JDBC present (the openness acceptance)

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
  seam; java.sql.Types→SqlType (NUMERIC/DECIMAL→F64 v1, stated);
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
- **Still open**: the non-JVM consumer (sql-pg-node, with the
  cross-platform transport leg).
