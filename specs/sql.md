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
```

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

- [ ] the typed layer (rows/params/verify/transact) compiles and
      tests against `Sql` with ZERO references to java.sql above
      the driver line (asserted structurally: okay-sql has no
      java.sql import)
- [ ] the JDBC driver passes the full specs/jdbc.md behavior list
      through the seam (same tests, re-targeted — H2, no-DDL user)
- [ ] the pg driver: startup + SCRAM auth, extended-protocol query
      with portal streaming at constant memory, params and rows
      through SqlValue, transact begin/commit/rollback with granted
      isolation — against a real Postgres (live-suite pattern:
      skips where the endpoint is absent)
- [ ] the SAME typed test program runs unmodified over the JDBC
      driver and the pg driver against equivalent schemas (the
      cluster acceptance-test move, applied to SQL)
- [ ] verify through `describe` catches the same four drifts on
      both drivers, naming the column
- [ ] COPY-based bulk load with a load id through the pg driver
      (the specs/data.md warehouse posture, exercised on the free
      engine first)
- [ ] a Native-image (or Node) consumer queries Postgres through
      okay-pg with no JVM/JDBC present (the openness acceptance)

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

## Results

(after implementation — the two-driver acceptance run, pg-wire
against live Postgres, the no-java.sql structural check)
