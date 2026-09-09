package okay.sql

import okay.{!, +, Async, Chunk, Produce}

/**
 * The relational driver seam (specs/sql.md): a driver is a way to
 * move statements, values and row frames — nothing more. Everything
 * smart (typing, totality, verify, the transaction region) lives
 * above it in `Typed`, written once; drivers plug in underneath:
 * okay-jdbc (JVM, blocking-behind-Async on virtual threads), okay-pg
 * (the Postgres wire, cross-platform), okay-r2dbc (the hatch).
 *
 * No java.sql anywhere in this module — asserted structurally: it
 * cross-builds for JS and Native, platforms where java.sql does not
 * exist.
 */

/** neutral values — what a row cell or a parameter can be. The
 * driver maps these to its wire/API; Schema binds case classes to
 * them above. */
enum SqlValue:
  case Null
  case Bool(v: Boolean)
  case I32(v: Int)
  case I64(v: Long)
  case F64(v: Double)
  case Text(v: String)
  case Bytes(v: Array[Byte])
  /** numeric/decimal, EXACT (pg-scalar-types): money does not round
   * in the driver; a Double field rounds it by the field's choice */
  case Num(v: BigDecimal)
  /** a SQL array: the elements typed by the driver (pg-composite-decode).
   * Nested arrays are Arr-of-Arr; a SQL NULL element is `Null`. */
  case Arr(elems: Vector[SqlValue])
  /** a composite / ROW() value: the fields, in order. Anonymous
   * `record` fields arrive as `Text` (their types are not on the wire
   * without a describe); a named composite's fields are typed when the
   * driver resolves them. A NULL field is `Null`. */
  case Row(fields: Vector[SqlValue])
  /** microseconds since the epoch, UTC (sql-temporal-types): a
   * `timestamptz` exactly; a `timestamp` without zone read as UTC,
   * stated. Renders as ISO 8601 (`Temporal.renderTimestamp`) */
  case Timestamp(micros: Long)
  /** days since 1970-01-01 */
  case Date(days: Int)
  /** microseconds into the day (a `time` without zone) */
  case Time(micros: Long)
  case Uuid(v: java.util.UUID)
  /** json/jsonb: the document's text, untouched */
  case Json(text: String)

/** the column types verify speaks; `Other` carries a vendor type by
 * name so a drift report can say what it found rather than shrug */
enum SqlType:
  case Bool, I32, I64, F64, Text, Bytes
  /** numeric/decimal: exact, arbitrary precision */
  case Num
  case Other(name: String)
  // the temporal, uuid and json columns (sql-temporal-types); a
  // String field fits every one of them — the ISO text is what it reads
  case Timestamp, Date, Time, Uuid, Json
  /** an array column; `Other` as the element when the driver's
   * metadata cannot name it (JDBC) — decode checks the elements */
  case Arr(elem: SqlType)
  /** a composite column, fields in order (sql-schema-composite) */
  case Row(fields: Vector[SqlType])

/** column description, the verify input — driver-neutral */
final case class Col(label: String, tpe: SqlType, nullable: Boolean)

/** the per-transaction decision (the Ack pattern once more);
 * engines interpret levels differently, so `begin` answers with
 * what was actually granted */
enum Isolation:
  case ReadCommitted, RepeatableRead, Serializable

final case class Granted(requested: Isolation, granted: Isolation):
  def downgraded: Boolean = granted != requested

/**
 * The driver. Everything speaks Async — a wire driver is async by
 * nature; the JDBC driver wraps blocking calls in `Async.Run`,
 * which virtual threads make honest on the JVM.
 *
 * `cancel` is the ONE deliberately synchronous method: the
 * emergency brake a Resource finalizer pulls when a transaction
 * scope unwinds without a commit — finalizers are sync by design.
 * Outside a transaction (or after a commit) it is a no-op. A wire
 * driver may implement it by closing the connection: every serious
 * server rolls back an abandoned transaction.
 */
trait Sql:
  /** the shape of this statement's result, for verify */
  def describe(sql: String): Vector[Col] ! Async

  /** row frames, chunked — one Async operation per chunk, constant
   * memory for any result size */
  def query(sql: String, params: Vector[SqlValue] = Vector.empty)
  : Chunk[Vector[SqlValue]] ! (Produce + Async)

  /** affected-row count */
  def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async

  /** one chunk, one batch */
  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async

  /** opens a transaction; a second begin before commit/rollback
   * REFUSES loudly (nested transact is the rollback that quietly
   * does not roll back — specs/jdbc.md) */
  def begin(isolation: Isolation): Granted ! Async
  def commit(): Unit ! Async
  def rollback(): Unit ! Async

  /** the sync emergency brake (see the trait comment) */
  def cancel(): Unit

  /** the SQLSTATE behind a failure this driver raised, when it has
   * one — the engine's own classification, so the region can tell a
   * serialization failure (a normal outcome under Serializable, to
   * be retried) from a defect. `None` for anything else. */
  def sqlState(t: Throwable): Option[String] = None

object Sql:
  /** the states a region RETRIES: serialization_failure and
   * deadlock_detected (the SQL standard's class 40, transaction
   * rollback — the engine chose this transaction to lose) */
  def retryable(state: String): Boolean =
    state == "40001" || state == "40P01"

/** startup drift between our Schema and their schema: data naming
 * the column, never a throw (the Durable fingerprint lesson at the
 * database seam) */
final case class Drift(column: String, expected: String, found: String)

/** per-row decode damage: data naming the column and the row
 * position (filled in by the streaming decode), never a throw */
final case class Bad(column: String, error: String, row: Long = -1L)
