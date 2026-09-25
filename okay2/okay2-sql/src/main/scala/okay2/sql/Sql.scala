package okay2.sql

import okay2._
import okay2.async.Async
import okay2.stream.{Chunk, Source}

/**
 * The relational driver seam (okay-sql's Sql.scala, specs/sql.md): a
 * driver moves statements, values and row frames, nothing more.
 * Everything smart — typing, totality, verify, the transaction region —
 * lives above it in `Typed`, written once; drivers plug in underneath
 * (okay2-jdbc on the JVM). No java.sql in this module: it cross-builds
 * for Scala.js and Scala Native, where java.sql does not exist.
 */
sealed trait SqlValue

/** neutral values: what a row cell or a parameter can be */
object SqlValue {
  case object Null extends SqlValue
  final case class Bool(v: Boolean) extends SqlValue
  final case class I32(v: Int) extends SqlValue
  final case class I64(v: Long) extends SqlValue
  final case class F64(v: Double) extends SqlValue
  final case class Text(v: String) extends SqlValue
  final case class Bytes(v: Array[Byte]) extends SqlValue
  /** numeric/decimal, EXACT: money does not round in the driver */
  final case class Num(v: BigDecimal) extends SqlValue
  /** a SQL array; nested arrays are Arr of Arr, a NULL element `Null` */
  final case class Arr(elems: Vector[SqlValue]) extends SqlValue
  /** a composite / ROW() value: the fields, in order */
  final case class Row(fields: Vector[SqlValue]) extends SqlValue
  /** microseconds since the epoch, UTC */
  final case class Timestamp(micros: Long) extends SqlValue
  /** days since 1970-01-01 */
  final case class Date(days: Int) extends SqlValue
  /** microseconds into the day */
  final case class Time(micros: Long) extends SqlValue
  final case class Uuid(v: java.util.UUID) extends SqlValue
  /** json/jsonb: the document's text, untouched */
  final case class Json(text: String) extends SqlValue
}

/** the column types verify speaks; `Other` carries a vendor type by name
 * so a drift report says what it found */
sealed trait SqlType

object SqlType {
  case object Bool extends SqlType
  case object I32 extends SqlType
  case object I64 extends SqlType
  case object F64 extends SqlType
  case object Text extends SqlType
  case object Bytes extends SqlType
  /** numeric/decimal: exact, arbitrary precision */
  case object Num extends SqlType
  final case class Other(name: String) extends SqlType
  case object Timestamp extends SqlType
  case object Date extends SqlType
  case object Time extends SqlType
  case object Uuid extends SqlType
  case object Json extends SqlType
  final case class Arr(elem: SqlType) extends SqlType
  final case class Row(fields: Vector[SqlType]) extends SqlType
}

/** a result column: its label, its type, whether it may be NULL */
final case class Col(label: String, tpe: SqlType, nullable: Boolean)

sealed trait Isolation

object Isolation {
  case object ReadCommitted extends Isolation
  case object RepeatableRead extends Isolation
  case object Serializable extends Isolation
}

/** what `begin` got: the engine may grant a different level (SQLite
 * has two), and a caller that needs the one it asked for can refuse */
final case class Granted(requested: Isolation, granted: Isolation, readOnly: Boolean = false) {
  def downgraded: Boolean = granted != requested
}

/** one driver connection */
trait Sql {
  /** the result columns a statement would produce, without running it */
  def describe(sql: String): Vector[Col] ! Async

  /** the rows, as a chunked stream */
  def query(sql: String, params: Vector[SqlValue] = Vector.empty): Source[Chunk[Vector[SqlValue]]]

  /** a statement's affected-row count */
  def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async

  /** one statement, many parameter rows */
  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async

  def begin(isolation: Isolation, readOnly: Boolean = false): Granted ! Async
  def commit(): Unit ! Async
  def rollback(): Unit ! Async

  /** the brake: abandon whatever is open (a region's finalizer, a pool's
   * return), synchronously */
  def cancel(): Unit

  /** the SQLSTATE a failure carries, where the driver knows it */
  def sqlState(t: Throwable): Option[String] = None
}

object Sql {
  /** serialization failure and deadlock: the two a region may re-run */
  def retryable(state: String): Boolean = state == "40001" || state == "40P01"
}

/** what `verify` found: a column and what was expected of it */
final case class Drift(column: String, expected: String, found: String)

/** a cell that did not decode: the column, why, and the row (-1 when
 * the plan itself failed) — damage as data, never a throw */
final case class Bad(column: String, error: String, row: Long = -1L)
