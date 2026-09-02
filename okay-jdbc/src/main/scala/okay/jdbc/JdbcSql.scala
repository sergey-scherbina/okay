package okay.jdbc

import okay.{!, +, Async, Chunk, ChunkBuf, Chunks, Produce, async, effect}
import okay.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue}
import java.sql.{Connection, PreparedStatement, ResultSet, ResultSetMetaData, Types}

/**
 * The JDBC driver of the Sql seam (specs/sql.md): blocking calls
 * behind `Async.Run`, which virtual threads make honest on the JVM —
 * not a legacy shim but the right road for warehouses, DuckDB, H2
 * and everything with a battle-tested JVM driver. The raw
 * `JdbcInterop` streaming stays alongside, unchanged, for those who
 * want a ResultSet in hand.
 *
 * One driver instance wraps ONE connection, like the connection it
 * wraps: use it from one logical thread of control at a time.
 */
final class JdbcSql(conn: Connection, fetchSize: Int = 64) extends Sql:
  import JdbcSql.*

  private var inTx = false
  private var autoBefore = true

  def describe(sql: String): Vector[Col] ! Async = async {
    val ps = conn.prepareStatement(sql)
    try colsOf(ps.getMetaData)
    finally ps.close()
  }

  def query(sql: String, params: Vector[SqlValue])
  : Chunk[Vector[SqlValue]] ! (Produce + Async) =
    type F = Produce + Async

    def readChunk(rs: ResultSet, cols: Vector[SqlType]): Chunk[Vector[SqlValue]] =
      val buf = ChunkBuf[Vector[SqlValue]](fetchSize)
      var i = 0
      while i < fetchSize && rs.next() do
        buf(i) = rowOf(rs, cols)
        i += 1
      buf.take(i)

    def go(rs: ResultSet, ps: PreparedStatement, cols: Vector[SqlType])
    : Chunk[Vector[SqlValue]] ! F =
      effect[F, Chunk[Vector[SqlValue]]](Async.Run(() => readChunk(rs, cols))).flatMap { c =>
        if c.length < fetchSize then
          effect[F, Unit](Async.Run { () => rs.close(); ps.close() }).flatMap { _ =>
            if c.isEmpty then okay.pure(Chunks.emptyChunk)
            else effect[F, Chunk[Vector[SqlValue]]](c)
          }
        else effect[F, Chunk[Vector[SqlValue]]](c).flatMap(_ => go(rs, ps, cols))
      }

    effect[F, (ResultSet, PreparedStatement, Vector[SqlType])](Async.Run { () =>
      val ps = conn.prepareStatement(sql)
      ps.setFetchSize(fetchSize)
      bindAll(ps, params)
      val rs = ps.executeQuery()
      (rs, ps, colsOf(rs.getMetaData).map(_.tpe))
    }).flatMap(go)

  def update(sql: String, params: Vector[SqlValue]): Long ! Async = async {
    val ps = conn.prepareStatement(sql)
    try
      bindAll(ps, params)
      ps.executeUpdate().toLong
    finally ps.close()
  }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = async {
    val ps = conn.prepareStatement(sql)
    try
      rows.foreach { r => bindAll(ps, r); ps.addBatch() }
      ps.executeBatch().foldLeft(0L)((acc, n) => acc + math.max(n, 0))
    finally ps.close()
  }

  def begin(isolation: Isolation): Granted ! Async = async {
    if inTx then throw IllegalStateException(
      "nested transaction: this connection is already in one — " +
        "refuse rather than silently flatten (specs/jdbc.md)")
    autoBefore = conn.getAutoCommit
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(levelOf(isolation))
    inTx = true
    Granted(isolation, isolationOf(conn.getTransactionIsolation))
  }

  def commit(): Unit ! Async = async {
    conn.commit()
    conn.setAutoCommit(autoBefore)
    inTx = false
  }

  def rollback(): Unit ! Async = async {
    conn.rollback()
    conn.setAutoCommit(autoBefore)
    inTx = false
  }

  /** the sync emergency brake: a no-op unless a transaction is open */
  def cancel(): Unit =
    if inTx then
      conn.rollback()
      conn.setAutoCommit(autoBefore)
      inTx = false

object JdbcSql:

  private def colsOf(md: ResultSetMetaData): Vector[Col] =
    (1 to md.getColumnCount).toVector.map { i =>
      Col(md.getColumnLabel(i),
        typeOf(md.getColumnType(i), md.getColumnTypeName(i)),
        md.isNullable(i) != ResultSetMetaData.columnNoNulls)
    }

  /** java.sql.Types → the neutral vocabulary. NUMERIC/DECIMAL map to
   * F64 in v1 — stated, not hidden; a decimal algebra type joins
   * when a consumer hurts without it (specs/jdbc.md, out of scope) */
  private def typeOf(t: Int, vendorName: String): SqlType = t match
    case Types.BOOLEAN | Types.BIT => SqlType.Bool
    case Types.TINYINT | Types.SMALLINT | Types.INTEGER => SqlType.I32
    case Types.BIGINT => SqlType.I64
    case Types.FLOAT | Types.DOUBLE | Types.REAL |
         Types.NUMERIC | Types.DECIMAL => SqlType.F64
    case Types.CHAR | Types.VARCHAR | Types.LONGVARCHAR |
         Types.NCHAR | Types.NVARCHAR | Types.LONGNVARCHAR | Types.CLOB => SqlType.Text
    case Types.BINARY | Types.VARBINARY | Types.LONGVARBINARY | Types.BLOB => SqlType.Bytes
    // JDBC metadata does not name the element type; verify accepts
    // Arr(Other) and the typed decode checks the elements
    case Types.ARRAY => SqlType.Arr(SqlType.Other(vendorName))
    case _ => SqlType.Other(vendorName)

  /** an array element as the driver hands it back (java boxes) */
  private def valueOf(o: AnyRef): SqlValue = o match
    case null => SqlValue.Null
    case b: java.lang.Boolean => SqlValue.Bool(b)
    case i: java.lang.Integer => SqlValue.I32(i)
    case i: java.lang.Short => SqlValue.I32(i.toInt)
    case i: java.lang.Byte => SqlValue.I32(i.toInt)
    case l: java.lang.Long => SqlValue.I64(l)
    case d: java.lang.Double => SqlValue.F64(d)
    case f: java.lang.Float => SqlValue.F64(f.toDouble)
    case d: java.math.BigDecimal => SqlValue.F64(d.doubleValue)
    case s: String => SqlValue.Text(s)
    case bs: Array[Byte] => SqlValue.Bytes(bs)
    case a: java.sql.Array => arrayOf(a)
    case xs: Array[AnyRef] => SqlValue.Arr(xs.toVector.map(valueOf))
    case other => SqlValue.Text(other.toString)

  private def arrayOf(a: java.sql.Array): SqlValue =
    if a == null then SqlValue.Null
    else SqlValue.Arr(a.getArray.asInstanceOf[Array[AnyRef]].toVector.map(valueOf))

  private def rowOf(rs: ResultSet, cols: Vector[SqlType]): Vector[SqlValue] =
    Vector.tabulate(cols.length) { ix =>
      val i = ix + 1
      val v: SqlValue = cols(ix) match
        case SqlType.Bool => SqlValue.Bool(rs.getBoolean(i))
        case SqlType.I32 => SqlValue.I32(rs.getInt(i))
        case SqlType.I64 => SqlValue.I64(rs.getLong(i))
        case SqlType.F64 => SqlValue.F64(rs.getDouble(i))
        case SqlType.Text => SqlValue.Text(rs.getString(i))
        case SqlType.Bytes => SqlValue.Bytes(rs.getBytes(i))
        case SqlType.Arr(_) => arrayOf(rs.getArray(i))
        case SqlType.Other(_) | SqlType.Row(_) =>
          val s = rs.getString(i)
          SqlValue.Text(if s == null then "" else s)
      if rs.wasNull then SqlValue.Null else v
    }

  private def bindAll(ps: PreparedStatement, params: Vector[SqlValue]): Unit =
    var i = 0
    while i < params.length do
      params(i) match
        case SqlValue.Null => ps.setObject(i + 1, null)
        case SqlValue.Bool(v) => ps.setBoolean(i + 1, v)
        case SqlValue.I32(v) => ps.setInt(i + 1, v)
        case SqlValue.I64(v) => ps.setLong(i + 1, v)
        case SqlValue.F64(v) => ps.setDouble(i + 1, v)
        case SqlValue.Text(v) => ps.setString(i + 1, v)
        case SqlValue.Bytes(v) => ps.setBytes(i + 1, v)
        // an Object[] is what H2 (and the pg driver's setObject) take
        // for an ARRAY parameter; the vendor-typed createArrayOf road
        // is not needed for the engines this stack binds
        case SqlValue.Arr(elems) => ps.setObject(i + 1, elems.map(jdbcOf).toArray)
        case SqlValue.Row(_) => throw IllegalArgumentException(
          s"param ${i + 1}: a composite parameter is not bindable through JDBC")
      i += 1

  private def jdbcOf(v: SqlValue): AnyRef = v match
    case SqlValue.Null => null
    case SqlValue.Bool(b) => java.lang.Boolean.valueOf(b)
    case SqlValue.I32(x) => java.lang.Integer.valueOf(x)
    case SqlValue.I64(x) => java.lang.Long.valueOf(x)
    case SqlValue.F64(x) => java.lang.Double.valueOf(x)
    case SqlValue.Text(s) => s
    case SqlValue.Bytes(bs) => bs
    case SqlValue.Arr(elems) => elems.map(jdbcOf).toArray
    case SqlValue.Row(fields) => fields.map(jdbcOf).toArray

  private def levelOf(i: Isolation): Int = i match
    case Isolation.ReadCommitted => Connection.TRANSACTION_READ_COMMITTED
    case Isolation.RepeatableRead => Connection.TRANSACTION_REPEATABLE_READ
    case Isolation.Serializable => Connection.TRANSACTION_SERIALIZABLE

  private def isolationOf(level: Int): Isolation = level match
    case Connection.TRANSACTION_SERIALIZABLE => Isolation.Serializable
    case Connection.TRANSACTION_REPEATABLE_READ => Isolation.RepeatableRead
    case _ => Isolation.ReadCommitted
