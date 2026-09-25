package okay2.jdbc

import java.sql.{Connection, PreparedStatement, ResultSet, ResultSetMetaData, Types}
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue, Temporal}
import okay2.stream.{Chunk, ChunkBuf, Source}

/**
 * The JDBC driver of the Sql seam (okay-jdbc's JdbcSql.scala): blocking
 * calls behind `Async.Run`, which virtual threads make honest on the JVM.
 * One instance wraps ONE connection, like the connection it wraps: use
 * it from one logical thread of control at a time.
 */
final class JdbcSql(conn: Connection, fetchSize: Int = 64) extends Sql {
  import JdbcSql._

  private var inTx = false
  private var autoBefore = true
  private var isolationBefore = Connection.TRANSACTION_READ_COMMITTED
  private var readOnlyBefore = false
  private var failedInTx = false

  /** run a statement, remembering a failure inside a transaction */
  private def guarded[A](body: => A): A =
    try body
    catch {
      case e: java.sql.SQLException =>
        if (inTx) failedInTx = true
        throw e
    }

  def describe(sql: String): Vector[Col] ! Async = Async {
    val ps = conn.prepareStatement(sql)
    try colsOf(ps.getMetaData)
    finally ps.close()
  }

  /** the rows, `fetchSize` at a time; the statement closes after the
   * last short chunk */
  def query(sql: String, params: Vector[SqlValue]): Source[Chunk[Vector[SqlValue]]] = {
    type W = Chunk[Vector[SqlValue]]

    def readChunk(rs: ResultSet, cols: Vector[SqlType], codes: Vector[Int]): W = {
      val buf = ChunkBuf[Vector[SqlValue]](fetchSize)
      var i = 0
      while (i < fetchSize && rs.next()) {
        buf(i) = rowOf(rs, cols, codes)
        i += 1
      }
      buf.take(i)
    }

    def go(rs: ResultSet, ps: PreparedStatement, cols: Vector[SqlType], codes: Vector[Int]): Source[W] =
      Async(readChunk(rs, cols, codes)).flatMap { (c: W) =>
        if (c.length < fetchSize)
          Async { rs.close(); ps.close() }.flatMap { _ =>
            if (c.isEmpty) pure[Writer[W] with Async, Unit](()) else Writer.tell[W](c)
          }
        else Writer.tell[W](c).flatMap(_ => go(rs, ps, cols, codes))
      }

    Async {
      val ps = conn.prepareStatement(sql)
      ps.setFetchSize(fetchSize)
      bindAll(ps, params)
      val rs = guarded(ps.executeQuery())
      val md = rs.getMetaData
      (rs, ps, colsOf(md).map(_.tpe),
        (1 to md.getColumnCount).toVector.map(i => zonedCode(md.getColumnType(i), md.getColumnTypeName(i))))
    }.flatMap[Writer[W] with Async, Unit] { case (rs, ps, cols, codes) => go(rs, ps, cols, codes) }
  }

  def update(sql: String, params: Vector[SqlValue]): Long ! Async = Async {
    val ps = conn.prepareStatement(sql)
    try {
      bindAll(ps, params)
      guarded(ps.executeUpdate()).toLong
    } finally ps.close()
  }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = Async {
    val ps = conn.prepareStatement(sql)
    try {
      rows.foreach { r => bindAll(ps, r); ps.addBatch() }
      guarded(ps.executeBatch()).foldLeft(0L)((acc, n) => acc + math.max(n, 0))
    } finally ps.close()
  }

  def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = Async {
    if (inTx) throw new IllegalStateException(
      "nested transaction: this connection is already in one — " +
        "refuse rather than silently flatten (specs/jdbc.md)")
    autoBefore = conn.getAutoCommit
    isolationBefore = conn.getTransactionIsolation
    readOnlyBefore = conn.isReadOnly
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(levelOf(isolation))
    if (readOnly) conn.setReadOnly(true)
    inTx = true
    Granted(isolation, isolationOf(conn.getTransactionIsolation), readOnly && conn.isReadOnly)
  }

  /** a COMMIT after a failed statement is refused when the engine has
   * aborted the transaction: nothing in the region is committed */
  def commit(): Unit ! Async = Async {
    if (failedInTx) {
      val alive =
        try { val st = conn.createStatement(); try st.execute("select 1") finally st.close(); true }
        catch { case _: java.sql.SQLException => false }
      if (!alive) {
        try conn.rollback() finally restore()
        throw new java.sql.SQLException(
          "COMMIT refused: an earlier statement failed and the engine aborted the transaction — " +
            "nothing in the region is committed (jdbc-tails)", "40000")
      }
    }
    conn.commit()
    restore()
  }

  def rollback(): Unit ! Async = Async {
    conn.rollback()
    restore()
  }

  private def restore(): Unit = {
    conn.setAutoCommit(autoBefore)
    conn.setTransactionIsolation(isolationBefore)
    conn.setReadOnly(readOnlyBefore)
    inTx = false
    failedInTx = false
  }

  def close(): Unit = conn.close()

  override def sqlState(t: Throwable): Option[String] = t match {
    case e: java.sql.SQLException => Option(e.getSQLState)
    case _ => None
  }

  def cancel(): Unit =
    if (inTx) {
      conn.rollback()
      restore()
    }
}

object JdbcSql {

  private def colsOf(md: ResultSetMetaData): Vector[Col] =
    (1 to md.getColumnCount).toVector.map { i =>
      Col(md.getColumnLabel(i), typeOf(md.getColumnType(i), md.getColumnTypeName(i)),
        md.isNullable(i) != ResultSetMetaData.columnNoNulls)
    }

  private def textual[N](s: String, parse: String => Option[N], mk: N => SqlValue): SqlValue =
    if (s == null) SqlValue.Null else parse(s).fold[SqlValue](SqlValue.Text(s))(mk)

  private def microsOf(t: java.sql.Timestamp): Long =
    Math.floorDiv(t.getTime, 1000L) * 1000000L + t.getNanos / 1000L

  private def microsOf(i: java.time.Instant): Long =
    i.getEpochSecond * 1000000L + i.getNano / 1000L

  private def instantOf(us: Long): java.time.Instant =
    java.time.Instant.ofEpochSecond(Math.floorDiv(us, 1000000L), Math.floorMod(us, 1000000L) * 1000L)

  /** a `timestamp with time zone` some drivers report as TIMESTAMP */
  private def zonedCode(code: Int, name: String): Int = {
    val n = if (name == null) "" else name.toLowerCase
    if (code == Types.TIMESTAMP && (n.endsWith("tz") || n.contains("with time zone"))) Types.TIMESTAMP_WITH_TIMEZONE
    else code
  }

  private def bindTimestamp(ps: PreparedStatement, i: Int, us: Long, paramType: Int => Int): Unit =
    paramType(i) match {
      case Types.TIMESTAMP_WITH_TIMEZONE =>
        ps.setObject(i, java.time.OffsetDateTime.ofInstant(instantOf(us), java.time.ZoneOffset.UTC))
      case Types.TIMESTAMP =>
        ps.setObject(i, java.time.LocalDateTime.ofInstant(instantOf(us), java.time.ZoneOffset.UTC))
      case _ => ps.setString(i, Temporal.renderTimestamp(us))
    }

  private def typeOf(t: Int, vendorName: String): SqlType = t match {
    case _ if vendorName != null && vendorName.equalsIgnoreCase("uuid") => SqlType.Uuid
    case _ if vendorName != null && (vendorName.equalsIgnoreCase("json") || vendorName.equalsIgnoreCase("jsonb")) => SqlType.Json
    case Types.TIMESTAMP | Types.TIMESTAMP_WITH_TIMEZONE => SqlType.Timestamp
    case Types.DATE => SqlType.Date
    case Types.TIME => SqlType.Time
    case Types.BOOLEAN | Types.BIT => SqlType.Bool
    case Types.TINYINT | Types.SMALLINT | Types.INTEGER => SqlType.I32
    case Types.BIGINT => SqlType.I64
    case Types.FLOAT | Types.DOUBLE | Types.REAL => SqlType.F64
    case Types.NUMERIC | Types.DECIMAL => SqlType.Num
    case Types.CHAR | Types.VARCHAR | Types.LONGVARCHAR |
         Types.NCHAR | Types.NVARCHAR | Types.LONGNVARCHAR | Types.CLOB => SqlType.Text
    case Types.BINARY | Types.VARBINARY | Types.LONGVARBINARY | Types.BLOB => SqlType.Bytes
    case Types.ARRAY => SqlType.Arr(SqlType.Other(vendorName))
    case _ => SqlType.Other(vendorName)
  }

  private def valueOf(o: Any): SqlValue = o match {
    case null => SqlValue.Null
    case b: java.lang.Boolean => SqlValue.Bool(b)
    case i: java.lang.Integer => SqlValue.I32(i)
    case i: java.lang.Short => SqlValue.I32(i.toInt)
    case i: java.lang.Byte => SqlValue.I32(i.toInt)
    case l: java.lang.Long => SqlValue.I64(l)
    case d: java.lang.Double => SqlValue.F64(d)
    case f: java.lang.Float => SqlValue.F64(f.toDouble)
    case d: java.math.BigDecimal => SqlValue.Num(BigDecimal(d))
    case s: String => SqlValue.Text(s)
    case bs: Array[Byte] => SqlValue.Bytes(bs)
    case t: java.sql.Timestamp => SqlValue.Timestamp(microsOf(t))
    case t: java.time.OffsetDateTime => SqlValue.Timestamp(microsOf(t.toInstant))
    case t: java.time.Instant => SqlValue.Timestamp(microsOf(t))
    case t: java.time.LocalDateTime => SqlValue.Timestamp(microsOf(t.toInstant(java.time.ZoneOffset.UTC)))
    case d: java.sql.Date => SqlValue.Date(d.toLocalDate.toEpochDay.toInt)
    case d: java.time.LocalDate => SqlValue.Date(d.toEpochDay.toInt)
    case t: java.sql.Time => SqlValue.Time(t.toLocalTime.toNanoOfDay / 1000L)
    case t: java.time.LocalTime => SqlValue.Time(t.toNanoOfDay / 1000L)
    case u: java.util.UUID => SqlValue.Uuid(u)
    case a: java.sql.Array => arrayOf(a)
    case xs: Array[AnyRef] => SqlValue.Arr(xs.toVector.map(valueOf))
    case other => SqlValue.Text(other.toString)
  }

  private def arrayOf(a: java.sql.Array): SqlValue =
    if (a == null) SqlValue.Null
    else a.getArray match {
      case arr: Array[_] => SqlValue.Arr(Vector.tabulate(scala.runtime.ScalaRunTime.array_length(arr))(i =>
        valueOf(scala.runtime.ScalaRunTime.array_apply(arr, i))))
      case other => SqlValue.Text(other.toString)
    }

  private def rowOf(rs: ResultSet, cols: Vector[SqlType], codes: Vector[Int]): Vector[SqlValue] =
    Vector.tabulate(cols.length) { ix =>
      val i = ix + 1
      cols(ix) match {
        case SqlType.Timestamp =>
          try {
            if (codes(ix) == Types.TIMESTAMP_WITH_TIMEZONE) {
              val t = rs.getObject(i, classOf[java.time.OffsetDateTime])
              if (t == null) SqlValue.Null else SqlValue.Timestamp(microsOf(t.toInstant))
            } else {
              val t = rs.getObject(i, classOf[java.time.LocalDateTime])
              if (t == null) SqlValue.Null else SqlValue.Timestamp(microsOf(t.toInstant(java.time.ZoneOffset.UTC)))
            }
          } catch { case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseTimestamp, SqlValue.Timestamp(_)) }
        case SqlType.Date =>
          try {
            val d = rs.getObject(i, classOf[java.time.LocalDate])
            if (d == null) SqlValue.Null else SqlValue.Date(d.toEpochDay.toInt)
          } catch { case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseDate, SqlValue.Date(_)) }
        case SqlType.Time =>
          try {
            val t = rs.getObject(i, classOf[java.time.LocalTime])
            if (t == null) SqlValue.Null else SqlValue.Time(t.toNanoOfDay / 1000L)
          } catch { case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseTime, SqlValue.Time(_)) }
        case SqlType.Uuid => rs.getObject(i) match {
          case null => SqlValue.Null
          case u: java.util.UUID => SqlValue.Uuid(u)
          case s: String => SqlValue.Uuid(java.util.UUID.fromString(s))
          case bs: Array[Byte] if bs.length == 16 =>
            val bb = java.nio.ByteBuffer.wrap(bs)
            SqlValue.Uuid(new java.util.UUID(bb.getLong, bb.getLong))
          case other => SqlValue.Text(other.toString)
        }
        case SqlType.Json =>
          val s = rs.getString(i)
          if (s == null) SqlValue.Null else SqlValue.Json(s)
        case SqlType.Num =>
          val d = rs.getBigDecimal(i)
          if (d == null) SqlValue.Null else SqlValue.Num(BigDecimal(d))
        case t =>
          val v: SqlValue = t match {
            case SqlType.Bool => SqlValue.Bool(rs.getBoolean(i))
            case SqlType.I32 => SqlValue.I32(rs.getInt(i))
            case SqlType.I64 => SqlValue.I64(rs.getLong(i))
            case SqlType.F64 => SqlValue.F64(rs.getDouble(i))
            case SqlType.Text => SqlValue.Text(rs.getString(i))
            case SqlType.Bytes => SqlValue.Bytes(rs.getBytes(i))
            case SqlType.Arr(_) => arrayOf(rs.getArray(i))
            case _ =>
              val s = rs.getString(i)
              SqlValue.Text(if (s == null) "" else s)
          }
          if (rs.wasNull) SqlValue.Null else v
      }
    }

  private def bindAll(ps: PreparedStatement, params: Vector[SqlValue]): Unit = {
    // asked once, and only when a timestamp needs it: SQLite has no
    // parameter metadata, and the ISO text is the answer there
    lazy val declared: Int => Int =
      try {
        val md = ps.getParameterMetaData
        (i: Int) => try zonedCode(md.getParameterType(i), md.getParameterTypeName(i)) catch { case _: java.sql.SQLException => Types.OTHER }
      } catch { case _: java.sql.SQLException => (_: Int) => Types.OTHER }
    var i = 0
    while (i < params.length) {
      params(i) match {
        case SqlValue.Null => ps.setObject(i + 1, null)
        case SqlValue.Bool(v) => ps.setBoolean(i + 1, v)
        case SqlValue.I32(v) => ps.setInt(i + 1, v)
        case SqlValue.I64(v) => ps.setLong(i + 1, v)
        case SqlValue.F64(v) => ps.setDouble(i + 1, v)
        case SqlValue.Num(v) => ps.setBigDecimal(i + 1, v.bigDecimal)
        case SqlValue.Text(v) => ps.setString(i + 1, v)
        case SqlValue.Bytes(v) => ps.setBytes(i + 1, v)
        case SqlValue.Timestamp(us) => bindTimestamp(ps, i + 1, us, declared)
        case SqlValue.Date(d) =>
          try ps.setObject(i + 1, java.time.LocalDate.ofEpochDay(d.toLong))
          catch { case _: java.sql.SQLException => ps.setString(i + 1, Temporal.renderDate(d)) }
        case SqlValue.Time(us) =>
          try ps.setObject(i + 1, java.time.LocalTime.ofNanoOfDay(us * 1000L))
          catch { case _: java.sql.SQLException => ps.setString(i + 1, Temporal.renderTime(us)) }
        case SqlValue.Uuid(u) => ps.setObject(i + 1, u)
        case SqlValue.Json(s) => ps.setString(i + 1, s)
        case SqlValue.Arr(elems) => ps.setObject(i + 1, elems.map(jdbcOf).toArray)
        case SqlValue.Row(_) => throw new IllegalArgumentException(
          s"param ${i + 1}: a composite parameter is not bindable through JDBC")
      }
      i += 1
    }
  }

  private def jdbcOf(v: SqlValue): AnyRef = v match {
    case SqlValue.Null => null
    case SqlValue.Bool(b) => java.lang.Boolean.valueOf(b)
    case SqlValue.I32(x) => java.lang.Integer.valueOf(x)
    case SqlValue.I64(x) => java.lang.Long.valueOf(x)
    case SqlValue.F64(x) => java.lang.Double.valueOf(x)
    case SqlValue.Num(x) => x.bigDecimal
    case SqlValue.Text(s) => s
    case SqlValue.Bytes(bs) => bs
    case SqlValue.Timestamp(us) => java.time.OffsetDateTime.ofInstant(instantOf(us), java.time.ZoneOffset.UTC)
    case SqlValue.Date(d) => java.time.LocalDate.ofEpochDay(d.toLong)
    case SqlValue.Time(us) => java.time.LocalTime.ofNanoOfDay(us * 1000L)
    case SqlValue.Uuid(u) => u
    case SqlValue.Json(s) => s
    case SqlValue.Arr(elems) => elems.map(jdbcOf).toArray
    case SqlValue.Row(fields) => fields.map(jdbcOf).toArray
  }

  private def levelOf(i: Isolation): Int = i match {
    case Isolation.ReadCommitted => Connection.TRANSACTION_READ_COMMITTED
    case Isolation.RepeatableRead => Connection.TRANSACTION_REPEATABLE_READ
    case Isolation.Serializable => Connection.TRANSACTION_SERIALIZABLE
  }

  private def isolationOf(level: Int): Isolation = level match {
    case Connection.TRANSACTION_SERIALIZABLE => Isolation.Serializable
    case Connection.TRANSACTION_REPEATABLE_READ => Isolation.RepeatableRead
    case _ => Isolation.ReadCommitted
  }

}
