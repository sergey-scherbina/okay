package okay.jdbc

import okay.{!, +, %, Async, async, Chunk, ChunkBuf, effect, Source, Writer}
import okay.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue, Temporal}
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
  // restored with autocommit when the region ends (sql-readonly-region):
  // a transact(Serializable) must not leave the connection Serializable
  private var isolationBefore = Connection.TRANSACTION_READ_COMMITTED
  private var readOnlyBefore = false
  // a statement failed inside the open transaction (jdbc-tails): some
  // engines (Postgres) abort the whole transaction on any error and
  // answer the following COMMIT with ROLLBACK and no exception —
  // pgjdbc included, measured. So COMMIT after a failure PROBES first.
  private var failedInTx = false

  /** run a statement, remembering a failure inside a transaction */
  private def guarded[A](body: => A): A =
    try body
    catch case e: java.sql.SQLException =>
      if inTx then failedInTx = true
      throw e

  def describe(sql: String): Vector[Col] ! Async = async {
    val ps = conn.prepareStatement(sql)
    try colsOf(ps.getMetaData)
    finally ps.close()
  }

  def query(sql: String, params: Vector[SqlValue])
  : Source[Chunk[Vector[SqlValue]]] =
    type F = Writer % Chunk[Vector[SqlValue]] + Async

    def readChunk(rs: ResultSet, cols: Vector[SqlType], codes: Vector[Int]): Chunk[Vector[SqlValue]] =
      val buf = ChunkBuf[Vector[SqlValue]](fetchSize)
      var i = 0
      while i < fetchSize && rs.next() do
        buf(i) = rowOf(rs, cols, codes)
        i += 1
      buf.take(i)

    def go(rs: ResultSet, ps: PreparedStatement, cols: Vector[SqlType], codes: Vector[Int])
    : Source[Chunk[Vector[SqlValue]]] =
      effect[F, Chunk[Vector[SqlValue]]](Async.Run(() => readChunk(rs, cols, codes))).flatMap { c =>
        if c.length < fetchSize then
          effect[F, Unit](Async.Run { () => rs.close(); ps.close() }).flatMap { _ =>
            if c.isEmpty then okay.pure(())
            else effect[F, Unit](Writer(c))
          }
        else effect[F, Unit](Writer(c)).flatMap(_ => go(rs, ps, cols, codes))
      }

    effect[F, (ResultSet, PreparedStatement, Vector[SqlType], Vector[Int])](Async.Run { () =>
      val ps = conn.prepareStatement(sql)
      ps.setFetchSize(fetchSize)
      bindAll(ps, params)
      val rs = guarded(ps.executeQuery())
      val md = rs.getMetaData
      (rs, ps, colsOf(md).map(_.tpe),
        (1 to md.getColumnCount).toVector.map(i => zonedCode(md.getColumnType(i), md.getColumnTypeName(i))))
    }).flatMap(go)

  def update(sql: String, params: Vector[SqlValue]): Long ! Async = async {
    val ps = conn.prepareStatement(sql)
    try
      bindAll(ps, params)
      guarded(ps.executeUpdate()).toLong
    finally ps.close()
  }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = async {
    val ps = conn.prepareStatement(sql)
    try
      rows.foreach { r => bindAll(ps, r); ps.addBatch() }
      guarded(ps.executeBatch()).foldLeft(0L)((acc, n) => acc + math.max(n, 0))
    finally ps.close()
  }

  def begin(isolation: Isolation, readOnly: Boolean): Granted ! Async = async {
    if inTx then throw IllegalStateException(
      "nested transaction: this connection is already in one — " +
        "refuse rather than silently flatten (specs/jdbc.md)")
    autoBefore = conn.getAutoCommit
    isolationBefore = conn.getTransactionIsolation
    readOnlyBefore = conn.isReadOnly
    conn.setAutoCommit(false)
    conn.setTransactionIsolation(levelOf(isolation))
    if readOnly then conn.setReadOnly(true)
    inTx = true
    // JDBC's setReadOnly is a HINT; what the connection reports back is
    // what was granted (H2 ignores it and reports false; pg enforces it)
    Granted(isolation, isolationOf(conn.getTransactionIsolation), readOnly && conn.isReadOnly)
  }

  def commit(): Unit ! Async = async {
    if failedInTx then
      // is the transaction still alive? on pg the probe answers 25P02
      // ("current transaction is aborted") and COMMIT would silently
      // roll back; then this region must FAIL, not report success
      val alive =
        try { val st = conn.createStatement(); try st.execute("select 1") finally st.close(); true }
        catch case _: java.sql.SQLException => false
      if !alive then
        try conn.rollback() finally restore()
        throw java.sql.SQLException(
          "COMMIT refused: an earlier statement failed and the engine aborted the transaction — " +
            "nothing in the region is committed (jdbc-tails)", "40000")
    conn.commit()
    restore()
  }

  def rollback(): Unit ! Async = async {
    conn.rollback()
    restore()
  }

  private def restore(): Unit =
    conn.setAutoCommit(autoBefore)
    conn.setTransactionIsolation(isolationBefore)
    conn.setReadOnly(readOnlyBefore)
    inTx = false
    failedInTx = false

  /** closes the connection (a pooled one goes back to its pool) */
  def close(): Unit = conn.close()

  /** the engine's SQLSTATE, as JDBC carries it */
  override def sqlState(t: Throwable): Option[String] = t match
    case e: java.sql.SQLException => Option(e.getSQLState)
    case _ => None

  /** the sync emergency brake: a no-op unless a transaction is open */
  def cancel(): Unit =
    if inTx then
      conn.rollback()
      restore()

object JdbcSql:

  private def colsOf(md: ResultSetMetaData): Vector[Col] =
    (1 to md.getColumnCount).toVector.map { i =>
      Col(md.getColumnLabel(i),
        typeOf(md.getColumnType(i), md.getColumnTypeName(i)),
        md.isNullable(i) != ResultSetMetaData.columnNoNulls)
    }

  /** java.sql.Types → the neutral vocabulary. NUMERIC/DECIMAL are
   * exact (`Num`, pg-scalar-types) — the v1 F64 mapping rounded */
  /** the text fallback of a temporal read: parsed when the form is
   * known, the text itself when not — loud in the type */
  private def textual[N](s: String, parse: String => Option[N], mk: N => SqlValue): SqlValue =
    if s == null then SqlValue.Null else parse(s).fold(SqlValue.Text(s))(mk)

  private def microsOf(t: java.sql.Timestamp): Long =
    Math.floorDiv(t.getTime, 1000L) * 1000000L + t.getNanos / 1000L

  private def microsOf(i: java.time.Instant): Long =
    i.getEpochSecond * 1000000L + i.getNano / 1000L

  private def instantOf(us: Long): java.time.Instant =
    java.time.Instant.ofEpochSecond(Math.floorDiv(us, 1000000L), Math.floorMod(us, 1000000L) * 1000L)

  /** a Timestamp param by the DECLARED parameter type: an
   * OffsetDateTime at UTC into a `timestamp with time zone`, a
   * LocalDateTime (the UTC wall clock) into a `timestamp`; a driver
   * that cannot describe its parameters (SQLite) takes ISO text */
  /** the JDBC code, with WITH_TIMEZONE forced where the vendor NAME
   * says so: pgjdbc reports a `timestamptz` as plain TIMESTAMP, and
   * binding a wall clock into it shifts by the session zone (measured
   * under Europe/Kyiv: three hours) */
  private def zonedCode(code: Int, name: String): Int =
    val n = if name == null then "" else name.toLowerCase
    if code == Types.TIMESTAMP && (n.endsWith("tz") || n.contains("with time zone")) then Types.TIMESTAMP_WITH_TIMEZONE
    else code

  private def bindTimestamp(ps: PreparedStatement, i: Int, us: Long, paramType: Int => Int): Unit =
    paramType(i) match
      case Types.TIMESTAMP_WITH_TIMEZONE =>
        ps.setObject(i, java.time.OffsetDateTime.ofInstant(instantOf(us), java.time.ZoneOffset.UTC))
      case Types.TIMESTAMP =>
        ps.setObject(i, java.time.LocalDateTime.ofInstant(instantOf(us), java.time.ZoneOffset.UTC))
      case _ => ps.setString(i, Temporal.renderTimestamp(us))

  private def typeOf(t: Int, vendorName: String): SqlType = t match
    // named before the code: H2 reports UUID under BINARY, pg under OTHER
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
    // JDBC metadata does not name the element type; verify accepts
    // Arr(Other) and the typed decode checks the elements
    case Types.ARRAY => SqlType.Arr(SqlType.Other(vendorName))
    case _ => SqlType.Other(vendorName)

  /** an array element as the driver hands it back (java boxes). Arrays
   * nest as deep as the VALUE does, and nothing bounds a value, so the
   * walk keeps its own stack: a container opens a frame, a finished
   * frame is its parent's next element (stack-safety-catch-up-okay2) */
  private[jdbc] def valueOf(o: Any): SqlValue =
    val stack = java.util.ArrayDeque[Reading]()
    var v = readOrOpen(o, stack)                 // null: a frame was opened
    while !stack.isEmpty do
      val top = stack.peek()
      if v != null then top.out += v
      v =
        if top.at < top.size then
          val x = scala.runtime.ScalaRunTime.array_apply(top.arr, top.at)
          top.at += 1
          readOrOpen(x, stack)
        else { stack.pop(): Unit; SqlValue.Arr(top.out.result()) }
    v

  /** an array being read: any component type (a primitive int[]
   * included), walked by the runtime */
  private final class Reading(val arr: AnyRef):
    val size: Int = scala.runtime.ScalaRunTime.array_length(arr)
    var at = 0
    val out = Vector.newBuilder[SqlValue]

  /** a scalar's value; an array opens a frame on `stack` and answers null */
  private def readOrOpen(o: Any, stack: java.util.ArrayDeque[Reading]): SqlValue = o match
    case a: java.sql.Array => a.getArray match
      case arr: Array[?] => stack.push(Reading(arr)); null
      case other => SqlValue.Text(other.toString)
    case xs: Array[AnyRef] => stack.push(Reading(xs)); null
    case _ => scalarOf(o)

  private def scalarOf(o: Any): SqlValue = o match
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
    case other => SqlValue.Text(other.toString)

  private def arrayOf(a: java.sql.Array): SqlValue = if a == null then SqlValue.Null else valueOf(a)

  /** the temporal reads are java.time objects by the column's JDBC
   * code — a `timestamp with time zone` as an OffsetDateTime (absolute),
   * a `timestamp` as a LocalDateTime read as UTC — so no session or
   * JVM zone enters (sql-temporal-types). The Calendar road is NOT
   * used: H2 stamps the session offset onto a UTC calendar's wall
   * clock, measured. A driver without the JDBC 4.2 getObject(Class)
   * (SQLite) falls back to the column's text through `Temporal` —
   * SQLite's driver TAKES getObject(LocalDateTime) and then fails to
   * parse ISO text with its own format (DateTimeParseException), so
   * the fallback catches any non-fatal failure, not only SQL ones. */
  private def rowOf(rs: ResultSet, cols: Vector[SqlType], codes: Vector[Int]): Vector[SqlValue] =
    Vector.tabulate(cols.length) { ix =>
      val i = ix + 1
      cols(ix) match
        case SqlType.Timestamp =>
          try
            if codes(ix) == Types.TIMESTAMP_WITH_TIMEZONE then
              val t = rs.getObject(i, classOf[java.time.OffsetDateTime])
              if t == null then SqlValue.Null else SqlValue.Timestamp(microsOf(t.toInstant))
            else
              val t = rs.getObject(i, classOf[java.time.LocalDateTime])
              if t == null then SqlValue.Null else SqlValue.Timestamp(microsOf(t.toInstant(java.time.ZoneOffset.UTC)))
          catch case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseTimestamp, SqlValue.Timestamp(_))
        case SqlType.Date =>
          try
            val d = rs.getObject(i, classOf[java.time.LocalDate])
            if d == null then SqlValue.Null else SqlValue.Date(d.toEpochDay.toInt)
          catch case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseDate, SqlValue.Date(_))
        case SqlType.Time =>
          try
            val t = rs.getObject(i, classOf[java.time.LocalTime])
            if t == null then SqlValue.Null else SqlValue.Time(t.toNanoOfDay / 1000L)
          catch case scala.util.control.NonFatal(_) => textual(rs.getString(i), Temporal.parseTime, SqlValue.Time(_))
        case SqlType.Uuid => rs.getObject(i) match
          case null => SqlValue.Null
          case u: java.util.UUID => SqlValue.Uuid(u)
          case s: String => SqlValue.Uuid(java.util.UUID.fromString(s))
          case bs: Array[Byte] if bs.length == 16 =>
            val bb = java.nio.ByteBuffer.wrap(bs)
            SqlValue.Uuid(java.util.UUID(bb.getLong, bb.getLong))
          case other => SqlValue.Text(other.toString)
        case SqlType.Json =>
          val s = rs.getString(i)
          if s == null then SqlValue.Null else SqlValue.Json(s)
        // the reference reads carry their own null; sqlite-jdbc's
        // getBigDecimal does not mark the column, so wasNull after it
        // throws — decide nullness from the value here
        case SqlType.Num =>
          val d = rs.getBigDecimal(i)
          if d == null then SqlValue.Null else SqlValue.Num(BigDecimal(d))
        case t =>
          val v: SqlValue = t match
            case SqlType.Bool => SqlValue.Bool(rs.getBoolean(i))
            case SqlType.I32 => SqlValue.I32(rs.getInt(i))
            case SqlType.I64 => SqlValue.I64(rs.getLong(i))
            case SqlType.F64 => SqlValue.F64(rs.getDouble(i))
            case SqlType.Text => SqlValue.Text(rs.getString(i))
            case SqlType.Bytes => SqlValue.Bytes(rs.getBytes(i))
            case SqlType.Arr(_) => arrayOf(rs.getArray(i))
            // Num and the temporal kinds were answered above; the
            // compiler wants the match total
            case SqlType.Num | SqlType.Other(_) | SqlType.Row(_) | SqlType.Timestamp | SqlType.Date
               | SqlType.Time | SqlType.Uuid | SqlType.Json =>
              val s = rs.getString(i)
              SqlValue.Text(if s == null then "" else s)
          if rs.wasNull then SqlValue.Null else v
    }

  private def bindAll(ps: PreparedStatement, params: Vector[SqlValue]): Unit =
    // the declared parameter types, asked for once and only when a
    // temporal param needs them (a describe round trip on pg)
    lazy val declared: Int => Int =
      try
        val md = ps.getParameterMetaData
        i => try zonedCode(md.getParameterType(i), md.getParameterTypeName(i)) catch case _: java.sql.SQLException => Types.OTHER
      catch case _: java.sql.SQLException => _ => Types.OTHER
    var i = 0
    while i < params.length do
      params(i) match
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
          catch case _: java.sql.SQLException => ps.setString(i + 1, Temporal.renderDate(d))
        case SqlValue.Time(us) =>
          try ps.setObject(i + 1, java.time.LocalTime.ofNanoOfDay(us * 1000L))
          catch case _: java.sql.SQLException => ps.setString(i + 1, Temporal.renderTime(us))
        case SqlValue.Uuid(u) => ps.setObject(i + 1, u)
        // json binds as its text; a jsonb column on pg wants the
        // DBA's `?::jsonb` in the statement (bind-don't-model)
        case SqlValue.Json(s) => ps.setString(i + 1, s)
        // an Object[] is what H2 (and the pg driver's setObject) take
        // for an ARRAY parameter; the vendor-typed createArrayOf road
        // is not needed for the engines this stack binds
        case SqlValue.Arr(elems) => ps.setObject(i + 1, elems.map(jdbcOf).toArray)
        case SqlValue.Row(_) => throw IllegalArgumentException(
          s"param ${i + 1}: a composite parameter is not bindable through JDBC")
      i += 1

  /** what a statement binds for `v`: an array or a composite as an
   * `Array[AnyRef]`, as deep as the value, on its own stack as `valueOf` */
  private[jdbc] def jdbcOf(v: SqlValue): AnyRef =
    val stack = java.util.ArrayDeque[Binding]()
    var r = bindOrOpen(v, stack)
    while !stack.isEmpty do
      val top = stack.peek()
      if r ne Opened then { top.out(top.at) = r; top.at += 1 }
      r =
        if top.at < top.out.length then bindOrOpen(top.elems(top.at), stack)
        else { stack.pop(): Unit; top.out }
    r

  /** a value being bound: its elements and the array they go into */
  private final class Binding(val elems: Vector[SqlValue]):
    val out = new Array[AnyRef](elems.length)
    var at = 0

  /** what `bindOrOpen` answers when it opened a frame (null is SQL NULL) */
  private val Opened = new AnyRef

  private def bindOrOpen(v: SqlValue, stack: java.util.ArrayDeque[Binding]): AnyRef = v match
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
    case SqlValue.Arr(elems) => stack.push(Binding(elems)); Opened
    case SqlValue.Row(fields) => stack.push(Binding(fields)); Opened

  private def levelOf(i: Isolation): Int = i match
    case Isolation.ReadCommitted => Connection.TRANSACTION_READ_COMMITTED
    case Isolation.RepeatableRead => Connection.TRANSACTION_REPEATABLE_READ
    case Isolation.Serializable => Connection.TRANSACTION_SERIALIZABLE

  private def isolationOf(level: Int): Isolation = level match
    case Connection.TRANSACTION_SERIALIZABLE => Isolation.Serializable
    case Connection.TRANSACTION_REPEATABLE_READ => Isolation.RepeatableRead
    case _ => Isolation.ReadCommitted
