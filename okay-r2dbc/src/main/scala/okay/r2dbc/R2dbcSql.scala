package okay.r2dbc

import okay.{!, +, Async, Chunk, ChunkBuf, Chunks, Produce, async, effect}
import okay.sql.{Col, Granted, Isolation, Sql, SqlType, SqlValue}
import io.r2dbc.spi.{Connection, ColumnMetadata, IsolationLevel, Result, Row, RowMetadata, Statement}
import java.nio.ByteBuffer

/**
 * The R2DBC hatch of the Sql seam (specs/sql.md, sql-r2dbc): any
 * `io.r2dbc.spi.Connection` behind the same trait — the typed layer
 * (rows/params/verify/transact) runs over it unchanged. Honestly
 * framed: on virtual threads this buys DRIVER AVAILABILITY (MSSQL,
 * Oracle, MySQL, and any engine with a maintained reactive driver),
 * not speed — the reactive publishers are pulled behind `Async.Run`
 * exactly as JDBC's blocking calls are parked there.
 *
 * Parameters are positional and bound by index (`$1..` in the SQL for
 * pg and H2's r2dbc drivers); one instance wraps ONE connection.
 */
final class R2dbcSql(conn: Connection, fetchSize: Int = 64) extends Sql:
  import R2dbcSql.*

  private var inTx = false

  /** R2DBC exposes column metadata only WITH a row: describe runs the
   * statement asking for one row and reads that row's metadata. A
   * statement with parameters, or one answering no rows, describes as
   * EMPTY — the hatch's stated limit; verify then names every column
   * missing, which is the honest answer, not a guess. */
  def describe(sql: String): Vector[Col] ! Async = async {
    val st = conn.createStatement(sql).fetchSize(1)
    var cols = Vector.empty[Col]
    eachResult(st) { r =>
      if cols.isEmpty then cols = Rx.first(r.map((_: Row, md: RowMetadata) => colsOf(md))).getOrElse(Vector.empty)
      else Rx.all(r.getRowsUpdated)   // consumed, as the SPI demands
    }
    cols
  }

  def query(sql: String, params: Vector[SqlValue])
  : Chunk[Vector[SqlValue]] ! (Produce + Async) =
    type F = Produce + Async

    def go(rows: RowStream): Chunk[Vector[SqlValue]] ! F =
      effect[F, (Vector[Vector[SqlValue]], Boolean)](Async.Run(() => rows.next(fetchSize))).flatMap {
        case (items, done) =>
          val c = ChunkBuf.of(items)
          if done then
            if c.isEmpty then okay.pure(Chunks.emptyChunk)
            else effect[F, Chunk[Vector[SqlValue]]](c)
          else effect[F, Chunk[Vector[SqlValue]]](c).flatMap(_ => go(rows))
      }

    effect[F, RowStream](Async.Run { () =>
      val st = conn.createStatement(sql).fetchSize(fetchSize)
      bindAll(st, params)
      RowStream(st)
    }).flatMap(go)

  def update(sql: String, params: Vector[SqlValue]): Long ! Async = async {
    val st = conn.createStatement(sql)
    bindAll(st, params)
    updated(st)
  }

  def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = async {
    if rows.isEmpty then 0L
    else
      val st = conn.createStatement(sql)
      var i = 0
      rows.foreach { r =>
        if i > 0 then st.add()
        bindAll(st, r)
        i += 1
      }
      updated(st)
  }

  def begin(isolation: Isolation): Granted ! Async = async {
    if inTx then throw IllegalStateException(
      "nested transaction: this connection is already in one — " +
        "refuse rather than silently flatten (specs/jdbc.md)")
    Rx.all(conn.beginTransaction())
    Rx.all(conn.setTransactionIsolationLevel(levelOf(isolation)))
    inTx = true
    Granted(isolation, isolationOf(conn.getTransactionIsolationLevel))
  }

  def commit(): Unit ! Async = async {
    Rx.all(conn.commitTransaction())
    inTx = false
  }

  def rollback(): Unit ! Async = async {
    Rx.all(conn.rollbackTransaction())
    inTx = false
  }

  /** the sync emergency brake: a no-op unless a transaction is open */
  def cancel(): Unit =
    if inTx then
      inTx = false
      Rx.all(conn.rollbackTransaction())

  def close(): Unit = Rx.all(conn.close())

  /** every Result's updated count, summed (a statement may answer
   * several results — a batch does) */
  private def updated(st: Statement): Long =
    var n = 0L
    eachResult(st)(r => n += Rx.all(r.getRowsUpdated).foldLeft(0L)(_ + _))
    n

  /** Results ONE AT A TIME, each consumed before the next is asked
   * for: the reactive drivers hand a Result out while its rows are
   * still on the wire, and the results publisher does not complete
   * until each has been drained — collecting them first hangs (found
   * against r2dbc-postgresql; H2 is eager and hides it) */
  private def eachResult(st: Statement)(f: Result => Unit): Unit =
    val pull = Rx.Pull(st.execute())
    try
      var done = false
      while !done do
        val (items, d) = pull.next(1)
        items.foreach(f)
        done = d
    finally pull.cancel()

  /** the first Result's rows as a pull, the results publisher kept
   * open underneath until the rows end; then the rest drained */
  private final class RowStream(st: Statement):
    private val outer = Rx.Pull(st.execute())
    private val inner: Option[Rx.Pull[Vector[SqlValue]]] =
      outer.next(1)._1.headOption.map(r => Rx.Pull(r.map((row: Row, md: RowMetadata) => rowOf(row, md))))
    def next(n: Int): (Vector[Vector[SqlValue]], Boolean) = inner match
      case None => close(); (Vector.empty, true)
      case Some(rows) =>
        val (items, done) = rows.next(n)
        if done then close()
        (items, done)
    def close(): Unit =
      inner.foreach(_.cancel())
      try
        var done = false
        while !done do
          val (rest, d) = outer.next(1)
          rest.foreach(r => Rx.all(r.getRowsUpdated))
          done = d
      finally outer.cancel()

object R2dbcSql:

  private def colsOf(md: RowMetadata): Vector[Col] =
    import scala.jdk.CollectionConverters.*
    md.getColumnMetadatas.asScala.toVector.map { c =>
      Col(c.getName, typeOf(c),
        c.getNullability != io.r2dbc.spi.Nullability.NON_NULL)
    }

  /** the driver's Java type → the neutral vocabulary; unknown types
   * keep their name, so verify can accept a String field for them */
  private def typeOf(c: ColumnMetadata): SqlType =
    val jt: Class[?] = if c.getJavaType == null then classOf[Object] else c.getJavaType
    if jt == classOf[java.lang.Boolean] then SqlType.Bool
    else if jt == classOf[java.lang.Integer] || jt == classOf[java.lang.Short] || jt == classOf[java.lang.Byte] then SqlType.I32
    else if jt == classOf[java.lang.Long] then SqlType.I64
    else if jt == classOf[java.lang.Double] || jt == classOf[java.lang.Float] then SqlType.F64
    else if jt == classOf[java.math.BigDecimal] || jt == classOf[java.math.BigInteger] then SqlType.Num
    else if jt == classOf[String] || jt == classOf[io.r2dbc.spi.Clob] then SqlType.Text
    else if jt == classOf[ByteBuffer] || jt == classOf[Array[Byte]] || jt == classOf[io.r2dbc.spi.Blob] then SqlType.Bytes
    else if jt.isArray then SqlType.Arr(SqlType.Other(jt.getComponentType.getSimpleName))
    else SqlType.Other(Option(c.getType).map(_.getName).getOrElse(jt.getSimpleName))

  private def rowOf(row: Row, md: RowMetadata): Vector[SqlValue] =
    val n = md.getColumnMetadatas.size
    Vector.tabulate(n)(i => valueOf(row.get(i)))

  /** a value as the driver hands it back (java boxes) */
  private def valueOf(o: AnyRef): SqlValue = o match
    case null => SqlValue.Null
    case b: java.lang.Boolean => SqlValue.Bool(b)
    case i: java.lang.Integer => SqlValue.I32(i)
    case s: java.lang.Short => SqlValue.I32(s.toInt)
    case b: java.lang.Byte => SqlValue.I32(b.toInt)
    case l: java.lang.Long => SqlValue.I64(l)
    case d: java.lang.Double => SqlValue.F64(d)
    case f: java.lang.Float => SqlValue.F64(f.toDouble)
    case d: java.math.BigDecimal => SqlValue.Num(BigDecimal(d))
    case d: java.math.BigInteger => SqlValue.Num(BigDecimal(d))
    case s: String => SqlValue.Text(s)
    case bs: Array[Byte] => SqlValue.Bytes(bs)
    case bb: ByteBuffer =>
      val bs = new Array[Byte](bb.remaining); bb.duplicate().get(bs); SqlValue.Bytes(bs)
    case arr: Array[?] => SqlValue.Arr(arr.toVector.map(x => valueOf(x.asInstanceOf[AnyRef])))
    case other => SqlValue.Text(other.toString)   // dates, uuids, json: named by describe, text on the row

  private def bindAll(st: Statement, params: Vector[SqlValue]): Unit =
    var i = 0
    while i < params.length do
      params(i) match
        // a NULL carries no type here; pg's driver takes the untyped
        // one, H2's wants a class — String is the one every engine
        // coerces
        case SqlValue.Null =>
          try st.bindNull(i, classOf[Object])
          catch case _: IllegalArgumentException => st.bindNull(i, classOf[String])
        case SqlValue.Bool(b) => st.bind(i, java.lang.Boolean.valueOf(b))
        case SqlValue.I32(x) => st.bind(i, java.lang.Integer.valueOf(x))
        case SqlValue.I64(x) => st.bind(i, java.lang.Long.valueOf(x))
        case SqlValue.F64(x) => st.bind(i, java.lang.Double.valueOf(x))
        case SqlValue.Num(x) => st.bind(i, x.bigDecimal)
        case SqlValue.Text(s) => st.bind(i, s)
        case SqlValue.Bytes(bs) => st.bind(i, ByteBuffer.wrap(bs))
        case SqlValue.Arr(elems) => st.bind(i, elems.map(javaOf).toArray)
        case SqlValue.Row(_) => throw IllegalArgumentException(
          s"param ${i + 1}: a composite parameter is not bindable through R2DBC")
      i += 1

  private def javaOf(v: SqlValue): AnyRef = v match
    case SqlValue.Null => null
    case SqlValue.Bool(b) => java.lang.Boolean.valueOf(b)
    case SqlValue.I32(x) => java.lang.Integer.valueOf(x)
    case SqlValue.I64(x) => java.lang.Long.valueOf(x)
    case SqlValue.F64(x) => java.lang.Double.valueOf(x)
    case SqlValue.Num(x) => x.bigDecimal
    case SqlValue.Text(s) => s
    case SqlValue.Bytes(bs) => ByteBuffer.wrap(bs)
    case SqlValue.Arr(elems) => elems.map(javaOf).toArray
    case SqlValue.Row(fields) => fields.map(javaOf).toArray

  private def levelOf(i: Isolation): IsolationLevel = i match
    case Isolation.ReadCommitted => IsolationLevel.READ_COMMITTED
    case Isolation.RepeatableRead => IsolationLevel.REPEATABLE_READ
    case Isolation.Serializable => IsolationLevel.SERIALIZABLE

  private def isolationOf(l: IsolationLevel): Isolation =
    if l == IsolationLevel.SERIALIZABLE then Isolation.Serializable
    else if l == IsolationLevel.REPEATABLE_READ then Isolation.RepeatableRead
    else Isolation.ReadCommitted
