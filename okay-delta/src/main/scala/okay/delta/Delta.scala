package okay.delta

import okay.{!, Async, Chunk, async}
import okay.sql.{Col, SqlType, SqlValue}
import io.delta.kernel.{Operation, Table, Transaction}
import io.delta.kernel.data.{ColumnarBatch, ColumnVector, FilteredColumnarBatch}
import io.delta.kernel.defaults.engine.DefaultEngine
import io.delta.kernel.engine.Engine
import io.delta.kernel.expressions.Literal
import io.delta.kernel.internal.InternalScanFileUtils
import io.delta.kernel.internal.data.ScanStateRow
import io.delta.kernel.types.*
import io.delta.kernel.utils.{CloseableIterable, CloseableIterator, DataFileStatus, FileStatus}
import org.apache.hadoop.conf.Configuration
import java.util.Optional
import scala.jdk.CollectionConverters.*

/**
 * Delta Lake without Spark (specs/data.md, lake-delta): the Delta
 * project's own kernel writes and scans tables from the seam's
 * SqlValue rows. The commit protocol — the optimistic log entry,
 * checkpoints, conflict resolution — is THEIRS to version; this
 * module only turns rows into the kernel's columnar batches and back.
 * A `loadId` (application id, monotonically increasing version) rides
 * the Delta transaction identifier, so a retried append lands once —
 * the bulk-load posture's dedup, in Delta's own words.
 *
 * Reads at scale are the JDBC road (DuckDB's delta extension over the
 * same files); `rows` is the kernel's own scan for the reader with no
 * engine at hand. Blocking file work runs behind `Async.Run`.
 */
object Delta:

  /** a table column in the seam's vocabulary; Arr/Row are refused
   * by name (a v1 line, not a limit of the kernel) */
  final case class Column(name: String, tpe: SqlType, nullable: Boolean = true)

  final case class Snapshot(version: Long, columns: Vector[Col])

  final case class Committed(version: Long)

  /** the kernel's engine over a plain Hadoop configuration (local
   * files, and any object store the Hadoop client knows) */
  def engine(): Engine = DefaultEngine.create(Configuration())

  /** create an EMPTY table with this schema (version 0) */
  def create(path: String, columns: Vector[Column], engine: Engine = engine()): Committed ! Async = async {
    val table = Table.forPath(engine, path)
    val txn = table.createTransactionBuilder(engine, "okay-delta", Operation.CREATE_TABLE)
      .withSchema(engine, structOf(columns))
      .build(engine)
    Committed(txn.commit(engine, CloseableIterable.emptyIterable()).getVersion)
  }

  /** append rows to an existing table (blind append); with `loadId`
   * the append is idempotent under (app, version) */
  def append(path: String, rows: Chunk[Vector[SqlValue]], loadId: Option[(String, Long)] = None,
             engine: Engine = engine()): Committed ! Async = async {
    val table = Table.forPath(engine, path)
    var builder = table.createTransactionBuilder(engine, "okay-delta", Operation.WRITE)
    loadId.foreach { case (app, v) => builder = builder.withTransactionId(engine, app, v) }
    val txn = builder.build(engine)
    val state = txn.getTransactionState(engine)
    val schema = txn.getSchema(engine)
    if txn.getPartitionColumns(engine).size > 0 then
      throw IllegalArgumentException("okay-delta appends to unpartitioned tables only (v1)")
    val batch: CloseableIterator[FilteredColumnarBatch] =
      single(FilteredColumnarBatch(RowsBatch(schema, rows.toVector), Optional.empty()))
    val partitionValues = java.util.Collections.emptyMap[String, Literal]()
    val physical = Transaction.transformLogicalData(engine, state, batch, partitionValues)
    val ctx = Transaction.getWriteContext(engine, state, partitionValues)
    try
      val files: CloseableIterator[DataFileStatus] =
        engine.getParquetHandler.writeParquetFiles(ctx.getTargetDirectory, physical, ctx.getStatisticsColumns)
      val actions = Transaction.generateAppendActions(engine, state, files, ctx)
      Committed(txn.commit(engine, CloseableIterable.inMemoryIterable(actions)).getVersion)
    catch case e: RuntimeException =>
      // the engine wraps a row's refusal, sometimes twice; the NAMED
      // one is ours to surface
      throw named(e)
  }

  /** the latest version and its schema, in the seam's vocabulary */
  def snapshot(path: String, engine: Engine = engine()): Snapshot ! Async = async {
    val snap = Table.forPath(engine, path).getLatestSnapshot(engine)
    Snapshot(snap.getVersion, colsOf(snap.getSchema))
  }

  /** the kernel's own full scan: every row of the latest snapshot */
  def rows(path: String, engine: Engine = engine()): Vector[Vector[SqlValue]] ! Async = async {
    val snap = Table.forPath(engine, path).getLatestSnapshot(engine)
    val scan = snap.getScanBuilder.build()
    val state = scan.getScanState(engine)
    val physicalSchema = ScanStateRow.getPhysicalDataReadSchema(state)
    val out = Vector.newBuilder[Vector[SqlValue]]
    val files = scan.getScanFiles(engine)
    try
      while files.hasNext do
        val fileBatch = files.next()
        val fileRows = fileBatch.getRows
        try
          while fileRows.hasNext do
            val fileRow = fileRows.next()
            val status: FileStatus = InternalScanFileUtils.getAddFileStatus(fileRow)
            val physical = engine.getParquetHandler.readParquetFiles(single(status), physicalSchema, Optional.empty())
              .map(r => r.getData)
            val logical = io.delta.kernel.Scan.transformPhysicalData(engine, state, fileRow, physical)
            try
              while logical.hasNext do
                val fb = logical.next()
                val data = fb.getData
                val sel = fb.getSelectionVector
                val n = data.getSize
                val cols = (0 until data.getSchema.length).map(data.getColumnVector)
                var r = 0
                while r < n do
                  val selected = !sel.isPresent || (!sel.get.isNullAt(r) && sel.get.getBoolean(r))
                  if selected then out += cols.map(c => valueOf(c, r)).toVector
                  r += 1
            finally logical.close()
        finally fileRows.close()
    finally files.close()
    out.result()
  }

  // ── the seam's vocabulary <-> the kernel's types ────────────────

  private def structOf(columns: Vector[Column]): StructType =
    columns.foldLeft(new StructType()) { (st, c) =>
      st.add(c.name, dataTypeOf(c.name, c.tpe), c.nullable)
    }

  private def dataTypeOf(name: String, t: SqlType): DataType = t match
    case SqlType.Bool => BooleanType.BOOLEAN
    case SqlType.I32 => IntegerType.INTEGER
    case SqlType.I64 => LongType.LONG
    case SqlType.F64 => DoubleType.DOUBLE
    case SqlType.Num => new DecimalType(38, 18)
    case SqlType.Text => StringType.STRING
    case SqlType.Bytes => BinaryType.BINARY
    case SqlType.Other(n) => throw IllegalArgumentException(s"column '$name': a vendor type '$n' has no Delta type")
    case SqlType.Arr(_) | SqlType.Row(_) =>
      throw IllegalArgumentException(s"column '$name': arrays and composites are not written by okay-delta (v1)")

  private def colsOf(st: StructType): Vector[Col] =
    st.fields.asScala.toVector.map(f => Col(f.getName, sqlTypeOf(f.getDataType), f.isNullable))

  private def sqlTypeOf(d: DataType): SqlType = d match
    case _: BooleanType => SqlType.Bool
    case _: IntegerType | _: ShortType | _: ByteType => SqlType.I32
    case _: LongType => SqlType.I64
    case _: DoubleType | _: FloatType => SqlType.F64
    case _: DecimalType => SqlType.Num
    case _: StringType => SqlType.Text
    case _: BinaryType => SqlType.Bytes
    case _: ArrayType => SqlType.Arr(SqlType.Other("array"))
    case _: StructType => SqlType.Row(Vector.empty)
    case other => SqlType.Other(other.toString)

  private def valueOf(c: ColumnVector, i: Int): SqlValue =
    if c.isNullAt(i) then SqlValue.Null
    else c.getDataType match
      case _: BooleanType => SqlValue.Bool(c.getBoolean(i))
      case _: IntegerType => SqlValue.I32(c.getInt(i))
      case _: ShortType => SqlValue.I32(c.getShort(i).toInt)
      case _: ByteType => SqlValue.I32(c.getByte(i).toInt)
      case _: LongType => SqlValue.I64(c.getLong(i))
      case _: DoubleType => SqlValue.F64(c.getDouble(i))
      case _: FloatType => SqlValue.F64(c.getFloat(i).toDouble)
      case _: DecimalType => SqlValue.Num(BigDecimal(c.getDecimal(i)))
      case _: StringType => SqlValue.Text(c.getString(i))
      case _: BinaryType => SqlValue.Bytes(c.getBinary(i))
      case other => SqlValue.Text(String.valueOf(other))

  private def single[A](a: A): CloseableIterator[A] = new CloseableIterator[A]:
    private var left = true
    def hasNext: Boolean = left
    def next(): A = { left = false; a }
    def close(): Unit = ()

  /** rows as the kernel's columnar batch — one vector per column over
   * the row vector, typed by the table schema */
  private final class RowsBatch(schema: StructType, rows: Vector[Vector[SqlValue]]) extends ColumnarBatch:
    def getSchema: StructType = schema
    def getSize: Int = rows.length
    def getColumnVector(ordinal: Int): ColumnVector =
      val field = schema.at(ordinal)
      val tpe = field.getDataType
      new ColumnVector:
        def getDataType: DataType = tpe
        def getSize: Int = rows.length
        def close(): Unit = ()
        private def at(i: Int): SqlValue = rows(i)(ordinal)
        def isNullAt(i: Int): Boolean = at(i) == SqlValue.Null
        override def getBoolean(i: Int): Boolean = at(i) match
          case SqlValue.Bool(b) => b
          case v => throw mismatch(field, v)
        override def getInt(i: Int): Int = at(i) match
          case SqlValue.I32(x) => x
          case SqlValue.I64(x) => x.toInt
          case v => throw mismatch(field, v)
        override def getLong(i: Int): Long = at(i) match
          case SqlValue.I64(x) => x
          case SqlValue.I32(x) => x.toLong
          case v => throw mismatch(field, v)
        override def getDouble(i: Int): Double = at(i) match
          case SqlValue.F64(x) => x
          case SqlValue.I32(x) => x.toDouble
          case SqlValue.I64(x) => x.toDouble
          case SqlValue.Num(x) => x.toDouble
          case v => throw mismatch(field, v)
        // the writer takes the column's scale as given: an unscaled
        // 725 at scale 18 is 7.25E-16, so the value is rescaled first
        private val scale = tpe match
          case d: DecimalType => d.getScale
          case _ => 0
        override def getDecimal(i: Int): java.math.BigDecimal = (at(i) match
          case SqlValue.Num(x) => x.bigDecimal
          case SqlValue.I32(x) => java.math.BigDecimal.valueOf(x.toLong)
          case SqlValue.I64(x) => java.math.BigDecimal.valueOf(x)
          case SqlValue.F64(x) => java.math.BigDecimal.valueOf(x)
          case v => throw mismatch(field, v)).setScale(scale, java.math.RoundingMode.HALF_UP)
        override def getString(i: Int): String = at(i) match
          case SqlValue.Text(s) => s
          case v => throw mismatch(field, v)
        override def getBinary(i: Int): Array[Byte] = at(i) match
          case SqlValue.Bytes(bs) => bs
          case v => throw mismatch(field, v)

  private def named(e: Throwable): Throwable =
    var t: Throwable = e
    while t != null && !t.isInstanceOf[IllegalArgumentException] do t = t.getCause
    if t == null then e else t

  private def mismatch(field: StructField, v: SqlValue): IllegalArgumentException =
    IllegalArgumentException(s"column '${field.getName}' is ${field.getDataType}; the row carries $v")
