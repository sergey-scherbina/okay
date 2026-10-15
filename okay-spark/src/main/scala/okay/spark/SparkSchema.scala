package okay.spark

import okay.codec.{Columns, Json, Schema}
import okay.codec.Columns.ColType
import okay.*
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.types.*

/**
 * An okay `Schema[A]` as a Spark DataFrame: okay-codec's `Columns` —
 * where every tabular decision is made, engine-free (enums as names,
 * sums as `kind` + branches, recursion as `cbor` + json, `BigInt` as
 * `decimal(38,0)`; specs/scalus.md §4) — translated into Spark's types
 * and EXTERNAL row values. Nothing here decides anything: a type maps
 * to a type, a value to a value, and a `Json` column is a Spark 4
 * VARIANT.
 */
object SparkSchema:

  /** a Columns type as Spark's */
  def dataType(t: ColType): DataType = !.run(typeOf(t))

  def struct(fs: Vector[Columns.Field]): StructType = !.run(structAt(fs))

  /** a Columns value as Spark's external value for that type */
  def value(t: ColType, v: Any): Any = !.run(valueAt(t, v))

  /** the struct a whole value is */
  def structOf[A](using s: Schema[A]): StructType = struct(Columns.fields[A])

  /** rows for `xs`, in the shape `structOf` names */
  def rows[A](xs: Seq[A])(using s: Schema[A]): Seq[Row] =
    val (fs, toRow) = Columns.table[A]
    xs.map(a => rowOf(fs, toRow(a)))

  // THE WALKS ARE TRAMPOLINED (spark-deep-values): each descent a
  // `!.tailcall` in a program `! Pure`, run once by `!.run` — so no depth
  // is refused HERE. They used to recurse on the JVM stack and refuse a
  // type past 64 levels, Arrow's limit, which Spark does not have. Spark's
  // own limits are Spark's to name: MEASURED (ProbeSparkDepth, Spark
  // 4.2.0), a 512-level struct/array type builds and collects, and its
  // Parquet write is refused by Jackson's nesting limit on the schema's
  // JSON (1000 JSON levels, `StreamConstraintsException`, at about 450
  // alternating struct/array levels); `parse_json` stops at 1000.

  private type P[X] = X ! Pure

  /** a list of descents, in order, each deferred */
  private def each[X, Y](xs: IndexedSeq[X])(f: X => P[Y]): P[Vector[Y]] =
    def go(i: Int, acc: List[Y]): P[Vector[Y]] =
      if i == xs.length then pure(acc.reverse.toVector)
      else !.tailcall(f(xs(i))).flatMap(y => go(i + 1, y :: acc))
    go(0, Nil)

  private def typeOf(t: ColType): P[DataType] = t match
    case ColType.Int32 => pure(IntegerType)
    case ColType.Int64 => pure(LongType)
    case ColType.Float64 => pure(DoubleType)
    case ColType.Bool => pure(BooleanType)
    case ColType.Text => pure(StringType)
    case ColType.Binary => pure(BinaryType)
    case ColType.Decimal(p, s) => pure(DecimalType(p, s))
    case ColType.Json => pure(VariantType)
    case ColType.Arr(e, n) => !.tailcall(typeOf(e)).map(et => ArrayType(et, n))
    case ColType.Struct(fs) => !.tailcall(structAt(fs))

  private def structAt(fs: Vector[Columns.Field]): P[StructType] =
    each(fs)(f => typeOf(f.tpe).map(dt => StructField(f.name, dt, f.nullable))).map(StructType(_))

  private def valueAt(t: ColType, v: Any): P[Any] = (t, v) match
    case (_, null) => pure(null)
    case (ColType.Decimal(_, _), b: BigInt) => pure(java.math.BigDecimal(b.bigInteger))
    case (ColType.Json, j: Json) =>
      val x = org.apache.spark.types.variant.VariantBuilder.parseJson(Json.print(j), false)
      pure(org.apache.spark.unsafe.types.VariantVal(x.getValue, x.getMetadata))
    case (ColType.Arr(e, _), xs: Vector[?]) => each(xs)(x => valueAt(e, x))
    case (ColType.Struct(fs), r: Columns.Row) => !.tailcall(rowAt(fs, r))
    case (_, other) => pure(other)

  private def rowAt(fs: Vector[Columns.Field], r: Columns.Row): P[Row] =
    each(fs.zip(r.values))((f, v) => valueAt(f.tpe, v)).map(Row.fromSeq)

  private[spark] def rowOf(fs: Vector[Columns.Field], r: Columns.Row): Row = !.run(rowAt(fs, r))

  def dataFrame[A](spark: SparkSession, xs: Seq[A])(using s: Schema[A]): DataFrame =
    import scala.jdk.CollectionConverters.*
    spark.createDataFrame(rows(xs).asJava, structOf[A])

  /** the named nodes a schema folds as recursive (`Columns.recursiveNames`) */
  def recursiveNames(s: Schema[?]): Set[String] = Columns.recursiveNames(s)
