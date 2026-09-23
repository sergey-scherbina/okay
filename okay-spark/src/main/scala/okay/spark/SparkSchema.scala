package okay.spark

import okay.codec.{Columns, Json, Schema}
import okay.codec.Columns.ColType
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
  def dataType(t: ColType): DataType = t match
    case ColType.Int32 => IntegerType
    case ColType.Int64 => LongType
    case ColType.Float64 => DoubleType
    case ColType.Bool => BooleanType
    case ColType.Text => StringType
    case ColType.Binary => BinaryType
    case ColType.Decimal(p, s) => DecimalType(p, s)
    case ColType.Json => VariantType
    case ColType.Arr(e, n) => ArrayType(dataType(e), n)
    case ColType.Struct(fs) => struct(fs)

  def struct(fs: Vector[Columns.Field]): StructType =
    StructType(fs.map(f => StructField(f.name, dataType(f.tpe), f.nullable)))

  /** a Columns value as Spark's external value for that type */
  def value(t: ColType, v: Any): Any = (t, v) match
    case (_, null) => null
    case (ColType.Decimal(_, _), b: BigInt) => java.math.BigDecimal(b.bigInteger)
    case (ColType.Json, j: Json) =>
      val x = org.apache.spark.types.variant.VariantBuilder.parseJson(Json.print(j), false)
      org.apache.spark.unsafe.types.VariantVal(x.getValue, x.getMetadata)
    case (ColType.Arr(e, _), xs: Vector[?]) => xs.map(value(e, _))
    case (ColType.Struct(fs), r: Columns.Row) => rowOf(fs, r)
    case (_, other) => other

  private def rowOf(fs: Vector[Columns.Field], r: Columns.Row): Row =
    Row.fromSeq(fs.iterator.zip(r.values).map((f, v) => value(f.tpe, v)).toSeq)

  /** the struct a whole value is */
  def structOf[A](using s: Schema[A]): StructType = struct(Columns.fields[A])

  /** rows for `xs`, in the shape `structOf` names */
  def rows[A](xs: Seq[A])(using s: Schema[A]): Seq[Row] =
    val (fs, toRow) = Columns.table[A]
    xs.map(a => rowOf(fs, toRow(a)))

  def dataFrame[A](spark: SparkSession, xs: Seq[A])(using s: Schema[A]): DataFrame =
    import scala.jdk.CollectionConverters.*
    spark.createDataFrame(rows(xs).asJava, structOf[A])

  /** the named nodes a schema folds as recursive (`Columns.recursiveNames`) */
  def recursiveNames(s: Schema[?]): Set[String] = Columns.recursiveNames(s)
