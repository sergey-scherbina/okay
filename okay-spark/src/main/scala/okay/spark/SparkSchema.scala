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

  /** how deep a type may nest: Arrow's own limit, the one okay-arrow keeps
   * (`Column.MaxNesting`, C++ `kMaxNestingDepth`) */
  val MaxNesting: Int = 64

  /** a Columns type as Spark's */
  def dataType(t: ColType): DataType = typeOf(checked(t))

  def struct(fs: Vector[Columns.Field]): StructType =
    val _ = checked(ColType.Struct(fs))
    structAt(fs)

  /** a Columns value as Spark's external value for that type */
  def value(t: ColType, v: Any): Any = valueAt(checked(t), v)

  /** the struct a whole value is */
  def structOf[A](using s: Schema[A]): StructType = struct(Columns.fields[A])

  /** rows for `xs`, in the shape `structOf` names */
  def rows[A](xs: Seq[A])(using s: Schema[A]): Seq[Row] =
    val (fs, toRow) = Columns.table[A]
    val _ = checked(ColType.Struct(fs))
    xs.map(a => rowOf(fs, toRow(a)))

  // The walks below recurse once per level of a type, and `value`'s in
  // step with the value. Every door checks the type first, so they go at
  // most MaxNesting deep (stack-safety-catch-up-okay2).

  private def typeOf(t: ColType): DataType = t match
    case ColType.Int32 => IntegerType
    case ColType.Int64 => LongType
    case ColType.Float64 => DoubleType
    case ColType.Bool => BooleanType
    case ColType.Text => StringType
    case ColType.Binary => BinaryType
    case ColType.Decimal(p, s) => DecimalType(p, s)
    case ColType.Json => VariantType
    case ColType.Arr(e, n) => ArrayType(typeOf(e), n)
    case ColType.Struct(fs) => structAt(fs)

  private def structAt(fs: Vector[Columns.Field]): StructType =
    StructType(fs.map(f => StructField(f.name, typeOf(f.tpe), f.nullable)))

  private def valueAt(t: ColType, v: Any): Any = (t, v) match
    case (_, null) => null
    case (ColType.Decimal(_, _), b: BigInt) => java.math.BigDecimal(b.bigInteger)
    case (ColType.Json, j: Json) =>
      val x = org.apache.spark.types.variant.VariantBuilder.parseJson(Json.print(j), false)
      org.apache.spark.unsafe.types.VariantVal(x.getValue, x.getMetadata)
    case (ColType.Arr(e, _), xs: Vector[?]) => xs.map(valueAt(e, _))
    case (ColType.Struct(fs), r: Columns.Row) => rowOf(fs, r)
    case (_, other) => other

  private def rowOf(fs: Vector[Columns.Field], r: Columns.Row): Row =
    Row.fromSeq(fs.iterator.zip(r.values).map((f, v) => valueAt(f.tpe, v)).toSeq)

  /** `t`, refused when it nests past MaxNesting. The levels are counted
   * on an explicit stack, which stops one level past the limit */
  private def checked(t: ColType): ColType =
    val todo = java.util.ArrayDeque[(ColType, Int)]()
    todo.push((t, 1))
    while !todo.isEmpty do
      val (x, d) = todo.pop()
      if d > MaxNesting then
        throw IllegalArgumentException(s"a type nested deeper than $MaxNesting levels (Arrow's own limit)")
      x match
        case ColType.Arr(e, _) => todo.push((e, d + 1))
        case ColType.Struct(fs) => fs.foreach(f => todo.push((f.tpe, d + 1)))
        case _ => ()
    t

  def dataFrame[A](spark: SparkSession, xs: Seq[A])(using s: Schema[A]): DataFrame =
    import scala.jdk.CollectionConverters.*
    spark.createDataFrame(rows(xs).asJava, structOf[A])

  /** the named nodes a schema folds as recursive (`Columns.recursiveNames`) */
  def recursiveNames(s: Schema[?]): Set[String] = Columns.recursiveNames(s)
