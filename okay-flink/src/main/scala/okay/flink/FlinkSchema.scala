package okay.flink

import okay.codec.{Columns, Json, Schema}
import okay.codec.Columns.ColType
import org.apache.flink.api.common.typeinfo.{TypeInformation, Types}
import org.apache.flink.types.Row

/**
 * An okay `Schema[A]` as a Flink row type and rows: okay-codec's
 * `Columns` — where every tabular decision is made, engine-free (enums
 * as names, sums as `kind` + branches, recursion as `cbor` + json) —
 * translated into Flink's `TypeInformation` and `Row`s, as okay-spark's
 * `SparkSchema` translates it for Spark. Nothing here decides anything.
 * Flink has no VARIANT type, so a `Json` column is its JSON TEXT.
 */
object FlinkSchema:

  def typeInfo(t: ColType): TypeInformation[?] = t match
    case ColType.Int32 => Types.INT
    case ColType.Int64 => Types.LONG
    case ColType.Float64 => Types.DOUBLE
    case ColType.Bool => Types.BOOLEAN
    case ColType.Text => Types.STRING
    case ColType.Binary => Types.PRIMITIVE_ARRAY(Types.BYTE)
    case ColType.Decimal(_, _) => Types.BIG_DEC
    case ColType.Json => Types.STRING
    case ColType.Arr(e, _) => Types.OBJECT_ARRAY(typeInfo(e))
    case ColType.Struct(fs) => rowType(fs)

  def rowType(fs: Vector[Columns.Field]): TypeInformation[Row] =
    Types.ROW_NAMED(fs.map(_.name).toArray, fs.map(f => typeInfo(f.tpe))*)

  /** a Columns value as Flink's value for that type */
  def value(t: ColType, v: Any): Any = (t, v) match
    case (_, null) => null
    case (ColType.Decimal(_, _), b: BigInt) => java.math.BigDecimal(b.bigInteger)
    case (ColType.Json, j: Json) => Json.print(j)
    case (ColType.Arr(e, _), xs: Vector[?]) =>
      // an OBJECT_ARRAY's runtime class is its element's: Long[] for LONG, Row[] for a row
      val arr = java.lang.reflect.Array.newInstance(typeInfo(e).getTypeClass, xs.size)
      xs.iterator.zipWithIndex.foreach((x, i) => java.lang.reflect.Array.set(arr, i, value(e, x)))
      arr
    case (ColType.Struct(fs), r: Columns.Row) => rowOf(fs, r)
    case (_, other) => other

  def rowOf(fs: Vector[Columns.Field], r: Columns.Row): Row =
    Row.of(fs.iterator.zip(r.values).map((f, v) => value(f.tpe, v).asInstanceOf[AnyRef]).toSeq*)

  /** the row type a whole value is */
  def rowTypeOf[A](using s: Schema[A]): TypeInformation[Row] = rowType(Columns.fields[A])

  /** `xs` as Flink rows in the shape `rowTypeOf` names */
  def rows[A](xs: Seq[A])(using s: Schema[A]): Seq[Row] =
    val (fs, toRow) = Columns.table[A]
    xs.map(a => rowOf(fs, toRow(a)))
