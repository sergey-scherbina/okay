package okay.spark

import okay.codec.{Cbor, Columns, Schema}
import org.apache.spark.sql.Row as SRow
import org.apache.spark.sql.types.*
import org.apache.spark.types.variant.{Variant, VariantUtil}
import org.apache.spark.unsafe.types.VariantVal

/**
 * SPARK VALUES AS `A`, EXACTLY (spark-values-exact): the decoder
 * `SparkFrames` reads rows with, driven by `Schema[A]` straight over
 * Spark's own values — no `Json` in between, so a `Long` above 2^53 and a
 * `BigInt` in `decimal(38,0)` arrive as they were written. It reads what
 * `Columns` (the encoder) writes, and three things a DataFrame from
 * elsewhere carries:
 *
 * - a RECURSIVE type's `(cbor, json)` column is read from its CBOR half,
 *   `Cbor.read(Schema)`, which is lossless;
 * - a VARIANT value is read TYPED through Spark's `Variant`: an object
 *   into a product by key, an array into a sequence, a long or a decimal
 *   into the field's own number, and any value into a `String` field as
 *   its JSON text;
 * - a MAP value is read as the list of its entries, each entry decoded
 *   against the field's element type, which must be a product of two
 *   fields (key, value) — a `List`/`Vector` of pairs, or a `Map` behind an
 *   `SIso` over one.
 *
 * Every walk goes one frame per level of the value and refuses past
 * `SparkSchema.MaxNesting`; the depth travels as a parameter.
 *
 * A serializable OBJECT: executors call it, and nothing here may capture
 * the session.
 */
object SparkValues extends Serializable:

  private def fail(msg: String): Nothing = throw IllegalArgumentException(s"SparkValues: $msg")

  /** a field's index by name (`StructType.getFieldIndex` is Spark's own) */
  private def fieldIndex(st: StructType, name: String): Option[Int] =
    val i = st.fieldNames.indexOf(name)
    Option.when(i >= 0)(i)

  private def deeper(depth: Int): Int =
    if depth >= SparkSchema.MaxNesting then fail(s"a value nested deeper than ${SparkSchema.MaxNesting} is refused")
    depth + 1

  /** a whole row as `A`: a product's own fields, anything else the one
   * field `value` (the shape `Columns.fields` names) */
  def decoder[A](st: StructType)(using s: Schema[A]): SRow => A =
    val rec = Columns.recursiveNames(s)
    unwrap(s) match
      case p: Schema.SProduct[?] if !rec(p.name) && p.fields.nonEmpty =>
        row => value(s, row, st, rec, 0)
      // a RECURSIVE root is its (cbor, json) struct itself
      case p: Schema.SProduct[?] if rec(p.name) => row => fromCbor(s, row)
      case su: Schema.SSum[?] if rec(su.name) => row => fromCbor(s, row)
      case _ =>
        val i = fieldIndex(st, "value").getOrElse(fail("a row of a non-product type has one field `value`"))
        row => value(s, row.get(i), st(i).dataType, rec, 0)

  /** past the isos: the shape a value is stored in */
  private def unwrap(s: Schema[?]): Schema[?] = s match
    case Schema.SIso(under, _, _) => unwrap(under())
    case other => other

  private def fieldless[X](s: Schema[X]): X = s match
    case p: Schema.SProduct[X] if p.fields.isEmpty => p.make(Nil)
    case Schema.SIso(under, to, _) => to(fieldless(under())).fold(fail, identity)
    case _ => fail(s"$s is not a field-less case")

  private def isFieldless(s: Schema[?]): Boolean = unwrap(s) match
    case p: Schema.SProduct[?] => p.fields.isEmpty
    case _ => false

  def value[A](s: Schema[A], v: Any, t: DataType, rec: Set[String], depth: Int): A = v match
    case vv: VariantVal => variant(s, new Variant(vv.getValue, vv.getMetadata), rec, deeper(depth))
    case _ => s match
      case Schema.SInt => v match
        case n: Int => n
        case n: Long => Math.toIntExact(n)
        case n: Short => n.toInt
        case n: Byte => n.toInt
        case d: java.math.BigDecimal => d.intValueExact
        case x: String => x.toInt
        case other => fail(s"$other is not an Int")
      case Schema.SLong => v match
        case n: Long => n
        case n: Int => n.toLong
        case n: Short => n.toLong
        case d: java.math.BigDecimal => d.longValueExact
        case x: String => x.toLong
        case other => fail(s"$other is not a Long")
      case Schema.SDouble => v match
        case n: Double => n
        case n: Float => n.toDouble
        case n: Long => n.toDouble
        case n: Int => n.toDouble
        case d: java.math.BigDecimal => d.doubleValue
        case x: String => x.toDouble
        case other => fail(s"$other is not a Double")
      case Schema.SBool => v match
        case b: Boolean => b
        case x: String => x.toBoolean
        case other => fail(s"$other is not a Boolean")
      case Schema.SString => v match
        case x: String => x
        case null => fail("a null is not a String (an Option[String] field takes it)")
        case other => other.toString
      case Schema.SChar => v match
        case x: String if x.length == 1 => x.charAt(0)
        case other => fail(s"$other is not a Char")
      case Schema.SBytes => v match
        case b: Array[Byte] => b
        case other => fail(s"$other is not bytes")
      case Schema.SBigInt => v match
        case d: java.math.BigDecimal => BigInt(d.toBigIntegerExact)
        case n: Long => BigInt(n)
        case n: Int => BigInt(n)
        case x: String => BigInt(x)
        case other => fail(s"$other is not an integer")
      case o: Schema.SOption[x] =>
        if v == null then None else Some(value(o.of(), v, t, rec, depth))
      case l: Schema.SList[x] => elements(l.of(), v, t, rec, deeper(depth)).toList
      case vs: Schema.SVector[x] => elements(vs.of(), v, t, rec, deeper(depth))
      case Schema.SIso(under, to, _) => to(value(under(), v, t, rec, depth)).fold(fail, identity)
      case p: Schema.SProduct[A] =>
        if rec(p.name) then fromCbor(p, v)
        else if p.fields.isEmpty then p.make(Nil)
        else (v, t) match
          case (r: SRow, st: StructType) => product(p, r, st, rec, deeper(depth))
          case _ => fail(s"a ${p.name} is a struct, not $v")
      case su: Schema.SSum[A] =>
        if rec(su.name) then fromCbor(su, v)
        else v match
          case kind: String => su.cases.indexWhere(_._1 == kind) match
            case -1 => fail(s"`$kind` is not a case of ${su.name}")
            case i => fieldless(su.cases(i)._2())
          case r: SRow => t match
            case st: StructType => fromBranches(su, r, st, rec, depth)
            case other => fail(s"a ${su.name} struct has a struct type, not $other")
          case other => fail(s"a ${su.name} is a kind or a struct, not $other")

  /** a sum with fields: `kind`, then one nullable branch per case with fields */
  private def fromBranches[A](su: Schema.SSum[A], r: SRow, st: StructType, rec: Set[String], depth: Int): A =
            val kind = r.getString(st.fieldIndex("kind"))
            su.cases.indexWhere(_._1 == kind) match
              case -1 => fail(s"`$kind` is not a case of ${su.name}")
              case i =>
                val (name, cs) = su.cases(i)
                if isFieldless(cs()) then fieldless(cs())
                else
                  val j = st.fieldIndex(name)
                  value(cs(), r.get(j), st(j).dataType, rec, deeper(depth))

  /** a recursive type's column is `(cbor, json)`: the CBOR half is exact */
  private def fromCbor[A](s: Schema[A], v: Any): A = v match
    case r: SRow => Cbor.read[A](r.getAs[Array[Byte]](0))(using s).fold(why => fail(s"its CBOR: $why"), identity)
    case other => fail(s"a recursive value is (cbor, json), not $other")

  private def product[A](p: Schema.SProduct[A], r: SRow, st: StructType, rec: Set[String], depth: Int): A =
    val vals = p.fields.zipWithIndex.map { case ((name, fs), j) =>
      fieldIndex(st, name) match
        case Some(i) => value(fs(), r.get(i), st(i).dataType, rec, depth): Any
        case None => missing(p, name, fs(), j)
    }
    p.make(vals)

  private def missing(p: Schema.SProduct[?], name: String, fs: Schema[?], j: Int): Any =
    p.defaults.lift(j).flatten match
      case Some(d) => d()
      case None => unwrap(fs) match
        case _: Schema.SOption[?] => None
        case _ => fail(s"${p.name} has no column `$name`")

  /** an array's elements, or a MAP's entries as pairs of the element type */
  private def elements[X](e: Schema[X], v: Any, t: DataType, rec: Set[String], depth: Int): Vector[X] = (v, t) match
    case (null, _) => Vector.empty
    case (xs: scala.collection.Seq[?], ArrayType(et, _)) => xs.iterator.map(x => value(e, x, et, rec, depth)).toVector
    case (m: scala.collection.Map[?, ?], MapType(kt, vt, vn)) =>
      unwrap(e) match
        case p: Schema.SProduct[?] if p.fields.length == 2 =>
          val st = StructType(Seq(StructField(p.fields(0)._1, kt, false), StructField(p.fields(1)._1, vt, vn)))
          m.iterator.map((k, x) => value(e, SRow(k, x), st, rec, depth)).toVector
        case other => fail(s"a MAP column is read into a sequence of (key, value) products, not of $other")
    case (other, _) => fail(s"$other is not an array")

  // ------------------------------------------------------------ VARIANT
  private def variant[A](s: Schema[A], vr: Variant, rec: Set[String], depth: Int): A =
    import VariantUtil.Type
    val tp = vr.getType
    s match
      case o: Schema.SOption[x] => if tp == Type.NULL then None else Some(variant(o.of(), vr, rec, depth))
      case Schema.SIso(under, to, _) => to(variant(under(), vr, rec, depth)).fold(fail, identity)
      case _ if tp == Type.NULL => fail(s"a variant null is not a $s (an Option field takes it)")
      case Schema.SString => if tp == Type.STRING then vr.getString else vr.toJson(java.time.ZoneOffset.UTC)
      case Schema.SLong => number(vr, tp).longValueExact
      case Schema.SInt => number(vr, tp).intValueExact
      case Schema.SBigInt => BigInt(number(vr, tp).toBigIntegerExact)
      case Schema.SDouble => if tp == Type.DOUBLE then vr.getDouble else if tp == Type.FLOAT then vr.getFloat.toDouble else number(vr, tp).doubleValue
      case Schema.SBool => if tp == Type.BOOLEAN then vr.getBoolean else fail(s"a variant $tp is not a Boolean")
      case Schema.SChar => if tp == Type.STRING && vr.getString.length == 1 then vr.getString.charAt(0) else fail(s"a variant $tp is not a Char")
      case Schema.SBytes => if tp == Type.BINARY then vr.getBinary else fail(s"a variant $tp is not bytes")
      case l: Schema.SList[x] => items(l.of(), vr, tp, rec, depth).toList
      case vs: Schema.SVector[x] => items(vs.of(), vr, tp, rec, depth)
      case p: Schema.SProduct[A] =>
        if p.fields.isEmpty then p.make(Nil)
        else if tp != Type.OBJECT then fail(s"a ${p.name} is an object, not a variant $tp")
        else p.make(p.fields.zipWithIndex.map { case ((name, fs), j) =>
          val f = vr.getFieldByKey(name)
          if f == null then missing(p, name, fs(), j) else variant(fs(), f, rec, deeper(depth)): Any
        })
      case su: Schema.SSum[A] =>
        val kind =
          if tp == Type.STRING then vr.getString
          else if tp == Type.OBJECT && vr.getFieldByKey("kind") != null then vr.getFieldByKey("kind").getString
          else fail(s"a ${su.name} is a kind or an object with `kind`, not a variant $tp")
        su.cases.indexWhere(_._1 == kind) match
          case -1 => fail(s"`$kind` is not a case of ${su.name}")
          case i =>
            val (name, cs) = su.cases(i)
            if isFieldless(cs()) then fieldless(cs())
            else
              val b = vr.getFieldByKey(name)
              if b == null then fail(s"a `$kind` has no `$name` branch") else variant(cs(), b, rec, deeper(depth))

  private def number(vr: Variant, tp: VariantUtil.Type): java.math.BigDecimal =
    import VariantUtil.Type
    if tp == Type.LONG then java.math.BigDecimal.valueOf(vr.getLong)
    else if tp == Type.DECIMAL then vr.getDecimal
    else if tp == Type.DOUBLE then java.math.BigDecimal.valueOf(vr.getDouble)
    else if tp == Type.STRING then new java.math.BigDecimal(vr.getString)
    else fail(s"a variant $tp is not a number")

  private def items[X](e: Schema[X], vr: Variant, tp: VariantUtil.Type, rec: Set[String], depth: Int): Vector[X] =
    if tp != VariantUtil.Type.ARRAY then fail(s"a sequence is an array, not a variant $tp")
    Vector.tabulate(vr.arraySize)(i => variant(e, vr.getElementAtIndex(i), rec, deeper(depth)))
