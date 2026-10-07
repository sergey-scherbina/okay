package okay.spark

import okay.freer.*

import okay.freer.given
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
 * STACK-SAFE AT ANY DEPTH (spark-deep-values): every walk is a program
 * `! Pure`, each descent deferred through `!.tailcall`, run once per row
 * by `!.run` — trampolined, so a VARIANT nested ten thousand levels deep
 * decodes on a small stack. No depth is refused here; Spark's own limits
 * are Spark's to name (`SparkSchema`, "THE WALKS ARE TRAMPOLINED").
 *
 * A serializable OBJECT: executors call it, and nothing here may capture
 * the session.
 */
object SparkValues extends Serializable:

  private type P[X] = X ! Pure

  private def fail(msg: String): Nothing = throw IllegalArgumentException(s"SparkValues: $msg")

  /** a field's index by name (`StructType.getFieldIndex` is Spark's own) */
  private def fieldIndex(st: StructType, name: String): Option[Int] =
    val i = st.fieldNames.indexOf(name)
    Option.when(i >= 0)(i)

  /** a whole row as `A`: a product's own fields, anything else the one
   * field `value` (the shape `Columns.fields` names) */
  def decoder[A](st: StructType)(using s: Schema[A]): SRow => A =
    val rec = Columns.recursiveNames(s)
    unwrap(s) match
      case p: Schema.SProduct[?] if !rec(p.name) && p.fields.nonEmpty =>
        row => !.run(value(s, row, st, rec))
      // a RECURSIVE root is its (cbor, json) struct itself
      case p: Schema.SProduct[?] if rec(p.name) => row => fromCbor(s, row)
      case su: Schema.SSum[?] if rec(su.name) => row => fromCbor(s, row)
      case _ =>
        val i = fieldIndex(st, "value").getOrElse(fail("a row of a non-product type has one field `value`"))
        row => !.run(value(s, row.get(i), st(i).dataType, rec))

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

  /** a scalar field: no descent */
  private def leaf[A](s: Schema[A], v: Any): A = s match
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
    case other => fail(s"$other is not a scalar")

  /** one descent, deferred: a method with its own type parameter, so a
   * schema of a captured type (a case's, a field's) can be passed */
  private def later[X](s: Schema[X], v: Any, t: DataType, rec: Set[String]): P[X] = !.tailcall(value(s, v, t, rec))
  private def laterV[X](s: Schema[X], vr: Variant, rec: Set[String]): P[X] = !.tailcall(variant(s, vr, rec))

  def value[A](s: Schema[A], v: Any, t: DataType, rec: Set[String]): P[A] = v match
    case vv: VariantVal => !.tailcall(variant(s, new Variant(vv.getValue, vv.getMetadata), rec))
    case _ => s match
      case o: Schema.SOption[x] =>
        if v == null then pure[Pure, Option[x]](None)
        else !.tailcall(value(o.of(), v, t, rec)).map(Some(_))
      case l: Schema.SList[x] => elements(l.of(), v, t, rec).map(_.toList)
      case vs: Schema.SVector[x] => elements(vs.of(), v, t, rec)
      case Schema.SIso(under, to, _) => !.tailcall(value(under(), v, t, rec)).map(b => to(b).fold(fail, identity))
      case p: Schema.SProduct[A] =>
        if rec(p.name) then pure(fromCbor(p, v))
        else if p.fields.isEmpty then pure(p.make(Nil))
        else (v, t) match
          case (r: SRow, st: StructType) => product(p, r, st, rec)
          case _ => fail(s"a ${p.name} is a struct, not $v")
      case su: Schema.SSum[A] =>
        if rec(su.name) then pure(fromCbor(su, v))
        else v match
          case kind: String => su.cases.indexWhere(_._1 == kind) match
            case -1 => fail(s"`$kind` is not a case of ${su.name}")
            case i => pure(fieldless(su.cases(i)._2()))
          case r: SRow => t match
            case st: StructType => fromBranches(su, r, st, rec)
            case other => fail(s"a ${su.name} struct has a struct type, not $other")
          case other => fail(s"a ${su.name} is a kind or a struct, not $other")
      case _ => pure(leaf(s, v))

  /** a sum with fields: `kind`, then one nullable branch per case with fields */
  private def fromBranches[A](su: Schema.SSum[A], r: SRow, st: StructType, rec: Set[String]): P[A] =
    val kind = r.getString(st.fieldIndex("kind"))
    su.cases.indexWhere(_._1 == kind) match
      case -1 => fail(s"`$kind` is not a case of ${su.name}")
      case i =>
        val (name, cs) = su.cases(i)
        if isFieldless(cs()) then pure(fieldless(cs()))
        else
          val j = st.fieldIndex(name)
          later(cs(), r.get(j), st(j).dataType, rec).map(x => x: A)

  /** a recursive type's column is `(cbor, json)`: the CBOR half is exact */
  private def fromCbor[A](s: Schema[A], v: Any): A = v match
    case r: SRow => Cbor.read[A](r.getAs[Array[Byte]](0))(using s).fold(why => fail(s"its CBOR: $why"), identity)
    case other => fail(s"a recursive value is (cbor, json), not $other")

  /** the fields in order, each descent deferred, accumulated in a list */
  private def product[A](p: Schema.SProduct[A], r: SRow, st: StructType, rec: Set[String]): P[A] =
    val fs = p.fields
    def go(j: Int, acc: List[Any]): P[A] =
      if j == fs.length then pure(p.make(acc.reverse))
      else
        val (name, f) = fs(j)
        fieldIndex(st, name) match
          case Some(i) => later(f(), r.get(i), st(i).dataType, rec).flatMap(x => go(j + 1, x :: acc))
          case None => go(j + 1, missing(p, name, f(), j) :: acc)
    go(0, Nil)

  private def missing(p: Schema.SProduct[?], name: String, fs: Schema[?], j: Int): Any =
    p.defaults.lift(j).flatten match
      case Some(d) => d()
      case None => unwrap(fs) match
        case _: Schema.SOption[?] => None
        case _ => fail(s"${p.name} has no column `$name`")

  /** one decoded element after another, each descent deferred */
  private def each[X](xs: IndexedSeq[Any], dec: Any => P[X]): P[Vector[X]] =
    def go(i: Int, acc: List[X]): P[Vector[X]] =
      if i == xs.length then pure(acc.reverse.toVector)
      else !.tailcall(dec(xs(i))).flatMap(x => go(i + 1, x :: acc))
    go(0, Nil)

  /** an array's elements, or a MAP's entries as pairs of the element type */
  private def elements[X](e: Schema[X], v: Any, t: DataType, rec: Set[String]): P[Vector[X]] = (v, t) match
    case (null, _) => pure(Vector.empty)
    case (xs: scala.collection.Seq[?], ArrayType(et, _)) => each(xs.toIndexedSeq, x => value(e, x, et, rec))
    case (m: scala.collection.Map[?, ?], MapType(kt, vt, vn)) =>
      unwrap(e) match
        case p: Schema.SProduct[?] if p.fields.length == 2 =>
          val st = StructType(Seq(StructField(p.fields(0)._1, kt, false), StructField(p.fields(1)._1, vt, vn)))
          each(m.iterator.map((k, x) => SRow(k, x): Any).toIndexedSeq, x => value(e, x, st, rec))
        case other => fail(s"a MAP column is read into a sequence of (key, value) products, not of $other")
    case (other, _) => fail(s"$other is not an array")

  // ------------------------------------------------------------ VARIANT
  private def variant[A](s: Schema[A], vr: Variant, rec: Set[String]): P[A] =
    import VariantUtil.Type
    val tp = vr.getType
    s match
      case o: Schema.SOption[x] =>
        if tp == Type.NULL then pure[Pure, Option[x]](None)
        else !.tailcall(variant(o.of(), vr, rec)).map(Some(_))
      case Schema.SIso(under, to, _) => !.tailcall(variant(under(), vr, rec)).map(b => to(b).fold(fail, identity))
      case _ if tp == Type.NULL => fail(s"a variant null is not a $s (an Option field takes it)")
      case l: Schema.SList[x] => items(l.of(), vr, tp, rec).map(_.toList)
      case vs: Schema.SVector[x] => items(vs.of(), vr, tp, rec)
      case p: Schema.SProduct[A] =>
        if p.fields.isEmpty then pure(p.make(Nil))
        else if tp != Type.OBJECT then fail(s"a ${p.name} is an object, not a variant $tp")
        else
          val fs = p.fields
          def go(j: Int, acc: List[Any]): P[A] =
            if j == fs.length then pure(p.make(acc.reverse))
            else
              val (name, f) = fs(j)
              val fv = vr.getFieldByKey(name)
              if fv == null then go(j + 1, missing(p, name, f(), j) :: acc)
              else laterV(f(), fv, rec).flatMap(x => go(j + 1, x :: acc))
          go(0, Nil)
      case su: Schema.SSum[A] =>
        val kind =
          if tp == Type.STRING then vr.getString
          else if tp == Type.OBJECT && vr.getFieldByKey("kind") != null then vr.getFieldByKey("kind").getString
          else fail(s"a ${su.name} is a kind or an object with `kind`, not a variant $tp")
        su.cases.indexWhere(_._1 == kind) match
          case -1 => fail(s"`$kind` is not a case of ${su.name}")
          case i =>
            val (name, cs) = su.cases(i)
            if isFieldless(cs()) then pure(fieldless(cs()))
            else
              val b = vr.getFieldByKey(name)
              if b == null then fail(s"a `$kind` has no `$name` branch") else laterV(cs(), b, rec).map(x => x: A)
      case _ => pure(variantLeaf(s, vr, tp))

  /** a scalar out of a variant: no descent. A `String` field takes any
   * value as its JSON text — the one place a variant is serialised, and
   * Spark's `toJson` is its own walk */
  private def variantLeaf[A](s: Schema[A], vr: Variant, tp: VariantUtil.Type): A =
    import VariantUtil.Type
    s match
      case Schema.SString => if tp == Type.STRING then vr.getString else vr.toJson(java.time.ZoneOffset.UTC)
      case Schema.SLong => number(vr, tp).longValueExact
      case Schema.SInt => number(vr, tp).intValueExact
      case Schema.SBigInt => BigInt(number(vr, tp).toBigIntegerExact)
      case Schema.SDouble => if tp == Type.DOUBLE then vr.getDouble else if tp == Type.FLOAT then vr.getFloat.toDouble else number(vr, tp).doubleValue
      case Schema.SBool => if tp == Type.BOOLEAN then vr.getBoolean else fail(s"a variant $tp is not a Boolean")
      case Schema.SChar => if tp == Type.STRING && vr.getString.length == 1 then vr.getString.charAt(0) else fail(s"a variant $tp is not a Char")
      case Schema.SBytes => if tp == Type.BINARY then vr.getBinary else fail(s"a variant $tp is not bytes")
      case other => fail(s"$other is not a scalar")

  private def number(vr: Variant, tp: VariantUtil.Type): java.math.BigDecimal =
    import VariantUtil.Type
    if tp == Type.LONG then java.math.BigDecimal.valueOf(vr.getLong)
    else if tp == Type.DECIMAL then vr.getDecimal
    else if tp == Type.DOUBLE then java.math.BigDecimal.valueOf(vr.getDouble)
    else if tp == Type.STRING then new java.math.BigDecimal(vr.getString)
    else fail(s"a variant $tp is not a number")

  private def items[X](e: Schema[X], vr: Variant, tp: VariantUtil.Type, rec: Set[String]): P[Vector[X]] =
    if tp != VariantUtil.Type.ARRAY then fail(s"a sequence is an array, not a variant $tp")
    each(IndexedSeq.tabulate(vr.arraySize)(i => vr.getElementAtIndex(i): Any), {
      case x: Variant => variant(e, x, rec)
      case other => fail(s"$other is not a variant")
    })
