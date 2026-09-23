package okay.spark

import okay.codec.{Cbor, Json, Schema}
import org.apache.spark.sql.{DataFrame, Row, SparkSession}
import org.apache.spark.sql.types.*

/**
 * A Spark DataFrame schema, and rows, from an okay `Schema[A]` — the
 * "Spark encoder" `Schema`'s own header promised, as one `Schema.fold`
 * algebra (specs/scalus.md §4). The part Spark cannot express is SUMS
 * and RECURSION, so those are the decisions:
 *
 * - a sum whose cases carry no fields (a pure enum) is a `string`
 *   holding the CASE NAME — never the ordinal, which a new case would
 *   renumber under every old file;
 * - a sum with payloads is a TAGGED SPARSE STRUCT: `kind` (the case
 *   name) plus one nullable struct per case that HAS fields, exactly
 *   one of them non-null. A field-less case gets no branch: Parquet
 *   refuses to write an empty `struct<>`. Adding a case adds a
 *   nullable column, which old files read as null;
 * - a RECURSIVE type — a named node reachable from itself, found by a
 *   first fold over the schema graph, never by a list of names — is
 *   `struct<cbor: binary, json: variant>`: okay's CBOR (lossless) and
 *   the same value as a Spark 4 VARIANT for queries. A StructType is
 *   finite; bounded unrolling would truncate silently;
 * - a field-less product used as a value is a non-null `boolean`
 *   (always true): the same Parquet refusal, and presence is all it
 *   says;
 * - `BigInt` is `decimal(38,0)`; a value past 38 digits is refused
 *   with its field's reason, not rounded.
 *
 * Values are Spark's EXTERNAL row values (`Row`, `Seq`, `String`,
 * `java.math.BigDecimal`, `VariantVal`), so `spark.createDataFrame`
 * takes them as they are.
 */
object SparkSchema:

  /** a column: its type, whether it can be null, and how a value
   * becomes Spark's external value for that type */
  final case class Col[A](dataType: DataType, nullable: Boolean, write: A => Any)

  /** the struct a whole value is: a product's own struct, anything else
   * wrapped as one column named `value` */
  def structOf[A](using s: Schema[A]): StructType = root(s)._1

  /** rows for `xs`, in the shape `structOf` names */
  def rows[A](xs: Seq[A])(using s: Schema[A]): Seq[Row] =
    val (_, toRow) = root(s)
    xs.map(toRow)

  def dataFrame[A](spark: SparkSession, xs: Seq[A])(using s: Schema[A]): DataFrame =
    import scala.jdk.CollectionConverters.*
    spark.createDataFrame(rows(xs).asJava, structOf[A])

  /** the column for `s`, the algebra's answer */
  def column[A](s: Schema[A]): Col[A] =
    Schema.fold(s)(Algebra(recursiveNames(s)))

  private def root[A](s: Schema[A]): (StructType, A => Row) =
    val c = column(s)
    c.dataType match
      case st: StructType if !c.nullable =>
        (st, a => c.write(a) match
          case r: Row => r
          case other => Row(other))
      case t => (StructType(Seq(StructField("value", t, c.nullable))), a => Row(c.write(a)))

  // ---- which named nodes are recursive --------------------------------

  /**
   * The named nodes reachable from themselves. A first fold records,
   * per named node, the named nodes directly below it (through
   * options, lists and wrappers); a back edge is the fold's `ref`. The
   * closure of that graph answers mutual recursion too (A → B → A makes
   * both recursive) — which a single "did my own name come back" test
   * inside the encoder would miss for B.
   */
  def recursiveNames(s: Schema[?]): Set[String] =
    val edges = scala.collection.mutable.Map.empty[String, Set[String]]
    type G[A] = Set[String]
    Schema.fold(s)(new Schema.Algebra[G]:
      def int = Set.empty; def long = Set.empty; def double = Set.empty; def bool = Set.empty
      def string = Set.empty; def char = Set.empty; def bytes = Set.empty; def bigInt = Set.empty
      def option[A](o: Schema.SOption[A], of: () => G[A]) = of()
      def list[A](l: Schema.SList[A], of: () => G[A]) = of()
      def vector[A](v: Schema.SVector[A], of: () => G[A]) = of()
      def iso[A, B](iso: Schema.SIso[A, B], under: () => G[B]) = under()
      def ref[A](name: String) = Set(name)
      def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[G, Any])]) =
        edges(p.name) = edges.getOrElse(p.name, Set.empty) ++ fields.flatMap(_._2())
        Set(p.name)
      def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[G, A])]) =
        edges(su.name) = edges.getOrElse(su.name, Set.empty) ++ cases.flatMap(_._2())
        Set(su.name)
    ): Unit
    def reach(from: String): Set[String] =
      var seen = Set.empty[String]
      var todo = edges.getOrElse(from, Set.empty).toList
      while todo.nonEmpty do
        val n = todo.head; todo = todo.tail
        if !seen(n) then { seen += n; todo = edges.getOrElse(n, Set.empty).toList ++ todo }
      seen
    edges.keySet.filter(n => reach(n)(n)).toSet

  // ---- the encoder -----------------------------------------------------

  private val recursiveType: StructType = StructType(Seq(
    StructField("cbor", BinaryType, nullable = false),
    StructField("json", VariantType, nullable = false)))

  /** a recursive node's value: okay's CBOR, and its JSON as a VARIANT */
  private def recursive[A](s: Schema[A]): Col[A] =
    Col(recursiveType, false, a =>
      val v = org.apache.spark.types.variant.VariantBuilder.parseJson(Json.encode(s)(a), false)
      Row(Cbor.write(a)(using s), org.apache.spark.unsafe.types.VariantVal(v.getValue, v.getMetadata)))

  private def isFieldless(s: Schema[?]): Boolean = s match
    case p: Schema.SProduct[?] => p.fields.isEmpty
    case _ => false

  /**
   * An edge's column applied to a value the node hands over as `Any` —
   * the one place a value is re-stated at the edge's type. It is the
   * kernel's own cast (Schema.fold's `fieldEdge`/`caseEdge` store the
   * thunk as `Schema[?]`): `parts(a)(i)` IS field i's type and a value
   * whose `caseOf` is i IS case i's type, so the cast restores what
   * erasure took and nothing more (no-casts-without-necessity).
   */
  private def writeAt[B](e: Schema.Edge[Col, B], v: Any): Any =
    e().write(v.asInstanceOf[e.X])

  private final class Algebra(recursive: Set[String]) extends Schema.Algebra[Col]:
    private def leaf[A](t: DataType, w: A => Any = (a: A) => a): Col[A] = Col(t, false, w)
    def int = leaf(IntegerType)
    def long = leaf(LongType)
    def double = leaf(DoubleType)
    def bool = leaf(BooleanType)
    def string = leaf(StringType)
    def char = leaf(StringType, (c: Char) => c.toString)
    def bytes = leaf(BinaryType)
    def bigInt = leaf(DecimalType(38, 0), (b: BigInt) =>
      if b.abs.toString.length > 38 then
        throw IllegalArgumentException(s"integer $b has more than 38 digits, past decimal(38,0)")
      else java.math.BigDecimal(b.bigInteger))

    def option[A](o: Schema.SOption[A], of: () => Col[A]) =
      val c = of()
      Col[Option[A]](c.dataType, true, {
        case Some(a) => c.write(a)
        case None => null
      })

    private def seq[A, S <: Iterable[A]](of: () => Col[A]): Col[S] =
      val c = of()
      Col[S](ArrayType(c.dataType, c.nullable), false, xs => xs.iterator.map(c.write).toSeq)
    def list[A](l: Schema.SList[A], of: () => Col[A]) = seq[A, List[A]](of)
    def vector[A](v: Schema.SVector[A], of: () => Col[A]) = seq[A, Vector[A]](of)

    def iso[A, B](iso: Schema.SIso[A, B], under: () => Col[B]) =
      val c = under()
      Col[A](c.dataType, c.nullable, a => c.write(iso.from(a)))

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Col, Any])]) =
      if recursive(p.name) then SparkSchema.recursive(p)
      else if p.fields.isEmpty then Col[A](BooleanType, false, _ => true)
      else
        val cols = fields.map((n, e) => (n, e, e()))
        Col[A](StructType(cols.map((n, _, c) => StructField(n, c.dataType, c.nullable))), false, a =>
          Row.fromSeq(p.parts(a).zip(cols).map { case (v, (_, e, _)) => writeAt(e, v) }))

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Col, A])]) =
      if recursive(su.name) then SparkSchema.recursive(su)
      else
        val shapes = su.cases.map((n, c) => (n, c()))
        if shapes.forall((_, c) => isFieldless(c)) then
          Col[A](StringType, false, a => su.cases(su.caseOf(a))._1)
        else
          // one branch per case WITH fields; a field-less case is its `kind` alone
          val branches = cases.zip(shapes).zipWithIndex.collect {
            case (((n, e), (_, sh)), i) if !isFieldless(sh) => (i, n, e, e())
          }
          val struct = StructType(StructField("kind", StringType, nullable = false) +:
            branches.map((_, n, _, c) => StructField(n, c.dataType, nullable = true)))
          Col[A](struct, false, a =>
            val i = su.caseOf(a)
            Row.fromSeq(su.cases(i)._1 +: branches.map((j, _, e, _) => if j == i then writeAt(e, a) else null)))

    def ref[A](name: String) =
      throw IllegalStateException(s"$name is recursive and was not found by recursiveNames — a defect")
