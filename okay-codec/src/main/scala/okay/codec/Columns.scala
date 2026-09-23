package okay.codec

/**
 * A datatype as COLUMNS — the tabular reading of a `Schema[A]`, with no
 * engine in it (specs/scalus.md §4). Spark, DuckDB, Parquet, Delta, a
 * CSV writer or okay's own aggregators each translate `ColType` and the
 * plain values below into their own; the decisions a table forces on a
 * sum type or a recursive one are made ONCE, here:
 *
 * - a sum whose cases carry no fields (a pure enum) is `Text` holding
 *   the CASE NAME — never the ordinal, which a new case renumbers under
 *   every old file;
 * - a sum with payloads is a TAGGED SPARSE STRUCT: `kind` (the case
 *   name) plus one nullable struct per case that HAS fields, exactly
 *   one set. A field-less case has no branch: Parquet refuses an empty
 *   struct. A new case is a new nullable column, which old files read
 *   as null;
 * - a RECURSIVE type — a named node reachable from itself, found by a
 *   first fold over the schema graph — is `Struct(cbor: Binary, json:
 *   Json)`: okay's CBOR, lossless, and the value as JSON for engines
 *   that query into it (Spark's VARIANT, DuckDB's JSON). A struct is
 *   finite; bounded unrolling would truncate silently;
 * - a field-less product used as a value is a non-null `Bool` (true):
 *   the same empty-struct refusal, and presence is all it says;
 * - `BigInt` is `Decimal(38, 0)`; a value past 38 digits is refused
 *   with the reason, not rounded.
 *
 * VALUES are plain Scala: `Int`, `Long`, `Double`, `Boolean`, `String`,
 * `Array[Byte]`, `BigInt` (a decimal), okay's `Json` (a Json column), a
 * `Vector[Any]` (an array), a `Columns.Row` (a struct), and `null`
 * where the column is nullable and the value absent.
 */
object Columns:

  enum ColType:
    case Int32, Int64, Float64, Bool, Text, Binary
    case Decimal(precision: Int, scale: Int)
    /** a semi-structured value: Spark VARIANT, DuckDB JSON, JSON text */
    case Json
    case Arr(elem: ColType, elemNullable: Boolean)
    case Struct(fields: Vector[Field])

  final case class Field(name: String, tpe: ColType, nullable: Boolean)

  /** a struct's values, aligned with its fields */
  final case class Row(values: Vector[Any])

  /** a column: its type, whether it can be null, and how a value
   * becomes the column's plain value */
  final case class Col[A](tpe: ColType, nullable: Boolean, write: A => Any)

  /** the column for `s` */
  def column[A](s: Schema[A]): Col[A] =
    Schema.fold(s)(Algebra(recursiveNames(s)))

  /** the fields a whole value is: a product's own, anything else one
   * field named `value` */
  def fields[A](using s: Schema[A]): Vector[Field] = root(s)._1

  /** `a` as a row in the shape `fields` names */
  def row[A](a: A)(using s: Schema[A]): Row = root(s)._2(a)

  /** the fields and the row writer, computed once for many values */
  def table[A](using s: Schema[A]): (Vector[Field], A => Row) = root(s)

  private def root[A](s: Schema[A]): (Vector[Field], A => Row) =
    val c = column(s)
    c.tpe match
      case ColType.Struct(fs) if !c.nullable =>
        (fs, a => c.write(a) match
          case r: Row => r
          case other => Row(Vector(other)))
      case t => (Vector(Field("value", t, c.nullable)), a => Row(Vector(c.write(a))))

  // ---- which named nodes are recursive --------------------------------

  /**
   * The named nodes reachable from themselves. A first fold records,
   * per named node, the named nodes directly below it (through options,
   * lists and wrappers; a back edge is the fold's `ref`), and the
   * closure answers MUTUAL recursion too: in A → B → A only A's own name
   * comes back through `ref`, so a "did my name return" test inside the
   * encoder would leave B holding a placeholder (found building the
   * Spark encoder, 2026-09-23).
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

  val recursiveType: ColType = ColType.Struct(Vector(
    Field("cbor", ColType.Binary, nullable = false),
    Field("json", ColType.Json, nullable = false)))

  private def recursive[A](s: Schema[A]): Col[A] =
    Col(recursiveType, false, a => Row(Vector(Cbor.write(a)(using s), Json.parse(Json.encode(s)(a)))))

  private def isFieldless(s: Schema[?]): Boolean = s match
    case p: Schema.SProduct[?] => p.fields.isEmpty
    case _ => false

  /**
   * An edge's column applied to a value the node hands over as `Any` —
   * the one place a value is re-stated at the edge's type. It is the
   * kernel's own cast (`Schema.fold` stores field and case thunks as
   * `Schema[?]`): `parts(a)(i)` IS field i's type and a value whose
   * `caseOf` is i IS case i's type, so the cast restores what erasure
   * took and nothing more (no-casts-without-necessity).
   */
  private def writeAt[B](e: Schema.Edge[Col, B], v: Any): Any =
    e().write(v.asInstanceOf[e.X])

  private final class Algebra(recursive: Set[String]) extends Schema.Algebra[Col]:
    private def leaf[A](t: ColType, w: A => Any = (a: A) => a): Col[A] = Col(t, false, w)
    def int = leaf(ColType.Int32)
    def long = leaf(ColType.Int64)
    def double = leaf(ColType.Float64)
    def bool = leaf(ColType.Bool)
    def string = leaf(ColType.Text)
    def char = leaf(ColType.Text, (c: Char) => c.toString)
    def bytes = leaf(ColType.Binary)
    def bigInt = leaf(ColType.Decimal(38, 0), (b: BigInt) =>
      if b.abs.toString.length > 38 then
        throw IllegalArgumentException(s"integer $b has more than 38 digits, past decimal(38,0)")
      else b)

    def option[A](o: Schema.SOption[A], of: () => Col[A]) =
      val c = of()
      Col[Option[A]](c.tpe, true, {
        case Some(a) => c.write(a)
        case None => null
      })

    private def seq[A, S <: Iterable[A]](of: () => Col[A]): Col[S] =
      val c = of()
      Col[S](ColType.Arr(c.tpe, c.nullable), false, xs => xs.iterator.map(c.write).toVector)
    def list[A](l: Schema.SList[A], of: () => Col[A]) = seq[A, List[A]](of)
    def vector[A](v: Schema.SVector[A], of: () => Col[A]) = seq[A, Vector[A]](of)

    def iso[A, B](iso: Schema.SIso[A, B], under: () => Col[B]) =
      val c = under()
      Col[A](c.tpe, c.nullable, a => c.write(iso.from(a)))

    def product[A](p: Schema.SProduct[A], fields: Vector[(String, Schema.Edge[Col, Any])]) =
      if recursive(p.name) then Columns.recursive(p)
      else if p.fields.isEmpty then Col[A](ColType.Bool, false, _ => true)
      else
        val cols = fields.map((n, e) => (n, e, e()))
        Col[A](ColType.Struct(cols.map((n, _, c) => Field(n, c.tpe, c.nullable))), false, a =>
          Row(p.parts(a).iterator.zip(cols).map { case (v, (_, e, _)) => writeAt(e, v) }.toVector))

    def sum[A](su: Schema.SSum[A], cases: Vector[(String, Schema.Edge[Col, A])]) =
      if recursive(su.name) then Columns.recursive(su)
      else
        val shapes = su.cases.map((n, c) => (n, c()))
        if shapes.forall((_, c) => isFieldless(c)) then
          Col[A](ColType.Text, false, a => su.cases(su.caseOf(a))._1)
        else
          // one branch per case WITH fields; a field-less case is its `kind` alone
          val branches = cases.zip(shapes).zipWithIndex.collect {
            case (((n, e), (_, sh)), i) if !isFieldless(sh) => (i, n, e, e())
          }
          val struct = ColType.Struct(Field("kind", ColType.Text, nullable = false) +:
            branches.map((_, n, _, c) => Field(n, c.tpe, nullable = true)))
          Col[A](struct, false, a =>
            val i = su.caseOf(a)
            Row(su.cases(i)._1 +: branches.map((j, _, e, _) => if j == i then writeAt(e, a) else null)))

    def ref[A](name: String) =
      throw IllegalStateException(s"$name is recursive and was not found by recursiveNames — a defect")
