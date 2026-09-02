package okay.sql

import okay.{!, +, Async, Chunk, Chunks, Produce, Resource, Stream, effect}
import okay.given
import okay.codec.Schema
import scala.collection.immutable.ArraySeq

/**
 * The typed layer (specs/jdbc.md, restated against the Sql seam of
 * specs/sql.md): the database's schema is authoritative — we bind
 * to it, we do not model it. SQL stays the query language; this
 * layer adds TYPED EDGES: parameters bound from a case class by
 * Schema, rows decoded into a case class by Schema, and drift
 * caught loudly at startup by `verify`, naming the column.
 *
 * A row is a product: primitives, bytes, Option for nullable — and
 * (sql-schema-composite) Vector/List for an ARRAY column, a nested
 * case class for a COMPOSITE column, recursively; a composite's
 * fields bind by POSITION (no names on the wire), the row's columns
 * by NAME. Field matches column by NAME — `userName` matches `user_name`
 * (camelCase → snake_case, case-insensitive; an exact-name match
 * also passes, the escape hatch for legacy labels) — never by
 * position, so `SELECT *` reordering cannot shear the mapping.
 * Decode is TOTAL: damage is data (`Bad`), never a throw.
 */
object Typed:

  // ── the row shape of a Schema ──────────────────────────────────

  /** what a column can hold, read off the Schema once, TYPED by the
   * value it holds (cast-free-typed): a primitive carrying its own
   * decode/encode, an Option of a shape, a wrapper (codec-iso: `to`/
   * `from` carry the conversions — to the row the wrapper does not
   * exist), an ARRAY of a shape, a nested PRODUCT (sql-schema-
   * composite) carrying its Schema for the typed field walk. Decode
   * and encode recurse over it by GADT matching; `tpe` is what verify
   * compares. */
  private enum Shape[A]:
    case Prim[A](t: SqlType, dec: SqlValue => Either[String, A], enc: A => SqlValue) extends Shape[A]
    case Opt[A](of: Shape[A]) extends Shape[Option[A]]
    case Iso[A, B](of: Shape[B], to: B => Either[String, A], from: A => B) extends Shape[A]
    case Arr[A, C](elem: Shape[A], build: Vector[A] => C, parts: C => Vector[A]) extends Shape[C]
    /** the product: field shapes by position for decoding (the Mirror's
     * `make` takes them erased), the Schema itself for encoding (its
     * `eachField` hands every value over at its own type) */
    case Row[A](fields: Vector[Shape[?]], schema: Schema.SProduct[A]) extends Shape[A]

    def tpe: SqlType = this match
      case Prim(t, _, _) => t
      case Opt(of) => of.tpe
      case Iso(of, _, _) => of.tpe
      case Arr(el, _, _) => SqlType.Arr(el.tpe)
      case Row(fs, _) => SqlType.Row(fs.map(_.tpe))

    def optional: Boolean = this match
      case Opt(_) => true
      case Iso(of, _, _) => of.optional
      case _ => false

  private object Shape:
    /** a primitive column's shape: one SqlType, and the two typed
     * directions — the widenings a column may take (I32 → I64,
     * Num → F64/Text) live in `dec` */
    def prim[A](t: SqlType, dec: PartialFunction[SqlValue, A], enc: A => SqlValue): Shape[A] =
      Prim(t, v => dec.lift(v).toRight(s"expected $t, got $v"), enc)

  private val i32 = Shape.prim[Int](SqlType.I32, { case SqlValue.I32(x) => x }, SqlValue.I32(_))
  private val i64 = Shape.prim[Long](SqlType.I64,
    { case SqlValue.I64(x) => x; case SqlValue.I32(x) => x.toLong }, SqlValue.I64(_))
  private val f64 = Shape.prim[Double](SqlType.F64,
    { case SqlValue.F64(x) => x; case SqlValue.Num(x) => x.toDouble }, SqlValue.F64(_))
  private val bool = Shape.prim[Boolean](SqlType.Bool, { case SqlValue.Bool(x) => x }, SqlValue.Bool(_))
  private val text = Shape.prim[String](SqlType.Text,
    { case SqlValue.Text(x) => x; case SqlValue.Num(x) => x.toString }, SqlValue.Text(_))
  private val bytes = Shape.prim[Array[Byte]](SqlType.Bytes, { case SqlValue.Bytes(x) => x }, SqlValue.Bytes(_))

  private final case class Field(name: String, shape: Shape[?]):
    def tpe: SqlType = shape.tpe
    def optional: Boolean = shape.optional

  private def shapeOf[A](s: Schema[A]): Either[String, Shape[A]] = s match
    case Schema.SInt => Right(i32)
    case Schema.SLong => Right(i64)
    case Schema.SDouble => Right(f64)
    case Schema.SBool => Right(bool)
    case Schema.SString => Right(text)
    case Schema.SBytes => Right(bytes)
    case o: Schema.SOption[a] => shapeOf(o.of()).map(Shape.Opt(_))
    case Schema.SIso(u, to, from) => shapeOf(u()).map(Shape.Iso(_, to, from))
    case v: Schema.SVector[a] => shapeOf(v.of()).map(Shape.Arr[a, Vector[a]](_, identity, identity))
    case l: Schema.SList[a] => shapeOf(l.of()).map(Shape.Arr[a, List[a]](_, _.toList, _.toVector))
    case p: Schema.SProduct[a] =>
      shapesOf(p).map(Shape.Row(_, p))
    case other => Left(s"not row-shaped (a row holds primitives, bytes, Option, Vector/List and nested products): $other")

  /** a product's field shapes, by position */
  private def shapesOf(p: Schema.SProduct[?]): Either[String, Vector[Shape[?]]] =
    val out = Vector.newBuilder[Shape[?]]
    var err: String = null
    for (name, thunk) <- p.fields if err == null do
      shapeOf(thunk()) match
        case Right(sh) => out += sh
        case Left(e) => err = s"field $name: $e"
    if err == null then Right(out.result()) else Left(err)

  private def fieldsOf(s: Schema[?]): Either[String, Vector[Field]] = s match
    case p: Schema.SProduct[?] =>
      shapesOf(p).left.map(e => s"field ${e}").map(shapes => p.fields.zip(shapes).map((f, sh) => Field(f._1, sh)))
    case _ => Left("a row is a product (a case class)")

  /** camelCase → snake_case, lowercased */
  def snake(name: String): String =
    val sb = new StringBuilder
    for c <- name do
      if c.isUpper then { sb += '_'; sb += c.toLower } else sb += c
    sb.result()

  private def matches(field: String, label: String): Boolean =
    val l = label.toLowerCase
    l == snake(field) || l == field.toLowerCase

  /** a column may hold a wider home than the value needs: an I32
   * column serves an I64 field; everything else is exact */
  private def fits(field: SqlType, col: SqlType): Boolean = (field, col) match
    case (SqlType.I64, SqlType.I32) => true
    // a Double reads a numeric column — lossy, by the FIELD's choice;
    // a String reads it exactly (its decimal text), and a String reads
    // ANY vendor type: the text is what the wire carries
    // (pg-scalar-types, bind-don't-model)
    case (SqlType.F64, SqlType.Num) => true
    case (SqlType.Text, SqlType.Num) => true
    case (SqlType.Text, SqlType.Other(_)) => true
    // the driver could not name the element type (JDBC metadata):
    // decode checks the elements, and decode is total
    case (SqlType.Arr(_), SqlType.Arr(SqlType.Other(_))) => true
    case (SqlType.Arr(f), SqlType.Arr(c)) => fits(f, c)
    case (SqlType.Row(fs), SqlType.Row(cs)) =>
      fs.length == cs.length && fs.zip(cs).forall(fits)
    case _ => field == col

  // ── verify: the fingerprint lesson at the database seam ────────

  /** prepare-and-compare: every mismatch between `Schema[A]` and the
   * statement's described shape, as data naming the column — run it
   * at startup, before data flows */
  def verify[A](db: Sql, sql: String)(using s: Schema[A]): Vector[Drift] ! Async =
    db.describe(sql).map { cols =>
      fieldsOf(s) match
        case Left(e) => Vector(Drift("<schema>", "a product of row-shaped fields", e))
        case Right(fs) => fs.flatMap { f =>
          cols.find(c => matches(f.name, c.label)) match
            case None =>
              Vector(Drift(snake(f.name), s"a column for field ${f.name}", "absent"))
            case Some(c) =>
              val tpe =
                if fits(f.tpe, c.tpe) then Vector.empty
                else Vector(Drift(c.label, f.tpe.toString, c.tpe.toString))
              val nul =
                if c.nullable && !f.optional then
                  Vector(Drift(c.label, "not null (the field is not Option)", "nullable"))
                else Vector.empty
              tpe ++ nul
        }
    }

  // ── rows: typed streaming decode, damage as data ───────────────

  /** total: damage is a message, never a throw — typed by the shape */
  private def decode[A](sh: Shape[A], v: SqlValue): Either[String, A] = sh match
    case o: Shape.Opt[a] => v match
      case SqlValue.Null => Right(None)
      case other => decode(o.of, other).map(Some(_))
    case Shape.Iso(of, to, _) => decode(of, v).flatMap(to)   // a refining wrapper may refuse
    case _ if v == SqlValue.Null => Left("NULL in a non-Option field")
    case Shape.Prim(_, dec, _) => dec(v)
    case arr: Shape.Arr[a, c] => v match
      case SqlValue.Arr(elems) =>
        val out = Vector.newBuilder[a]
        var err: String = null
        var i = 0
        while i < elems.length && err == null do
          decode(arr.elem, elems(i)) match
            case Right(x) => out += x
            case Left(m) => err = s"element $i: $m"
          i += 1
        if err == null then Right(arr.build(out.result())) else Left(err)
      case other => Left(s"expected ${sh.tpe}, got $other")
    case row: Shape.Row[a] => v match
      case SqlValue.Row(fields) =>
        if fields.length != row.fields.length then
          Left(s"expected a composite of ${row.fields.length} fields, got ${fields.length}")
        else
          val out = new Array[Any](row.fields.length)
          var err: String = null
          var i = 0
          while i < row.fields.length && err == null do
            decodeAny(row.fields(i), fields(i)) match
              case Right(x) => out(i) = x
              case Left(m) => err = s"field $i: $m"
            i += 1
          if err == null then Right(row.schema.make(ArraySeq.unsafeWrapArray(out))) else Left(err)
      case other => Left(s"expected ${sh.tpe}, got $other")

  /** one field at its own type; the value joins the product's erased
   * parts (the Mirror's `make` takes Any) */
  private def decodeAny[X](sh: Shape[X], v: SqlValue): Either[String, Any] = decode(sh, v)

  private def decodeCell(f: Field, label: String, v: SqlValue): Either[Bad, Any] =
    decodeAny(f.shape, v).left.map(Bad(label, _))

  /** the mirror of `decode` — for params; a product's fields are
   * walked at their own types by the Schema's `eachField` */
  private def encode[A](sh: Shape[A], v: A): SqlValue = sh match
    case Shape.Iso(of, _, from) => encode(of, from(v))
    case o: Shape.Opt[a] => v match
      case None => SqlValue.Null
      case Some(x) => encode(o.of, x)
    case Shape.Prim(_, _, enc) => enc(v)
    case arr: Shape.Arr[a, c] => SqlValue.Arr(arr.parts(v).map(encode(arr.elem, _)))
    case row: Shape.Row[a] =>
      SqlValue.Row(row.schema.eachField(v)([X] => (name: String, sc: Schema[X], x: X) =>
        shapeOf(sc) match
          case Right(sh) => encode(sh, x)
          case Left(e) => throw IllegalArgumentException(s"params: field $name: $e")))

  /** parameter encoding by Schema, positionally (used by Params) */
  private[sql] def encodeParams[P](s: Schema[P], p: P): Vector[SqlValue] = shapeOf(s) match
    case Right(row: Shape.Row[a]) =>
      row.schema.eachField(p)([X] => (name: String, sc: Schema[X], x: X) =>
        shapeOf(sc) match
          case Right(sh) => encode(sh, x)
          case Left(e) => throw IllegalArgumentException(s"params: field $name: $e"))
    case Right(_) => throw IllegalArgumentException(
      "params bind from a product (a case class of row-shaped fields)")
    case Left(e) => throw IllegalArgumentException(s"params: $e")

  /** the per-frame decoder, resolved ONCE against the described
   * columns — label matching happens here, not per row */
  private def planOf[A](s: Schema[A], cols: Vector[Col])
  : Either[Bad, Vector[SqlValue] => Either[Bad, A]] = s match
    case p: Schema.SProduct[A] =>
      val make = p.make
      fieldsOf(s) match
        case Left(e) => Left(Bad("<schema>", e))
        case Right(fs) =>
          val at = fs.map(f => (f, cols.indexWhere(c => matches(f.name, c.label))))
          at.find(_._2 < 0) match
            case Some((f, _)) => Left(Bad(snake(f.name), s"no column for field ${f.name}"))
            case None => Right { frame =>
              val out = new Array[Any](at.length)
              var bad: Bad = null
              var i = 0
              while i < at.length && bad == null do
                val (f, ix) = at(i)
                decodeCell(f, cols(ix).label, frame(ix)) match
                  case Right(v) => out(i) = v
                  case Left(b) => bad = b
                i += 1
              if bad != null then Left(bad)
              else Right(make(ArraySeq.unsafeWrapArray(out)))
            }
    case _ => Left(Bad("<schema>", "a row is a product (a case class)"))

  private type F = Produce + Async

  /** typed streaming read: the mapping resolves once via `describe`,
   * then every frame decodes to `Either[Bad, A]` — per-row damage is
   * data carrying the row position, and after a passing `verify` it
   * means the world changed mid-run: the CALLER decides */
  def rows[A](db: Sql, sql: String, params: Vector[SqlValue] = Vector.empty)
             (using s: Schema[A]): Chunk[Either[Bad, A]] ! F =
    !.widen[Vector[Col], Async, Produce](db.describe(sql)).flatMap { cols =>
      planOf(s, cols) match
        case Left(bad) =>
          // the whole statement cannot decode: one chunk of the answer
          effect[F, Chunk[Either[Bad, A]]](ArraySeq(Left(bad)))
        case Right(dec) =>
          val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
          def go(p: Chunk[Vector[SqlValue]] ! F, row: Long): Chunk[Either[Bad, A]] ! F =
            !.widen[Option[(Chunk[Vector[SqlValue]], Chunk[Vector[SqlValue]] ! F)], Async, Produce](
              S.uncons(p)).flatMap {
              case None => okay.pure(Chunks.emptyChunk)
              case Some((c, rest)) =>
                var r = row
                val decoded: Chunk[Either[Bad, A]] = c.map { frame =>
                  val out = dec(frame).left.map(b => b.copy(row = r))
                  r += 1
                  out
                }
                effect[F, Chunk[Either[Bad, A]]](decoded).flatMap(_ => go(rest, r))
            }
          go(db.query(sql, params), 0L)
    }

  /** typed read with params bound from a product */
  def rowsOf[A, P](db: Sql, sql: String)(p: P)
                  (using Schema[A], Schema[P]): Chunk[Either[Bad, A]] ! F =
    rows[A](db, sql, Params.bind(p))

  // ── writes: params from the product, always prepared ───────────

  def update[P](db: Sql, sql: String)(p: P)(using Schema[P]): Long ! Async =
    db.update(sql, Params.bind(p))

  def batchOf[P](db: Sql, sql: String)(rows: Chunk[P])(using Schema[P]): Long ! Async =
    db.batch(sql, rows.map(Params.bind(_)))

  // ── the transaction region ─────────────────────────────────────

  /** the region over the driver's begin/commit/rollback: COMMIT on
   * normal completion; on an exception or a handled abort crossing
   * the scope the commit step never runs and the Resource finalizer
   * pulls `db.cancel()` — the sync emergency brake (specs/sql.md).
   * After a commit the brake is a no-op. `G` is whatever else the
   * body performs (Throws for abortable bodies; pass `Async` when
   * nothing extra). A transact program is one-shot. */
  def transact[A, G[+_]](db: Sql, isolation: Isolation = Isolation.ReadCommitted)
                        (body: Granted => A ! (Resource + Async + G))
  : A ! (Resource + Async + G) =
    for
      g <- !.widen[Granted, Async, Resource + G](db.begin(isolation))
      _ <- !.widen[Unit, Resource, Async + G](Resource.acquire(())(_ => db.cancel()))
      a <- body(g)
      _ <- !.widen[Unit, Async, Resource + G](db.commit())
    yield a

  // ── the TYPED region: the protocol in the types ────────────────

  /** the two transaction states, as phantoms. This is PState's
   * typestate (State.scala, Atkey's parameterised monad — the theory
   * textbook, ch. 3) in its two-state degenerate form: entering the
   * region is the No -> Yes transition, leaving it Yes -> No, and the
   * type system enforces the protocol order — a nested `region` does
   * not COMPILE, where `transact` refuses it at runtime. The full
   * PState embedding (threading the state type through Cont's answer
   * type) was considered and declined for v1: it buys the same
   * guarantee at the price of a Free<->Cont bridge on every step. */
  object Tx:
    sealed trait No
    sealed trait Yes

  /** a driver handle carrying its transaction state in the type.
   * Queries run in any state; `region` demands `No` and hands the
   * body `Yes` — and there is no begin/commit on the handle at all,
   * the region IS them. */
  final class Db[S] private[sql] (private[sql] val db: Sql):
    def describe(sql: String): Vector[Col] ! Async = db.describe(sql)
    def query(sql: String, params: Vector[SqlValue] = Vector.empty)
    : Chunk[Vector[SqlValue]] ! (Produce + Async) = db.query(sql, params)
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async =
      db.update(sql, params)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async =
      db.batch(sql, rows)

  object Db:
    /** every driver starts outside a transaction */
    def apply(db: Sql): Db[Tx.No] = new Db[Tx.No](db)

  /** `transact`, with the protocol lifted into the types: the body
   * sees a `Db[Tx.Yes]`, on which `region` cannot be called again —
   * the nested-begin failure specs/jdbc.md documents as a runtime
   * refusal is unrepresentable here. Runtime behavior is EXACTLY
   * `transact` (commit on completion, the cancel brake on abort). */
  def region[A, G[+_]](db: Db[Tx.No], isolation: Isolation = Isolation.ReadCommitted)
                      (body: Db[Tx.Yes] => A ! (Resource + Async + G))
  : A ! (Resource + Async + G) =
    transact(db.db, isolation)(_ => body(new Db[Tx.Yes](db.db)))

/** parameter binding: positionally from a product's declared field
 * order, through the driver's prepared path — there is no API that
 * interpolates a value into SQL text, so injection is
 * unrepresentable, not discouraged */
object Params:

  def bind[P](p: P)(using s: Schema[P]): Vector[SqlValue] = Typed.encodeParams(s, p)

/** the exact decimal field (pg-scalar-types): on the row it is the
 * numeric's text (verify passes Text against Num; decode renders a
 * Num exactly), and in JSON/CBOR it travels as a string — no float on
 * either road. `import okay.sql.given` */
given decimalSchema: Schema[BigDecimal] = Schema.refine[BigDecimal, String](
  s => try Right(BigDecimal(s)) catch { case _: NumberFormatException => Left(s"not a decimal: '$s'") },
  _.toString)
