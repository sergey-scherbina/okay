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
 * A row is a FLAT product: primitives, bytes, Option for nullable.
 * Field matches column by NAME — `userName` matches `user_name`
 * (camelCase → snake_case, case-insensitive; an exact-name match
 * also passes, the escape hatch for legacy labels) — never by
 * position, so `SELECT *` reordering cannot shear the mapping.
 * Decode is TOTAL: damage is data (`Bad`), never a throw.
 */
object Typed:

  // ── the row shape of a Schema ──────────────────────────────────

  /** `into`/`outof` carry a wrapper's conversions (codec-iso): a
   * wrapped column decodes through `into` after its primitive and
   * encodes through `outof` before it — to the row the wrapper does
   * not exist */
  private final case class Field(name: String, tpe: SqlType, optional: Boolean,
                                 into: Any => Either[String, Any] = Right(_),
                                 outof: Any => Any = identity)

  private def shapeOf(s: Schema[?]): Option[(SqlType, Boolean, Any => Either[String, Any], Any => Any)] = s match
    case Schema.SInt => Some((SqlType.I32, false, Right(_), identity))
    case Schema.SLong => Some((SqlType.I64, false, Right(_), identity))
    case Schema.SDouble => Some((SqlType.F64, false, Right(_), identity))
    case Schema.SBool => Some((SqlType.Bool, false, Right(_), identity))
    case Schema.SString => Some((SqlType.Text, false, Right(_), identity))
    case Schema.SBytes => Some((SqlType.Bytes, false, Right(_), identity))
    case Schema.SOption(of) => shapeOf(of()).map((t, _, in, out) => (t, true, in, out))
    case Schema.SIso(u, to, from) => shapeOf(u()).map { (t, opt, in, out) =>
      (t, opt,
        (v: Any) => in(v).flatMap(x => to.asInstanceOf[Any => Either[String, Any]](x)),
        (v: Any) => out(from.asInstanceOf[Any => Any](v)))
    }
    case _ => None

  private def fieldsOf(s: Schema[?]): Either[String, Vector[Field]] = s match
    case Schema.SProduct(_, fields, _, _, _) =>
      val out = Vector.newBuilder[Field]
      var err: String = null
      for (name, thunk) <- fields if err == null do
        shapeOf(thunk()) match
          case Some((t, opt, in, outof)) => out += Field(name, t, opt, in, outof)
          case None => err = s"field $name is not row-shaped (a row holds primitives, bytes and Option)"
      if err == null then Right(out.result()) else Left(err)
    case _ => Left("a row is a flat product (a case class)")

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
  private def fits(field: SqlType, col: SqlType): Boolean =
    field == col || (field == SqlType.I64 && col == SqlType.I32)

  // ── verify: the fingerprint lesson at the database seam ────────

  /** prepare-and-compare: every mismatch between `Schema[A]` and the
   * statement's described shape, as data naming the column — run it
   * at startup, before data flows */
  def verify[A](db: Sql, sql: String)(using s: Schema[A]): Vector[Drift] ! Async =
    db.describe(sql).map { cols =>
      fieldsOf(s) match
        case Left(e) => Vector(Drift("<schema>", "a flat product of row-shaped fields", e))
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

  private def decodeCell(f: Field, label: String, v: SqlValue): Either[Bad, Any] = v match
    case SqlValue.Null =>
      if f.optional then Right(None)
      else Left(Bad(label, "NULL in a non-Option field"))
    case other =>
      val prim: Option[Any] = (f.tpe, other) match
        case (SqlType.Bool, SqlValue.Bool(x)) => Some(x)
        case (SqlType.I32, SqlValue.I32(x)) => Some(x)
        case (SqlType.I64, SqlValue.I64(x)) => Some(x)
        case (SqlType.I64, SqlValue.I32(x)) => Some(x.toLong)
        case (SqlType.F64, SqlValue.F64(x)) => Some(x)
        case (SqlType.Text, SqlValue.Text(x)) => Some(x)
        case (SqlType.Bytes, SqlValue.Bytes(x)) => Some(x)
        case _ => None
      prim match
        case Some(x) => f.into(x) match
          case Right(y) => Right(if f.optional then Some(y) else y)
          case Left(m) => Left(Bad(label, m))   // a refining wrapper refused
        case None => Left(Bad(label, s"expected ${f.tpe}, got $other"))

  /** the per-frame decoder, resolved ONCE against the described
   * columns — label matching happens here, not per row */
  private def planOf[A](s: Schema[A], cols: Vector[Col])
  : Either[Bad, Vector[SqlValue] => Either[Bad, A]] = s match
    case Schema.SProduct(_, _, make, _, _) =>
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
    case _ => Left(Bad("<schema>", "a row is a flat product"))

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

/** parameter binding: positionally from a product's declared field
 * order, through the driver's prepared path — there is no API that
 * interpolates a value into SQL text, so injection is
 * unrepresentable, not discouraged */
object Params:

  def bind[P](p: P)(using s: Schema[P]): Vector[SqlValue] = s match
    case Schema.SProduct(_, fields, _, parts, _) =>
      parts(p).toVector.zip(fields).map((v, f) => encode(f._1, f._2(), v))
    case _ => throw IllegalArgumentException(
      "params bind from a flat product (a case class of row-shaped fields)")

  private def encode(name: String, s: Schema[?], v: Any): SqlValue = s match
    case Schema.SIso(u, _, from) => encode(name, u(), from.asInstanceOf[Any => Any](v))
    case Schema.SInt => SqlValue.I32(v.asInstanceOf[Int])
    case Schema.SLong => SqlValue.I64(v.asInstanceOf[Long])
    case Schema.SDouble => SqlValue.F64(v.asInstanceOf[Double])
    case Schema.SBool => SqlValue.Bool(v.asInstanceOf[Boolean])
    case Schema.SString => SqlValue.Text(v.asInstanceOf[String])
    case Schema.SBytes => SqlValue.Bytes(v.asInstanceOf[Array[Byte]])
    case Schema.SOption(of) => v.asInstanceOf[Option[Any]] match
      case None => SqlValue.Null
      case Some(x) => encode(name, of(), x)
    case _ => throw IllegalArgumentException(s"param $name is not row-shaped")
