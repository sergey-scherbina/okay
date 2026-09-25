package okay2.sql

import scala.collection.immutable.ArraySeq
import okay2.{!, +, Resource, Writer, pure}
import okay2.async.{Async, Scheduler, Timer, asyncFailing}
import okay2.codec.Schema
import okay2.stream.{Chunk, Source}

/**
 * The typed layer over the seam (okay-sql's Typed.scala, specs/sql.md):
 * a case class IS a row, read by column LABEL, written positionally, and
 * checked against the engine's own description of a statement.
 *
 * Everything here reads the row's `Schema`; nothing is generated per
 * type. Totality: a cell that does not decode is a `Left(Bad)` naming
 * the column and the row, never a throw.
 */
object Typed {

  /**
   * How one field travels. Scala 3 matches an enum with GADT
   * refinement; here each case carries its own `decode`/`encode`, which
   * is the same dispatch typed by the case itself.
   */
  private[sql] sealed abstract class Shape[A] {
    def tpe: SqlType
    def optional: Boolean = false
    /** the value, a NULL answered by the case (only Option admits one) */
    def decode(v: SqlValue): Either[String, A]
    def encode(a: A): SqlValue
  }

  private[sql] object Shape {
    final class Prim[A](val tpe: SqlType, dec: SqlValue => Either[String, A], enc: A => SqlValue) extends Shape[A] {
      def decode(v: SqlValue): Either[String, A] =
        if (v == SqlValue.Null) Left("NULL in a non-Option field") else dec(v)
      def encode(a: A): SqlValue = enc(a)
    }

    final class Opt[A](of: Shape[A]) extends Shape[Option[A]] {
      def tpe: SqlType = of.tpe
      override def optional: Boolean = true
      def decode(v: SqlValue): Either[String, Option[A]] =
        if (v == SqlValue.Null) Right(None) else of.decode(v).map(Some(_))
      def encode(a: Option[A]): SqlValue = a.fold[SqlValue](SqlValue.Null)(of.encode)
    }

    /** a wrapper is its underlying kind, both ways; a refining one may
     * refuse on the way in */
    final class Iso[A, B](of: Shape[B], to: B => Either[String, A], from: A => B) extends Shape[A] {
      def tpe: SqlType = of.tpe
      override def optional: Boolean = of.optional
      def decode(v: SqlValue): Either[String, A] = of.decode(v).flatMap(to)
      def encode(a: A): SqlValue = of.encode(from(a))
    }

    final class Arr[A, C](elem: Shape[A], build: Vector[A] => C, parts: C => Vector[A]) extends Shape[C] {
      def tpe: SqlType = SqlType.Arr(elem.tpe)
      def decode(v: SqlValue): Either[String, C] = v match {
        case SqlValue.Null => Left("NULL in a non-Option field")
        case SqlValue.Arr(elems) =>
          val out = Vector.newBuilder[A]
          var err: String = null
          var i = 0
          while (i < elems.length && err == null) {
            elem.decode(elems(i)) match {
              case Right(x) => out += x
              case Left(m) => err = s"element $i: $m"
            }
            i += 1
          }
          if (err == null) Right(build(out.result())) else Left(err)
        case other => Left(s"expected $tpe, got $other")
      }
      def encode(c: C): SqlValue = SqlValue.Arr(parts(c).map(elem.encode))
    }

    /** a nested product: a composite value, field by field */
    final class Row[A](val fields: Vector[Shape[_]], val schema: Schema.SProduct[A]) extends Shape[A] {
      def tpe: SqlType = SqlType.Row(fields.map(_.tpe))
      def decode(v: SqlValue): Either[String, A] = v match {
        case SqlValue.Null => Left("NULL in a non-Option field")
        case SqlValue.Row(vs) =>
          if (vs.length != fields.length) Left(s"expected a composite of ${fields.length} fields, got ${vs.length}")
          else {
            val out = new Array[Any](fields.length)
            var err: String = null
            var i = 0
            while (i < fields.length && err == null) {
              fields(i).decode(vs(i)) match {
                case Right(x) => out(i) = x
                case Left(m) => err = s"field $i: $m"
              }
              i += 1
            }
            if (err == null) Right(schema.make(ArraySeq.unsafeWrapArray(out))) else Left(err)
          }
        case other => Left(s"expected $tpe, got $other")
      }
      def encode(a: A): SqlValue = SqlValue.Row(encodeFields(schema, a))
    }

    def prim[A](t: SqlType, dec: PartialFunction[SqlValue, A], enc: A => SqlValue): Shape[A] =
      new Prim[A](t, v => dec.lift(v).toRight(s"expected $t, got $v"), enc)
  }

  /** a product's fields, each encoded by its own schema — typed
   * together by `eachField`, so no part is cast */
  private def encodeFields[A](p: Schema.SProduct[A], a: A): Vector[SqlValue] =
    p.eachField(a)(new Schema.FieldFn[SqlValue] {
      def apply[X](name: String, sc: Schema[X], x: X): SqlValue = shapeOf(sc) match {
        case Right(sh) => sh.encode(x)
        case Left(e) => throw new IllegalArgumentException(s"params: field $name: $e")
      }
    })

  private val i32 = Shape.prim[Int](SqlType.I32, { case SqlValue.I32(x) => x }, SqlValue.I32(_))
  private val i64 = Shape.prim[Long](SqlType.I64,
    { case SqlValue.I64(x) => x; case SqlValue.I32(x) => x.toLong }, SqlValue.I64(_))
  private val f64 = Shape.prim[Double](SqlType.F64,
    { case SqlValue.F64(x) => x; case SqlValue.Num(x) => x.toDouble }, SqlValue.F64(_))
  private val bool = Shape.prim[Boolean](SqlType.Bool, { case SqlValue.Bool(x) => x }, SqlValue.Bool(_))
  /** a String reads any vendor type as its text */
  private val text = Shape.prim[String](SqlType.Text, {
    case SqlValue.Text(x) => x
    case SqlValue.Num(x) => x.toString
    case SqlValue.Timestamp(us) => Temporal.renderTimestamp(us)
    case SqlValue.Date(d) => Temporal.renderDate(d)
    case SqlValue.Time(us) => Temporal.renderTime(us)
    case SqlValue.Uuid(u) => u.toString
    case SqlValue.Json(j) => j
  }, SqlValue.Text(_))
  /** exact from a whole Num or an integer column; binds as its digits */
  private val bigInt = Shape.prim[BigInt](SqlType.Num, {
    case SqlValue.Num(x) if x.isWhole => x.toBigInt
    case SqlValue.I64(x) => BigInt(x)
    case SqlValue.I32(x) => BigInt(x)
    case SqlValue.Text(x) if x.nonEmpty && x.stripPrefix("-").nonEmpty && x.stripPrefix("-").forall(_.isDigit) => BigInt(x)
  }, x => SqlValue.Text(x.toString))
  private val uuid = Shape.prim[java.util.UUID](SqlType.Uuid, { case SqlValue.Uuid(u) => u }, SqlValue.Uuid(_))
  private val bytes = Shape.prim[Array[Byte]](SqlType.Bytes, { case SqlValue.Bytes(x) => x }, SqlValue.Bytes(_))

  /** a schema instance with a shape of its own, found by IDENTITY */
  private[sql] final class Known[X](val schema: Schema[X], val shape: Shape[X])
  private[sql] object Known {
    // a heterogeneous table keyed by the schema's identity: an entry
    // whose schema IS `s` holds a Shape at that very type
    def find[A](table: Vector[Known[_]], s: Schema[A]): Option[Shape[A]] =
      table.find(_.schema eq s).map(_.shape.asInstanceOf[Shape[A]])
  }
  private val known: Vector[Known[_]] =
    new Known(uuidSchema, uuid) +: javatime.known

  private final case class Field(name: String, shape: Shape[_]) {
    def tpe: SqlType = shape.tpe
    def optional: Boolean = shape.optional
  }

  private type Found[X] = Either[String, Shape[X]]

  private[sql] def shapeOf[A](s: Schema[A]): Either[String, Shape[A]] = Known.find(known, s) match {
    case Some(sh) => Right(sh)
    case None => s.visit(new Schema.Visit[Found] {
      private def refuse[X]: Found[X] =
        Left(s"not row-shaped (a row holds primitives, bytes, Option, Vector/List and nested products): $s")
      def int = Right(i32)
      def long = Right(i64)
      def double = Right(f64)
      def bool = Right(Typed.bool)
      def string = Right(text)
      def char = refuse
      def bytes = Right(Typed.bytes)
      def bigInt = Right(Typed.bigInt)
      def option[B](o: Schema.SOption[B]) = shapeOf(o.of()).map(new Shape.Opt(_))
      def list[B](l: Schema.SList[B]) = shapeOf(l.of()).map(new Shape.Arr[B, List[B]](_, _.toList, _.toVector))
      def vector[B](v: Schema.SVector[B]) = shapeOf(v.of()).map(new Shape.Arr[B, Vector[B]](_, identity, identity))
      def product[B](p: Schema.SProduct[B]) = shapesOf(p).map(new Shape.Row(_, p))
      def sum[B](su: Schema.SSum[B]) = refuse
      def iso[B, C](i: Schema.SIso[B, C]) = shapeOf(i.under()).map(new Shape.Iso(_, i.to, i.from))
    })
  }

  private def shapesOf(p: Schema.SProduct[_]): Either[String, Vector[Shape[_]]] = {
    val out = Vector.newBuilder[Shape[_]]
    var err: String = null
    val it = p.fields.iterator
    while (err == null && it.hasNext) {
      val (name, thunk) = it.next()
      shapeOf(thunk()) match {
        case Right(sh) => out += sh
        case Left(e) => err = s"field $name: $e"
      }
    }
    if (err == null) Right(out.result()) else Left(err)
  }

  private def fieldsOf(s: Schema[_]): Either[String, Vector[Field]] = s match {
    case p: Schema.SProduct[_] =>
      shapesOf(p).left.map(e => s"field $e").map(shapes => p.fields.zip(shapes).map { case (f, sh) => Field(f._1, sh) })
    case _ => Left("a row is a product (a case class)")
  }

  private[sql] def columnOf(s: Schema[_], name: String): Either[String, (Int, SqlType, Boolean)] =
    fieldsOf(s).flatMap { fs =>
      val i = fs.indexWhere(_.name == name)
      if (i < 0) Left(s"`$name` is not a field of this row (fields: ${fs.map(_.name).mkString(", ")})")
      else Right((i, fs(i).tpe, fs(i).optional))
    }

  private[sql] def typeOf(s: Schema[_]): Either[String, SqlType] = shapeOf(s).map(_.tpe)

  /** camelCase -> snake_case: the column a field reads */
  def snake(name: String): String = {
    val sb = new StringBuilder
    for (c <- name) if (c.isUpper) { sb += '_'; sb += c.toLower } else sb += c
    sb.result()
  }

  private def matches(field: String, label: String): Boolean = {
    val l = label.toLowerCase
    l == snake(field) || l == field.toLowerCase
  }

  /** a field type that reads a column type */
  private def fits(field: SqlType, col: SqlType): Boolean = (field, col) match {
    case (SqlType.I64, SqlType.I32) => true
    case (SqlType.F64, SqlType.Num) => true
    case (SqlType.Text, SqlType.Num) => true
    case (SqlType.Num, SqlType.I64 | SqlType.I32) => true
    case (SqlType.Text, SqlType.Other(_)) => true
    case (SqlType.Text, SqlType.Timestamp | SqlType.Date | SqlType.Time | SqlType.Uuid | SqlType.Json) => true
    case (SqlType.Arr(_), SqlType.Arr(SqlType.Other(_))) => true
    case (SqlType.Arr(f), SqlType.Arr(c)) => fits(f, c)
    case (SqlType.Row(fs), SqlType.Row(cs)) => fs.length == cs.length && fs.zip(cs).forall { case (a, b) => fits(a, b) }
    case _ => field == col
  }

  /** the row's fields against the engine's description of `sql`: each
   * missing, retyped or wrongly nullable column, named */
  def verify[A](db: Sql, sql: String)(implicit s: Schema[A]): Vector[Drift] ! Async =
    db.describe(sql).map { cols =>
      fieldsOf(s) match {
        case Left(e) => Vector(Drift("<schema>", "a product of row-shaped fields", e))
        case Right(fs) => fs.flatMap { f =>
          cols.find(c => matches(f.name, c.label)) match {
            case None => Vector(Drift(snake(f.name), s"a column for field ${f.name}", "absent"))
            case Some(c) =>
              val tpe = if (fits(f.tpe, c.tpe)) Vector.empty else Vector(Drift(c.label, f.tpe.toString, c.tpe.toString))
              val nul =
                if (c.nullable && !f.optional) Vector(Drift(c.label, "not null (the field is not Option)", "nullable"))
                else Vector.empty
              tpe ++ nul
          }
        }
      }
    }

  private[sql] def encodeOne[A](s: Schema[A], v: A): Either[String, SqlValue] = shapeOf(s).map(_.encode(v))

  /** a product's fields as positional parameters */
  private[sql] def encodeParams[P](s: Schema[P], p: P): Vector[SqlValue] = s match {
    case prod: Schema.SProduct[P] => shapesOf(prod) match {
      case Right(_) => encodeFields(prod, p)
      case Left(e) => throw new IllegalArgumentException(s"params: $e")
    }
    case _ => shapeOf(s) match {
      case Left(e) => throw new IllegalArgumentException(s"params: $e")
      case Right(_) => throw new IllegalArgumentException("params bind from a product (a case class of row-shaped fields)")
    }
  }

  /** the decoder for a frame of `cols`, planned once per statement */
  private def planOf[A](s: Schema[A], cols: Vector[Col]): Either[Bad, Vector[SqlValue] => Either[Bad, A]] = s match {
    case p: Schema.SProduct[A] =>
      fieldsOf(p) match {
        case Left(e) => Left(Bad("<schema>", e))
        case Right(fs) =>
          val at = fs.map(f => (f, cols.indexWhere(c => matches(f.name, c.label))))
          at.find(_._2 < 0) match {
            case Some((f, _)) => Left(Bad(snake(f.name), s"no column for field ${f.name}"))
            case None => Right { frame =>
              val out = new Array[Any](at.length)
              var bad: Bad = null
              var i = 0
              while (i < at.length && bad == null) {
                val (f, ix) = at(i)
                f.shape.decode(frame(ix)) match {
                  case Right(v) => out(i) = v
                  case Left(e) => bad = Bad(cols(ix).label, e)
                }
                i += 1
              }
              if (bad != null) Left(bad) else Right(p.make(ArraySeq.unsafeWrapArray(out)))
            }
          }
      }
    case _ => Left(Bad("<schema>", "a row is a product (a case class)"))
  }

  /** the rows of `sql` as `A`, chunk for chunk as the driver reads them;
   * a row that does not decode is a `Left(Bad)` with its row number */
  def rows[A](db: Sql, sql: String, params: Vector[SqlValue] = Vector.empty)
             (implicit s: Schema[A]): Source[Chunk[Either[Bad, A]]] =
    db.describe(sql).flatMap { cols =>
      planOf(s, cols) match {
        case Left(bad) => Writer.tell[Chunk[Either[Bad, A]]](ArraySeq(Left(bad)))
        case Right(dec) =>
          // per run: this closure runs each time the Source does
          var row = 0L
          Writer.mapAt[Chunk[Vector[SqlValue]], Chunk[Either[Bad, A]], Unit, Async](db.query(sql, params)) { c =>
            c.map { frame =>
              val out = dec(frame).left.map(_.copy(row = row))
              row += 1
              out
            }
          }
      }
    }

  /** `rows` with the parameters bound from a product */
  def rowsOf[A, P](db: Sql, sql: String)(p: P)(implicit a: Schema[A], ps: Schema[P]): Source[Chunk[Either[Bad, A]]] =
    rows[A](db, sql, Params.bind(p))

  def update[P](db: Sql, sql: String)(p: P)(implicit s: Schema[P]): Long ! Async =
    db.update(sql, Params.bind(p))

  def batchOf[P](db: Sql, sql: String)(rows: Chunk[P])(implicit s: Schema[P]): Long ! Async =
    db.batch(sql, rows.map(Params.bind(_)))

  /**
   * A transaction region: begin, the brake (`cancel`) registered as the
   * scope's finalizer, the body, commit. A failure anywhere — the body,
   * a statement — leaves the brake to roll back when the scope closes.
   */
  def transact[A, G <: okay2.Row](db: Sql, isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false)
                                 (body: Granted => A ! (Resource + Async + G)): A ! (Resource + Async + G) =
    db.begin(isolation, readOnly).flatMap { g =>
      Resource.acquire(())(_ => db.cancel()).flatMap { _ =>
        body(g).flatMap(a => db.commit().map(_ => a))
      }
    }

  /** how many times a region may run, and the pause before run n+1 */
  final case class Retry(attempts: Int, backoffMillis: Int => Long = _ => 0L) {
    require(attempts >= 1, "a region runs at least once")
  }
  object Retry {
    val none: Retry = Retry(1)
  }

  final case class Retried[A](value: A, attempts: Int)

  /** `transact`, re-run on a serialization failure or a deadlock (the
   * failure's SQLSTATE, `Sql.retryable`); anything else propagates */
  def transactRetry[A](db: Sql, isolation: Isolation = Isolation.ReadCommitted,
                       retry: Retry = Retry.none, readOnly: Boolean = false)
                      (body: Granted => A ! (Resource + Async))
                      (implicit S: Scheduler, T: Timer): Retried[A] ! Async = {
    def once: A ! Async = Resource.run[A, Async](transact[A, Async](db, isolation, readOnly)(body))
    def go(n: Int): Retried[A] ! Async =
      Async.attempt(once).flatMap {
        case Right(a) => pure[Async, Retried[A]](Retried(a, n))
        case Left(t) if n < retry.attempts && db.sqlState(t).exists(Sql.retryable) =>
          Async.sleep(retry.backoffMillis(n)).flatMap(_ => go(n + 1))
        case Left(t) => throw t
      }
    go(1)
  }

  /** the typed region's two states: a `Db[Tx.No]` may begin, a
   * `Db[Tx.Yes]` (inside a region) cannot — a nested begin does not
   * compile */
  object Tx {
    sealed trait No
    sealed trait Yes
  }

  final class Db[S] private[sql] (private[sql] val db: Sql) {
    def describe(sql: String): Vector[Col] ! Async = db.describe(sql)
    def query(sql: String, params: Vector[SqlValue] = Vector.empty): Source[Chunk[Vector[SqlValue]]] = db.query(sql, params)
    def update(sql: String, params: Vector[SqlValue] = Vector.empty): Long ! Async = db.update(sql, params)
    def batch(sql: String, rows: Chunk[Vector[SqlValue]]): Long ! Async = db.batch(sql, rows)
  }

  object Db {
    def apply(db: Sql): Db[Tx.No] = new Db[Tx.No](db)
  }

  def region[A, G <: okay2.Row](db: Db[Tx.No], isolation: Isolation = Isolation.ReadCommitted, readOnly: Boolean = false)
                               (body: Db[Tx.Yes] => A ! (Resource + Async + G)): A ! (Resource + Async + G) =
    transact[A, G](db.db, isolation, readOnly)(_ => body(new Db[Tx.Yes](db.db)))
}

/** a product's fields as positional statement parameters */
object Params {
  def bind[P](p: P)(implicit s: Schema[P]): Vector[SqlValue] = Typed.encodeParams(s, p)
}
