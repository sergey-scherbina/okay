package okay2.sql

import okay2.codec.{Json, Schema}

/**
 * A predicate with TWO readings (okay-sql's Query.scala): the SQL clause
 * and its parameters, and the same predicate evaluated in memory over a
 * row — so an engine's answer can be checked against the in-memory one
 * (the law, TestQuerySqlite). A field is refused at construction, by name
 * and by type, naming the fault.
 */
object Query {

  sealed abstract class Op(val sql: String)
  object Op {
    case object Eq extends Op("=")
    case object Ne extends Op("<>")
    case object Lt extends Op("<")
    case object Le extends Op("<=")
    case object Gt extends Op(">")
    case object Ge extends Op(">=")
    case object Like extends Op("LIKE")
  }

  sealed trait Pred
  object Pred {
    final case class Cmp(field: String, col: String, idx: Int, op: Op, value: SqlValue) extends Pred
    final case class Null(field: String, col: String, idx: Int, isNull: Boolean) extends Pred
    final case class And(l: Pred, r: Pred) extends Pred
    final case class Or(l: Pred, r: Pred) extends Pred
    final case class Not(p: Pred) extends Pred
    case object True extends Pred
  }

  final class Where[A] private[sql] (val pred: Pred) {
    def and(that: Where[A]): Where[A] = new Where[A](Pred.And(pred, that.pred))
    def or(that: Where[A]): Where[A] = new Where[A](Pred.Or(pred, that.pred))
    def unary_! : Where[A] = new Where[A](Pred.Not(pred))

    /** the clause (empty for "all") and its parameters, in order */
    def sql: (String, Vector[SqlValue]) = render(pred)

    /** the fields it reads */
    def fields: Set[String] = collect(pred)

    /** the same predicate over a value, in memory */
    def test(a: A)(implicit s: Schema[A]): Boolean = eval(pred, Typed.encodeParams(s, a))
  }

  object Where {
    def all[A]: Where[A] = new Where[A](Pred.True)
  }

  final class Field[A, T] private[sql] (val name: String, val col: String, val idx: Int, enc: T => SqlValue) {
    private def cmp(op: Op, v: T): Where[A] = new Where[A](Pred.Cmp(name, col, idx, op, enc(v)))
    def ===(v: T): Where[A] = cmp(Op.Eq, v)
    def =!=(v: T): Where[A] = cmp(Op.Ne, v)
    def <(v: T): Where[A] = cmp(Op.Lt, v)
    def <=(v: T): Where[A] = cmp(Op.Le, v)
    def >(v: T): Where[A] = cmp(Op.Gt, v)
    def >=(v: T): Where[A] = cmp(Op.Ge, v)
    def like(pattern: String)(implicit ev: T =:= String): Where[A] =
      new Where[A](Pred.Cmp(name, col, idx, Op.Like, SqlValue.Text(pattern)))
    def isNull: Where[A] = new Where[A](Pred.Null(name, col, idx, true))
    def isNotNull: Where[A] = new Where[A](Pred.Null(name, col, idx, false))
  }

  /** the field `name` of the row A, compared as T: refused unless A has
   * it and its column type is T's */
  def field[A, T](name: String)(implicit a: Schema[A], t: Schema[T]): Either[String, Field[A, T]] =
    for {
      found <- Typed.columnOf(a, name)
      valueType <- Typed.typeOf(t)
      _ <- if (found._2 == valueType) Right(()) else Left(s"`$name` is a ${found._2} column; a $valueType value cannot be compared with it")
    } yield new Field[A, T](name, Typed.snake(name), found._1, v => Typed.encodeOne(t, v).getOrElse(SqlValue.Null))

  /** the row's columns, as Typed names them */
  def columns[A](implicit s: Schema[A]): Either[String, Vector[String]] = s match {
    case p: Schema.SProduct[_] => Right(p.fields.map(f => Typed.snake(f._1)))
    case i: Schema.SIso[_, _] => columns(i.under())
    case _ => Left("a row is a product (a case class)")
  }

  final class Select[A] private[sql] (table: String, cols: Vector[String]) {
    def where(w: Where[A]): (String, Vector[SqlValue]) = {
      val (clause, params) = w.sql
      (s"SELECT ${cols.mkString(", ")} FROM $table" + (if (clause.isEmpty) "" else s" WHERE $clause"), params)
    }
    def all: (String, Vector[SqlValue]) = where(Where.all)
  }

  def select[A](table: String)(implicit s: Schema[A]): Either[String, Select[A]] =
    columns[A].map(cs => new Select[A](table, cs))

  /** an UPDATE: its statement, and the same edit applied to a value */
  final class Update[A] private[sql] (table: String, sets: Vector[(String, String, SqlValue, Json)]) {
    def set[T](f: Field[A, T], v: T)(implicit t: Schema[T]): Update[A] =
      new Update[A](table, sets :+ ((f.name, f.col, Typed.encodeOne(t, v).getOrElse(SqlValue.Null), Json.parse(Json.write(v)))))

    /** the statement: the sets' parameters, then the clause's */
    def where(w: Where[A]): (String, Vector[SqlValue]) = {
      val (clause, params) = w.sql
      (s"UPDATE $table SET ${sets.map(s => s"${s._2} = ?").mkString(", ")}" +
        (if (clause.isEmpty) "" else s" WHERE $clause"), sets.map(_._3) ++ params)
    }

    /** the same edit in memory: each set field replaced in the value's
     * JSON (okay-codec's JsonOptic there; a top-level field here), then
     * decoded back through the row's schema */
    def apply(a: A)(implicit s: Schema[A]): Either[String, A] = {
      val edited = sets.foldLeft(Json.parse(Json.write(a))) { case (j, (name, _, _, jv)) =>
        j match {
          case Json.JObj(fs) =>
            if (fs.exists(_._1 == name)) Json.JObj(fs.map { case (k, x) => if (k == name) (k, jv) else (k, x) })
            else Json.JObj(fs :+ (name -> jv))
          case other => other
        }
      }
      Json.decode(s)(edited)
    }
  }

  def update[A](table: String): Update[A] = new Update[A](table, Vector.empty)

  private def render(p: Pred): (String, Vector[SqlValue]) = p match {
    case Pred.True => ("", Vector.empty)
    case Pred.Cmp(_, col, _, op, v) => (s"$col ${op.sql} ?", Vector(v))
    case Pred.Null(_, col, _, isNull) => (s"$col IS ${if (isNull) "" else "NOT "}NULL", Vector.empty)
    case Pred.Not(q) => val (c, ps) = render(q); (s"NOT ($c)", ps)
    case Pred.And(l, r) => both(l, r, "AND")
    case Pred.Or(l, r) => both(l, r, "OR")
  }

  private def both(l: Pred, r: Pred, word: String): (String, Vector[SqlValue]) =
    (render(l), render(r)) match {
      case (("", ps), (c, qs)) => (c, ps ++ qs)
      case ((c, ps), ("", qs)) => (c, ps ++ qs)
      case ((c, ps), (d, qs)) => (s"($c) $word ($d)", ps ++ qs)
    }

  private def collect(p: Pred): Set[String] = p match {
    case Pred.True => Set.empty
    case Pred.Cmp(f, _, _, _, _) => Set(f)
    case Pred.Null(f, _, _, _) => Set(f)
    case Pred.Not(q) => collect(q)
    case Pred.And(l, r) => collect(l) ++ collect(r)
    case Pred.Or(l, r) => collect(l) ++ collect(r)
  }

  /** SQL's reading: a comparison with NULL is false either way */
  private def eval(p: Pred, row: Vector[SqlValue]): Boolean = p match {
    case Pred.True => true
    case Pred.Null(_, _, i, isNull) => (row(i) == SqlValue.Null) == isNull
    case Pred.Cmp(_, _, i, op, v) => compare(row(i), op, v)
    case Pred.Not(q) => !eval(q, row)
    case Pred.And(l, r) => eval(l, row) && eval(r, row)
    case Pred.Or(l, r) => eval(l, row) || eval(r, row)
  }

  private def compare(x: SqlValue, op: Op, y: SqlValue): Boolean = (x, y) match {
    case (SqlValue.Null, _) | (_, SqlValue.Null) => false
    case (SqlValue.Text(a), SqlValue.Text(b)) => op match {
      case Op.Like => likes(a, b)
      case _ => ordered(a.compareTo(b), op)
    }
    case _ => numeric(x).zip(numeric(y)) match {
      case Some((a, b)) => op match {
        case Op.Like => false
        case _ => ordered(a.compare(b), op)
      }
      case None => (op, x == y) match {
        case (Op.Eq, same) => same
        case (Op.Ne, same) => !same
        case _ => false
      }
    }
  }

  private def numeric(v: SqlValue): Option[BigDecimal] = v match {
    case SqlValue.I32(n) => Some(BigDecimal(n))
    case SqlValue.I64(n) => Some(BigDecimal(n))
    case SqlValue.F64(n) => Some(BigDecimal(n))
    case SqlValue.Num(n) => Some(n)
    case SqlValue.Bool(b) => Some(if (b) BigDecimal(1) else BigDecimal(0))
    case _ => None
  }

  private def ordered(c: Int, op: Op): Boolean = op match {
    case Op.Eq => c == 0
    case Op.Ne => c != 0
    case Op.Lt => c < 0
    case Op.Le => c <= 0
    case Op.Gt => c > 0
    case Op.Ge => c >= 0
    case Op.Like => false
  }

  /** SQL LIKE: % any run, _ any one character, the rest literal */
  private def likes(s: String, pattern: String): Boolean = {
    val re = pattern.flatMap {
      case '%' => ".*"
      case '_' => "."
      case c => java.util.regex.Pattern.quote(c.toString)
    }
    s.matches(re)
  }
}
