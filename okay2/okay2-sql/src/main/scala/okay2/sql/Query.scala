package okay2.sql

import okay2.codec.{Json, Schema}
import scala.annotation.tailrec

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
  @tailrec def columns[A](implicit s: Schema[A]): Either[String, Vector[String]] = s match {
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

  // Every walk below is an explicit stack (stack-safety-query): a
  // predicate built by a fold is as deep as the list is long, and one
  // frame per `and` overflowed at a few thousand. TestQueryDepth runs
  // 200 000 (okay-sql's Query, the same shapes).

  /** the subtrees that render as nothing: `True`, and an and/or of two such */
  private def blanks(p: Pred): java.util.IdentityHashMap[Pred, Unit] = {
    val blank = new java.util.IdentityHashMap[Pred, Unit]()
    val todo = scala.collection.mutable.Stack[(Pred, Boolean)]((p, false))
    while (todo.nonEmpty) todo.pop() match {
      case (Pred.True, _) => blank.put(Pred.True, ())
      case (n @ Pred.And(l, r), true) => if (blank.containsKey(l) && blank.containsKey(r)) blank.put(n, ())
      case (n @ Pred.Or(l, r), true) => if (blank.containsKey(l) && blank.containsKey(r)) blank.put(n, ())
      case (n @ Pred.And(l, r), false) => todo.push((n, true)); todo.push((r, false)); todo.push((l, false))
      case (n @ Pred.Or(l, r), false) => todo.push((n, true)); todo.push((r, false)); todo.push((l, false))
      case (Pred.Not(q), _) => todo.push((q, false))
      case _ => ()
    }
    blank
  }

  /** a render task: a predicate to write, or a literal to append */
  private sealed trait Task
  private final case class Write(p: Pred) extends Task
  private final case class Lit(s: String) extends Task

  private def render(p: Pred): (String, Vector[SqlValue]) = {
    val blank = blanks(p)
    val sb = new StringBuilder
    val ps = Vector.newBuilder[SqlValue]
    val todo = scala.collection.mutable.Stack[Task](Write(p))
    def pair(l: Pred, r: Pred, word: String): Unit =
      if (blank.containsKey(l)) todo.push(Write(r))
      else if (blank.containsKey(r)) todo.push(Write(l))
      else { todo.push(Lit(")")); todo.push(Write(r)); todo.push(Lit(s") $word (")); todo.push(Write(l)); todo.push(Lit("(")) }
    while (todo.nonEmpty) todo.pop() match {
      case Lit(s) => sb ++= s
      case Write(Pred.True) => ()
      case Write(Pred.Cmp(_, col, _, op, v)) => sb ++= s"$col ${op.sql} ?"; ps += v
      case Write(Pred.Null(_, col, _, isNull)) => sb ++= s"$col IS ${if (isNull) "" else "NOT "}NULL"
      case Write(Pred.Not(q)) => todo.push(Lit(")")); todo.push(Write(q)); todo.push(Lit("NOT ("))
      case Write(Pred.And(l, r)) => pair(l, r, "AND")
      case Write(Pred.Or(l, r)) => pair(l, r, "OR")
    }
    (sb.result(), ps.result())
  }

  private def collect(p: Pred): Set[String] = {
    val out = Set.newBuilder[String]
    val todo = scala.collection.mutable.Stack[Pred](p)
    while (todo.nonEmpty) todo.pop() match {
      case Pred.True => ()
      case Pred.Cmp(f, _, _, _, _) => out += f
      case Pred.Null(f, _, _, _) => out += f
      case Pred.Not(q) => todo.push(q)
      case Pred.And(l, r) => todo.push(r); todo.push(l)
      case Pred.Or(l, r) => todo.push(r); todo.push(l)
    }
    out.result()
  }

  /** what is left once a subtree has answered: negate it, or — for the
   * left side of an and/or — stop, or go on to the right side */
  private sealed trait Then
  private case object Negate extends Then
  private final case class AndThen(r: Pred) extends Then
  private final case class OrThen(r: Pred) extends Then

  /** SQL's reading: a comparison with NULL is false either way; short-
   * circuiting exactly as `&&`/`||` */
  private def eval(p: Pred, row: Vector[SqlValue]): Boolean = {
    val k = scala.collection.mutable.Stack[Then]()
    var cur = p
    var answer = false
    var answered = false
    var result: Option[Boolean] = None
    while (result.isEmpty) {
      if (!answered) cur match {
        case Pred.True => answer = true; answered = true
        case Pred.Null(_, _, i, isNull) => answer = (row(i) == SqlValue.Null) == isNull; answered = true
        case Pred.Cmp(_, _, i, op, v) => answer = compare(row(i), op, v); answered = true
        case Pred.Not(q) => k.push(Negate); cur = q
        case Pred.And(l, r) => k.push(AndThen(r)); cur = l
        case Pred.Or(l, r) => k.push(OrThen(r)); cur = l
      }
      else if (k.isEmpty) result = Some(answer)
      else k.pop() match {
        case Negate => answer = !answer
        case AndThen(r) => if (answer) { cur = r; answered = false }
        case OrThen(r) => if (!answer) { cur = r; answered = false }
      }
    }
    result.get
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
