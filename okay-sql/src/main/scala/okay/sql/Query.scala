package okay.sql

import okay.*
import okay.given
import okay.codec.Schema
import scala.annotation.tailrec

/**
 * A QUERY IS AN OPTIC-SHAPED DECLARATION (specs/optics-outside.md,
 * stage 9): a predicate over rows of `A`, built from the row's FIELD
 * NAMES checked against its `Schema` at construction, and read by
 * two interpreters that cannot drift from each other because they
 * read one value:
 *
 *   - DESCRIBE — `sql`: the WHERE clause with `?` placeholders and the
 *     bound parameters, columns named as `Typed` maps them
 *     (`userName` → `user_name`), so the statement is derived from
 *     the type and not typed a second time; `fields` — what it
 *     touches, no row in hand.
 *   - RUN — `test(a)`: the SAME predicate over a value in memory, the
 *     value's fields encoded by the one binding road (`Typed`), so a
 *     test needs no database and a database run needs no second
 *     predicate. SQL's three-valued logic is kept: a comparison
 *     against NULL is false, `isNull` is how NULL is asked.
 *
 * `Query.select[A](table).where(w)` renders the SELECT that
 * `Typed.rows[A]` decodes, its column list off the schema;
 * `Query.update[A](table).set(field, value).where(w)` renders the
 * UPDATE and, as `apply`, the same edit in memory through the codec.
 *
 * What the swamp was avoided by: no joins, no expressions, no
 * dialect — a field, an operator, a value, and `and`/`or`/`not`.
 * `like` is SQL's, case-sensitive; SQLite's default LIKE is
 * case-insensitive for ASCII, and `TestQuerySqlite` says so where it
 * matters.
 */
object Query:

  enum Op(val sql: String):
    case Eq extends Op("=")
    case Ne extends Op("<>")
    case Lt extends Op("<")
    case Le extends Op("<=")
    case Gt extends Op(">")
    case Ge extends Op(">=")
    case Like extends Op("LIKE")

  /** the reified predicate: what both interpreters read */
  enum Pred:
    case Cmp(field: String, col: String, idx: Int, op: Op, value: SqlValue)
    case Null(field: String, col: String, idx: Int, isNull: Boolean)
    case And(l: Pred, r: Pred)
    case Or(l: Pred, r: Pred)
    case Not(p: Pred)
    case True

  /** a predicate over rows of A */
  final class Where[A] private[sql] (val pred: Pred):
    infix def and(that: Where[A]): Where[A] = Where(Pred.And(pred, that.pred))
    infix def or(that: Where[A]): Where[A] = Where(Pred.Or(pred, that.pred))
    def unary_! : Where[A] = Where(Pred.Not(pred))

    /** DESCRIBE: the clause and its parameters, in order */
    def sql: (String, Vector[SqlValue]) = render(pred)

    /** the fields this predicate reads — no row in hand */
    def fields: Set[String] = collect(pred)

    /** RUN, in memory: the same predicate over a value, its fields
     * bound by `Typed` exactly as they would be for the database */
    def test(a: A)(using s: Schema[A]): Boolean = eval(pred, Typed.encodeParams(s, a))

  object Where:
    def all[A]: Where[A] = new Where[A](Pred.True)

  /** a field of A, by NAME, checked against the schema: the name must
   * exist and the value type must bind as the column's type */
  final class Field[A, T] private[sql] (val name: String, val col: String, val idx: Int, enc: T => SqlValue):
    private def cmp(op: Op, v: T): Where[A] = new Where[A](Pred.Cmp(name, col, idx, op, enc(v)))
    def ===(v: T): Where[A] = cmp(Op.Eq, v)
    def =!=(v: T): Where[A] = cmp(Op.Ne, v)
    def <(v: T): Where[A] = cmp(Op.Lt, v)
    def <=(v: T): Where[A] = cmp(Op.Le, v)
    def >(v: T): Where[A] = cmp(Op.Gt, v)
    def >=(v: T): Where[A] = cmp(Op.Ge, v)
    infix def like(pattern: String)(using T =:= String): Where[A] = new Where[A](Pred.Cmp(name, col, idx, Op.Like, SqlValue.Text(pattern)))
    def isNull: Where[A] = new Where[A](Pred.Null(name, col, idx, true))
    def isNotNull: Where[A] = new Where[A](Pred.Null(name, col, idx, false))

  /**
   * A field by name. Refused BY NAME when the row has no such field,
   * and BY TYPE when the value's schema binds as another column type
   * than the field's (an `Int` against a text column) — at
   * construction, where a typo belongs, not at the row it failed to
   * select.
   */
  def field[A, T](name: String)(using a: Schema[A], t: Schema[T]): Either[String, Field[A, T]] =
    for
      (idx, colType, _) <- Typed.columnOf(a, name)
      valueType <- Typed.typeOf(t)
      _ <- if colType == valueType then Right(()) else Left(s"`$name` is a $colType column; a $valueType value cannot be compared with it")
    yield new Field[A, T](name, Typed.snake(name), idx, v => Typed.encodeOne(t, v).getOrElse(SqlValue.Null))

  /** the column list a row of A reads, in declared order, as `Typed` names it */
  @tailrec def columns[A](using s: Schema[A]): Either[String, Vector[String]] = s match
    case p: Schema.SProduct[?] => Right(p.fields.map((n, _) => Typed.snake(n)))
    case Schema.SIso(u, _, _) => columns(using u())
    case _ => Left("a row is a product (a case class)")

  /** `SELECT <columns> FROM table WHERE …` — what `Typed.rows[A]` decodes */
  final class Select[A] private[sql] (table: String, cols: Vector[String]):
    def where(w: Where[A]): (String, Vector[SqlValue]) =
      val (clause, params) = w.sql
      (s"SELECT ${cols.mkString(", ")} FROM $table" + (if clause.isEmpty then "" else s" WHERE $clause"), params)
    def all: (String, Vector[SqlValue]) = where(Where.all)

  def select[A](table: String)(using s: Schema[A]): Either[String, Select[A]] =
    columns[A].map(cs => new Select[A](table, cs))

  /** `UPDATE table SET c = ?, … WHERE …`, and the same edit in memory */
  final class Update[A] private[sql] (table: String, sets: Vector[(String, String, SqlValue, okay.codec.Json)]):
    def set[T](f: Field[A, T], v: T)(using t: Schema[T]): Update[A] =
      new Update[A](table, sets :+ ((f.name, f.col, Typed.encodeOne(t, v).getOrElse(SqlValue.Null),
        okay.codec.Json.parse(okay.codec.Json.write(v)(using t)))))
    def where(w: Where[A]): (String, Vector[SqlValue]) =
      val (clause, params) = w.sql
      (s"UPDATE $table SET ${sets.map((_, c, _, _) => s"$c = ?").mkString(", ")}" +
        (if clause.isEmpty then "" else s" WHERE $clause"), sets.map(_._3) ++ params)
    /** the same edit, applied in memory through the codec: the row's
     * Json with the fields set, decoded back — `Left` names the field
     * that would not decode, which the type check makes unreachable */
    def apply(a: A)(using s: Schema[A]): Either[String, A] =
      import okay.codec.{Json, JsonOptic}
      val edited = sets.foldLeft(Json.parse(Json.write(a)(using s))) { case (j, (name, _, _, jv)) =>
        JsonOptic.at(name).set(Some(jv))(j)
      }
      okay.codec.Codecs.json(s).decode(edited)

  def update[A](table: String): Update[A] = new Update[A](table, Vector.empty)

  // ---- the two interpreters, over the reified predicate

  // Every walk below is an explicit stack (stack-safety-query): a
  // predicate built by a fold — `conds.reduce(_ and _)` — is as deep as
  // the list is long, and one frame per `and` overflowed at a few
  // thousand. TestQueryDepth runs 200 000.

  /** the subtrees that render as nothing: `True`, and an and/or of two
   * such — what `render` drops, decided before it writes a character */
  private def blanks(p: Pred): java.util.IdentityHashMap[Pred, Unit] =
    val blank = java.util.IdentityHashMap[Pred, Unit]()
    // post-order: a node is judged once both children have been
    val todo = scala.collection.mutable.Stack[(Pred, Boolean)]((p, false))
    while todo.nonEmpty do
      todo.pop() match
        case (Pred.True, _) => blank.put(Pred.True, ())
        case (n @ Pred.And(l, r), true) => if blank.containsKey(l) && blank.containsKey(r) then blank.put(n, ())
        case (n @ Pred.Or(l, r), true) => if blank.containsKey(l) && blank.containsKey(r) then blank.put(n, ())
        case (n @ Pred.And(l, r), false) => todo.push((n, true)); todo.push((r, false)); todo.push((l, false))
        case (n @ Pred.Or(l, r), false) => todo.push((n, true)); todo.push((r, false)); todo.push((l, false))
        case (Pred.Not(q), _) => todo.push((q, false))
        case _ => ()
    blank

  /** DESCRIBE, written left to right into one builder: a blank side of
   * an and/or drops out, as does its parentheses; the parameters follow
   * the `?`s in order */
  private def render(p: Pred): (String, Vector[SqlValue]) =
    val blank = blanks(p)
    val sb = StringBuilder()
    val ps = Vector.newBuilder[SqlValue]
    // a task is a predicate to write or a literal to append
    val todo = scala.collection.mutable.Stack[Pred | String](p)
    def pair(l: Pred, r: Pred, word: String): Unit =
      if blank.containsKey(l) then todo.push(r)
      else if blank.containsKey(r) then todo.push(l)
      else { todo.push(")"); todo.push(r); todo.push(s") $word ("); todo.push(l); todo.push("(") }
    while todo.nonEmpty do
      todo.pop() match
        case s: String => sb ++= s
        case Pred.True => ()
        case Pred.Cmp(_, col, _, op, v) => sb ++= s"$col ${op.sql} ?"; ps += v
        case Pred.Null(_, col, _, isNull) => sb ++= s"$col IS ${if isNull then "" else "NOT "}NULL"
        case Pred.Not(q) => todo.push(")"); todo.push(q); todo.push("NOT (")
        case Pred.And(l, r) => pair(l, r, "AND")
        case Pred.Or(l, r) => pair(l, r, "OR")
    (sb.result(), ps.result())

  private def collect(p: Pred): Set[String] =
    val out = Set.newBuilder[String]
    val todo = scala.collection.mutable.Stack[Pred](p)
    while todo.nonEmpty do
      todo.pop() match
        case Pred.True => ()
        case Pred.Cmp(f, _, _, _, _) => out += f
        case Pred.Null(f, _, _, _) => out += f
        case Pred.Not(q) => todo.push(q)
        case Pred.And(l, r) => todo.push(r); todo.push(l)
        case Pred.Or(l, r) => todo.push(r); todo.push(l)
    out.result()

  /** what is left to do once a subtree has answered: negate it, or — for
   * the left side of an and/or — either stop (the answer is decided) or
   * go on to the right side, whose answer is then the whole one */
  private enum Then:
    case Negate
    case AndThen(r: Pred)
    case OrThen(r: Pred)

  /** RUN, short-circuiting exactly as `&&`/`||`: the right side of an and
   * is not evaluated when the left is false */
  private def eval(p: Pred, row: Vector[SqlValue]): Boolean =
    val k = scala.collection.mutable.Stack[Then]()
    var cur = p
    var answer = false
    var answered = false
    var result: Option[Boolean] = None
    while result.isEmpty do
      if !answered then cur match
        case Pred.True => answer = true; answered = true
        case Pred.Null(_, _, i, isNull) => answer = (row(i) == SqlValue.Null) == isNull; answered = true
        case Pred.Cmp(_, _, i, op, v) => answer = compare(row(i), op, v); answered = true
        case Pred.Not(q) => k.push(Then.Negate); cur = q
        case Pred.And(l, r) => k.push(Then.AndThen(r)); cur = l
        case Pred.Or(l, r) => k.push(Then.OrThen(r)); cur = l
      else if k.isEmpty then result = Some(answer)
      else k.pop() match
        case Then.Negate => answer = !answer
        case Then.AndThen(r) => if answer then { cur = r; answered = false }
        case Then.OrThen(r) => if !answer then { cur = r; answered = false }
    result.get

  /** SQL's comparison: NULL on either side is unknown, which is false */
  private def compare(x: SqlValue, op: Op, y: SqlValue): Boolean = (x, y) match
    case (SqlValue.Null, _) | (_, SqlValue.Null) => false
    case (SqlValue.Text(a), SqlValue.Text(b)) => op match
      case Op.Like => likes(a, b)
      case _ => ordered(a.compareTo(b), op)
    case _ => numeric(x).zip(numeric(y)) match
      case Some((a, b)) => op match
        case Op.Like => false
        case _ => ordered(a.compare(b), op)
      case None => (op, x == y) match
        case (Op.Eq, same) => same
        case (Op.Ne, same) => !same
        case _ => false

  private def numeric(v: SqlValue): Option[BigDecimal] = v match
    case SqlValue.I32(n) => Some(BigDecimal(n))
    case SqlValue.I64(n) => Some(BigDecimal(n))
    case SqlValue.F64(n) => Some(BigDecimal(n))
    case SqlValue.Num(n) => Some(n)
    case SqlValue.Bool(b) => Some(if b then 1 else 0)
    case _ => None

  private def ordered(c: Int, op: Op): Boolean = op match
    case Op.Eq => c == 0
    case Op.Ne => c != 0
    case Op.Lt => c < 0
    case Op.Le => c <= 0
    case Op.Gt => c > 0
    case Op.Ge => c >= 0
    case Op.Like => false

  /** SQL LIKE: `%` any run, `_` one character, the rest literal */
  private def likes(s: String, pattern: String): Boolean =
    val re = pattern.flatMap {
      case '%' => ".*"
      case '_' => "."
      case c => java.util.regex.Pattern.quote(c.toString)
    }
    s.matches(re)
