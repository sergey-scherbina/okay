package okay.sql

import okay.{HMap, Same}
import okay.codec.Schema

/**
 * A typed column TOKEN (specs/jdbc.md's typed edge, spelled for the
 * cases `Typed`'s Schema-and-case-class road does not cover): a
 * `Column[A]` names a column and knows how to encode an `A` onto it —
 * nothing about which table, which query, or which OTHER columns
 * exist, so it composes freely. Named `Column`, not `Col`: `okay.sql
 * .Col` already exists (`Sql.describe`'s own row-metadata type — a
 * driver-DESCRIBED column, not a caller-declared one). Compared by
 * IDENTITY (`Same.byIdentity`, `TRef`'s own axiom): two `Column`s of
 * the same name are still two different columns unless they are the
 * same value, which is what lets a `Row` hold one entry per `Column`
 * and never confuse two same-named tokens from two call sites.
 */
final class Column[A] private (val name: String, private[sql] val schema: Schema[A])

object Column:
  given Same[Column] = Same.byIdentity
  def apply[A](name: String)(using s: Schema[A]): Column[A] = new Column(name, s)

/**
 * A ROW WHOSE SHAPE IS ITS TYPE: `Row.empty.updated(name, "grace")
 * .updated(age, 30)` builds an `HMap[Column, T]` whose type T lists
 * exactly the columns set, in the order set — a partial UPDATE/INSERT
 * whose signature IS its column list, checked at compile time, with
 * no case class to declare and no unused-field question to answer
 * (`UPDATE users SET name = ? WHERE id = ?` binds two columns of a
 * table that may have twenty).
 *
 * This is deliberately the WRITE half only. Reading an arbitrary
 * SELECT into an `HMap` would need the row's TYPE at the call site,
 * before any value is on hand — the shape `Typed.rows[A]`'s Schema
 * already covers, by declaring the case class instead. Same instinct
 * as `TDict`/`TMap`: HMap answers "which columns does this row
 * carry" in the type, a runtime SELECT answers "what did the driver
 * hand back" — different questions, and this module already keeps
 * both roads (`Typed` for a declared case class, `Row` for a few
 * columns named on the spot).
 */
object Row:
  // `toParams`/`values` below are extension methods on `HMap`, a type
  // this object does not own, so they need `import Row.*` at the call
  // site — `Row.empty`/`HMap`'s own `.updated`/`.get` need none.

  /** the empty row: no columns yet, so nothing can be read from it
   * and only `.updated` grows its type */
  def empty: HMap[Column, EmptyTuple] = HMap.empty[Column]

  extension [T <: Tuple](row: HMap[Column, T])
    /**
     * Every (column name, encoded value) pair the row holds, in
     * insertion order — for splicing into an `UPDATE ... SET` or an
     * `INSERT` the caller writes by hand (this module never builds
     * SQL text: specs/sql.md). `Left` names the first column whose
     * value its own Schema refuses to encode (a refining `Iso`, e.g.
     * `Schema.refine`, may reject an out-of-range value).
     *
     * The one cast HMap's own contract makes safe: `updated[A](k:
     * Column[A], v: A)` is the ONLY way to grow a row, so a
     * `Column[a]` paired with `v` in the underlying tuple has `v: a`
     * at every position — the type system just cannot see it once
     * both sides are erased to `Any` by a generic walk over a plain
     * `Tuple`, which carries no per-element types of its own to
     * consult.
     */
    def toParams: Either[String, Vector[(String, SqlValue)]] =
      // every non-empty position is a (Column[a], a) pair — HMap's own
      // invariant, which a generic walk over a plain Tuple cannot see
      // (the same trusted-kernel shape as Effects.scala's own
      // `(x.resume: @unchecked) match`)
      // a loop over the tuple's spine, not a recursion per column
      // (stack-safety-sql-family); the tuple is walked newest first and
      // reversed once below
      def walk(t0: Tuple): Either[String, Vector[(String, SqlValue)]] =
        val out = Vector.newBuilder[(String, SqlValue)]
        var t = t0
        var err: String = null
        while err == null && t != EmptyTuple do
          (t: @unchecked) match
            case (col: Column[a], v) *: rest =>
              Typed.encodeOne(col.schema, v.asInstanceOf[a]) match
                case Left(e) => err = s"column ${col.name}: $e"
                case Right(sv) => out += ((col.name, sv))
              t = rest
        if err == null then Right(out.result()) else Left(err)
      // HMap's own tuple is NEWEST first (each `.updated` prepends);
      // `.reverse` here is what makes `toParams` answer in the order a
      // caller actually chained `.updated`, which is what you want in
      // an UPDATE ... SET list or an INSERT's column list.
      walk(row.toTuple).map(_.reverse)

    /** `toParams`, POSITIONAL: just the values, in the same order
     * `toParams` names them — for a caller who already wrote the
     * column names into the SQL text and needs only the bindings */
    def values: Either[String, Vector[SqlValue]] = toParams.map(_.map(_._2))
