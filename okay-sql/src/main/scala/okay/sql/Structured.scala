package okay.sql

import okay.{!, Effect, Tables, Within, effect}
import okay.codec.Schema
import okay.Tables.{Table, select, join, where}

/**
 * THE STRUCTURAL OPERATORS (specs/streams-seam.md, lane 5): a filter and
 * a join a platform can SEE, beside the opaque `where(p)` and `join` of
 * `Tables`. A predicate is a `Query.Where[A]` — field names checked
 * against `Schema[A]` at construction, the same value okay-sql renders as
 * SQL and runs in memory — and a join key is a `Query.Field` on each
 * side, so both are data, not closures.
 *
 * A signature in the row beside `Tables`, `Sort` and `Streamed`, by the
 * extension rule of specs/bulk.md. `viaTables` answers it on ANY
 * platform through the opaque primitives (the predicate run by its own
 * `test`, the key read off the value's encoded fields), so a program in
 * `Tables + Structured` runs on `localBulk` and `FlowBulk` unchanged;
 * okay-spark's `SparkFrames` answers it natively, keeping a
 * DataFrame-born table inside Catalyst.
 *
 * The names are `matching` and `joinOn`, not `where` and `join`: both
 * objects' extensions are imported together, and a second `where` would
 * make every call ambiguous rather than overloaded.
 */
enum Structured[+A] derives Effect:
  case Matching[A](t: Table[A], w: Query.Where[A], s: Schema[A]) extends Structured[Table[A]]
  case JoinOn[A, B, K](l: Table[A], r: Table[B], lf: Query.Field[A, K], rf: Query.Field[B, K],
                       sa: Schema[A], sb: Schema[B]) extends Structured[Table[(A, B)]]

object Structured:

  // ------------------------------------------- on a handle: ! Structured
  extension [A](t: Table[A])
    inline def matching(w: Query.Where[A])(using s: Schema[A]): Table[A] ! Structured =
      effect(Matching(t, w, s))
    inline def joinOn[B, K](r: Table[B])(lf: Query.Field[A, K], rf: Query.Field[B, K])
                           (using sa: Schema[A], sb: Schema[B]): Table[(A, B)] ! Structured =
      effect(JoinOn(t, r, lf, rf, sa, sb))

  // ------------------------- on a program: any row that has Structured
  extension [A, F[_]](p: Table[A] ! F)(using Within[Structured, F])
    def matching(w: Query.Where[A])(using Schema[A]): Table[A] ! F =
      p.flatMap(t => t.matching(w).at[F])
    def joinOn[B, K](r: Table[B] ! F)(lf: Query.Field[A, K], rf: Query.Field[B, K])
                    (using Schema[A], Schema[B]): Table[(A, B)] ! F =
      p.flatMap(t => r.flatMap(rt => t.joinOn(rt)(lf, rf).at[F]))

  /** the key a field names, as the value's own encoding of it: two equal
   * `K`s encode to equal `SqlValue`s, which is what a hash join needs.
   * A `Bytes` key compares by identity and is refused where it is asked */
  private[okay] def key[A](s: Schema[A], idx: Int)(a: A): SqlValue =
    Typed.encodeParams(s, a)(idx) match
      case SqlValue.Bytes(_) => throw IllegalArgumentException("joinOn: a binary field is not a join key")
      case v => v

  /** the default: through the opaque primitives, so it runs on any platform — into the `Tables` the program has
   * already (one place, as okay-freer's union made it) */
  def viaTables[A, Es[_]](p: A ! Es)(using s: okay.core.Split[Es, Structured], w: Within[Tables, s.Rest]): A ! s.Rest =
    p.translateInto[Structured, Tables]:
      [X] => (e: Structured[X]) => e match
        case Matching(t, w, s) =>
          t.where(a => w.test(a)(using s)).map(t => t: X)
        case JoinOn(l, r, lf, rf, sa, sb) =>
          val kl = key(sa, lf.idx)
          val kr = key(sb, rf.idx)
          l.select(a => kl(a) -> a).flatMap(lk => r.select(b => kr(b) -> b).flatMap(rk =>
            lk.join(rk).flatMap(j => j.select(_._2).map(t => t: X))))
