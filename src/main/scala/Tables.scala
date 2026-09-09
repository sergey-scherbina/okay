package okay

import okay.RowLift.{In, at, plus}
import Chunks.elements

/**
 * THE ROAD TO THE AGGREGATION AS AN EFFECT (specs/bulk.md, "the effect
 * layer"). `Bulk[D]` is the platform's contract — nine primitives every
 * platform must supply. This is the PROGRAM's vocabulary over it: the
 * same operations as effects, answered by a handler that keeps the
 * platform's values on a heap and hands the program only a `Table[A]`,
 * a typed slot it cannot look inside (the `Refs.Ref` precedent).
 *
 * Why an effect and not the trait alone: an effect row is OPEN. A new
 * operation is a new signature in the row (`Sort`, below), handled
 * either by translation into these primitives — platform-free, written
 * once — or natively by a platform that has something better (Spark's
 * `sortBy`, in okay-spark). Nothing in `Bulk` changes, no platform's
 * build breaks, and a program that needs the new operation SAYS so in
 * its type: `Table[Dep] ! (Tables + Sort)`. And the program is a value:
 * `!.tracing` prints the plan, and a rewrite before the run — project
 * before the join — is a walk over data rather than a new API.
 *
 * The combinators are row-polymorphic through membership (`In`), so
 * `csv(p).select(f).join(q)` reads as a query and types in any row that
 * carries `Tables`. On a bare handle the same names give a `! Tables`
 * program, for direct style: `val hours = !deps.aggregate(hourly)`.
 */
enum Tables[+A] derives okay.Effect:
  case Of[A](xs: Iterable[A]) extends Tables[Tables.Table[A]]
  case Read(path: String) extends Tables[Tables.Table[okay.Csv.Row]]
  case Columns(t: Tables.Table[okay.Csv.Row], names: Set[String]) extends Tables[Tables.Table[okay.Csv.Row]]
  case Select[A, B](t: Tables.Table[A], f: A => B) extends Tables[Tables.Table[B]]
  case Expand[A, B](t: Tables.Table[A], f: A => IterableOnce[B]) extends Tables[Tables.Table[B]]
  case Where[A](t: Tables.Table[A], p: A => Boolean) extends Tables[Tables.Table[A]]
  case Join[K, A, B](l: Tables.Table[(K, A)], r: Tables.Table[(K, B)]) extends Tables[Tables.Table[(K, (A, B))]]
  case Cache[A](t: Tables.Table[A]) extends Tables[Tables.Table[A]]
  case Aggregate[A, Acc, Out](t: Tables.Table[A], agg: Aggregator[A, Acc, Out]) extends Tables[Out]
  case Collect[A](t: Tables.Table[A]) extends Tables[Chunks[A]]

object Tables:
  /** a slot on the handler's heap: typed for the program, a number for
   * the handler, and declared HERE so only the handler sees the number */
  opaque type Table[A] = Int

  /**
   * THE PLAN (bulk-plan): first-order, so a rewrite can see ALL of it.
   * The effect above builds these; nothing runs until an action
   * (`Cache`, `Aggregate`, `Collect`) forces a table, and at that
   * moment the whole lineage of that table is one tree — which is what
   * pushing a projection into a `Read`, or putting the small side of a
   * join on the right, needs to see. A `Free` program could not offer
   * this (its continuations are functions); the tree can, and the
   * effect keeps the open vocabulary on top of it.
   */
  enum Plan[A]:
    case Of(xs: Iterable[A])
    case Read(path: String, columns: Option[Set[String]]) extends Plan[okay.Csv.Row]
    case Columns(p: Plan[okay.Csv.Row], names: Set[String]) extends Plan[okay.Csv.Row]
    case Select[A, B](p: Plan[A], f: A => B) extends Plan[B]
    case Expand[A, B](p: Plan[A], f: A => IterableOnce[B]) extends Plan[B]
    case Where[A](p: Plan[A], q: A => Boolean) extends Plan[A]
    case Join[K, A, B](l: Plan[(K, A)], r: Plan[(K, B)]) extends Plan[(K, (A, B))]
    /** a table the platform already holds — a materialised boundary */
    case Held[A](slot: Int) extends Plan[A]

  object Plan:
    /** the tree, one node per line, for the eye */
    def show(p: Plan[?], depth: Int = 0): String =
      val pad = "  " * depth
      p match
        case Of(xs) => s"${pad}Of(${xs.knownSize match { case -1 => "?"; case n => n.toString }})"
        case Read(path, cols) => s"${pad}Read(${path.split('/').last}${cols.fold("")(cs => ", " + cs.toSeq.sorted.mkString("[", " ", "]"))})"
        case Columns(q, cs) => s"${pad}Columns${cs.toSeq.sorted.mkString("[", " ", "]")}\n" + show(q, depth + 1)
        case Select(q, _) => s"${pad}Select\n" + show(q, depth + 1)
        case Expand(q, _) => s"${pad}Expand\n" + show(q, depth + 1)
        case Where(q, _) => s"${pad}Where\n" + show(q, depth + 1)
        case Join(l, r) => s"${pad}Join\n" + show(l, depth + 1) + "\n" + show(r, depth + 1)
        case Held(slot) => s"${pad}Held#$slot"

    /**
     * What a plan is worth in bytes, when a platform can say: a `Read`
     * is its file, an `Of` its element count, a step keeps its child's
     * estimate (selectivity unknown), a join the larger side, a held
     * table unknown. Coarse on purpose — it only has to order two sides.
     */
    def estimate(p: Plan[?], size: String => Option[Long]): Option[Long] = p match
      case Of(xs) => Option.when(xs.knownSize >= 0)(xs.knownSize.toLong)
      case Read(path, _) => size(path)
      case Columns(q, _) => estimate(q, size)
      case Select(q, _) => estimate(q, size)
      case Expand(q, _) => estimate(q, size)
      case Where(q, _) => estimate(q, size)
      case Join(l, r) => for a <- estimate(l, size); b <- estimate(r, size) yield a max b
      case Held(_) => None

    /**
     * THE REWRITES, bottom-up. (1) A projection meets its `Read`:
     * `Columns(Read(p))` becomes `Read(p, cols)`, and the platform
     * prunes at the parser; two projections meet as their
     * intersection. (2) A join whose left side is estimated smaller
     * than its right is turned around and its answer swapped back, so
     * the small side is the one a platform hashes or broadcasts.
     * Everything else is left as written: a function is opaque, and a
     * rewrite that guessed through one would be wrong silently.
     */
    def optimize[A](p: Plan[A], size: String => Option[Long]): Plan[A] = p match
      case Columns(q, cs) => optimize(q, size) match
        case Read(path, None) => Read(path, Some(cs))
        case Read(path, Some(had)) => Read(path, Some(had & cs))
        case Columns(q2, cs2) => Columns(q2, cs2 & cs)
        case q2 => Columns(q2, cs)
      case Select(q, f) => Select(optimize(q, size), f)
      case Expand(q, f) => Expand(optimize(q, size), f)
      case Where(q, f) => Where(optimize(q, size), f)
      case Join(l, r) =>
        val (l2, r2) = (optimize(l, size), optimize(r, size))
        (estimate(l2, size), estimate(r2, size)) match
          case (Some(a), Some(b)) if a < b => turned(Join(r2, l2))
          case _ => Join(l2, r2)
      case leaf => leaf

    /** a join taken the other way round, its answer turned back: the
     * types do the bookkeeping, so no cast is needed for the swap */
    private def turned[K, X, Y](j: Plan[(K, (Y, X))]): Plan[(K, (X, Y))] =
      Select(j, kv => (kv._1, (kv._2._2, kv._2._1)))

  /**
   * The handler's heap: PLANS for the tables the program built, and
   * the platform's values for the ones it forced. Threaded as State so
   * the residual program stays re-runnable. THE ONE CAST is `plan` (and
   * its held twin): a heap keyed by slot cannot be typed, and every
   * value in it was put there by an operation whose answer type named
   * the same A (see Refs.slot).
   */
  final case class Heap[D[_]](next: Int, plans: Map[Int, Plan[?]], held: Map[Int, Any]):
    def put[A](p: Plan[A]): (Table[A], Heap[D]) = (next, Heap(next + 1, plans.updated(next, p), held))
    def plan[A](t: Table[A]): Plan[A] = plans(t).asInstanceOf[Plan[A]]
    /** hold a platform value: a new table whose plan is the boundary */
    def hold[A](d: D[A]): (Table[A], Heap[D]) =
      (next, Heap(next + 1, plans.updated(next, Plan.Held[A](next)), held.updated(next, d)))
    /** the platform's value for a table: its plan, rewritten, compiled */
    def force[A](t: Table[A])(B: Bulk[D]): D[A] = compile(B)(Plan.optimize(plan(t), B.size))
    def compile[A](B: Bulk[D])(p: Plan[A]): D[A] = p match
      case Plan.Of(xs) => B.of(xs)
      case Plan.Read(path, cols) => B.csv(path, cols)
      case Plan.Columns(q, cs) => B.map(compile(B)(q))(row => row.filter((k, _) => cs(k)))
      case Plan.Select(q, f) => B.map(compile(B)(q))(f)
      case Plan.Expand(q, f) => B.flatMap(compile(B)(q))(f)
      case Plan.Where(q, f) => B.filter(compile(B)(q))(f)
      case Plan.Join(l, r) => B.join(compile(B)(l), compile(B)(r))
      case Plan.Held(slot) => held(slot).asInstanceOf[D[A]]
  object Heap:
    def empty[D[_]]: Heap[D] = Heap(0, Map.empty, Map.empty)

  // --------------------------------------------------------- sources
  inline def of[A](xs: Iterable[A]): Table[A] ! Tables = effect(Of(xs))
  inline def read(path: String): Table[okay.Csv.Row] ! Tables = effect(Read(path))

  extension (t: Table[okay.Csv.Row])
    /** keep these columns — structural, so the rewrite can push it into the read */
    inline def columns(names: String*): Table[okay.Csv.Row] ! Tables = effect(Columns(t, names.toSet))
  extension [F[+_]](p: Table[okay.Csv.Row] ! F)(using In[Tables, F])
    def columns(names: String*): Table[okay.Csv.Row] ! F = p.flatMap(t => t.columns(names*).at[F])

  // ------------------------------------------- on a handle: ! Tables
  extension [A](t: Table[A])
    inline def select[B](f: A => B): Table[B] ! Tables = effect(Select(t, f))
    inline def expand[B](f: A => IterableOnce[B]): Table[B] ! Tables = effect(Expand(t, f))
    inline def where(p: A => Boolean): Table[A] ! Tables = effect(Where(t, p))
    inline def cache: Table[A] ! Tables = effect(Cache(t))
    inline def aggregate[Acc, Out](agg: Aggregator[A, Acc, Out]): Out ! Tables = effect(Aggregate(t, agg))
    inline def collect: Chunks[A] ! Tables = effect(Collect(t))
  extension [K, A](l: Table[(K, A)])
    inline def join[B](r: Table[(K, B)]): Table[(K, (A, B))] ! Tables = effect(Join(l, r))

  // ------------------------- on a program: any row that has Tables
  extension [A, F[+_]](p: Table[A] ! F)(using In[Tables, F])
    def select[B](f: A => B): Table[B] ! F = p.flatMap(t => t.select(f).at[F])
    def expand[B](f: A => IterableOnce[B]): Table[B] ! F = p.flatMap(t => t.expand(f).at[F])
    def where(q: A => Boolean): Table[A] ! F = p.flatMap(t => t.where(q).at[F])
    def cache: Table[A] ! F = p.flatMap(t => t.cache.at[F])
    def aggregate[Acc, Out](agg: Aggregator[A, Acc, Out]): Out ! F = p.flatMap(t => t.aggregate(agg).at[F])
    def collect: Chunks[A] ! F = p.flatMap(t => t.collect.at[F])
  extension [K, A, F[+_]](l: Table[(K, A)] ! F)(using In[Tables, F])
    def join[B](r: Table[(K, B)] ! F): Table[(K, (A, B))] ! F =
      l.flatMap(lt => r.flatMap(rt => lt.join(rt).at[F]))
    /** the right side already a handle — a table bound earlier in direct style */
    def join[B](r: Table[(K, B)]): Table[(K, (A, B))] ! F =
      l.flatMap(lt => lt.join(r).at[F])

  /**
   * The platform, as a translation: a building operation becomes a
   * PLAN NODE on the heap (nothing runs), an action forces the table it
   * names — rewrite, compile through `B`, run — and the heap is `State`,
   * so a native extension (okay-spark's sort) can share it by
   * translating into the same state, with no hook in this handler and
   * no knowledge of it here. `log` sees every plan as it is forced.
   */
  def via[A, D[_], F[+_]](B: Bulk[D], log: Plan[?] => Unit = _ => (), rewrite: Boolean = true)(p: A ! (Tables + F))
  : A ! (State % Heap[D] + F) =
    def forced[X](h: Heap[D], t: Table[X]): D[X] =
      val plan = if rewrite then Plan.optimize(h.plan(t), B.size) else h.plan(t)
      log(plan)
      h.compile(B)(plan)
    !.interpret(p):
      [X] => (e: Tables[X]) => e match
        case Of(xs) => State.update[Heap[D], X](_.put(Plan.Of(xs))).plus[F]
        case Read(path) => State.update[Heap[D], X](_.put(Plan.Read(path, None))).plus[F]
        case Columns(t, cs) => State.update[Heap[D], X](h => h.put(Plan.Columns(h.plan(t), cs))).plus[F]
        case Select(t, f) => State.update[Heap[D], X](h => h.put(Plan.Select(h.plan(t), f))).plus[F]
        case Expand(t, f) => State.update[Heap[D], X](h => h.put(Plan.Expand(h.plan(t), f))).plus[F]
        case Where(t, q) => State.update[Heap[D], X](h => h.put(Plan.Where(h.plan(t), q))).plus[F]
        case Join(l, r) => State.update[Heap[D], X](h => h.put(Plan.Join(h.plan(l), h.plan(r)))).plus[F]
        case Cache(t) => State.update[Heap[D], X](h => h.hold(B.cache(forced(h, t)))).plus[F]
        case Aggregate(t, agg) => State.get[Heap[D]].map(h => B.aggregate(forced(h, t))(agg)).plus[F]
        case Collect(t) => State.get[Heap[D]].map(h => B.toChunks(forced(h, t))).plus[F]

  /** run a program of tables, and nothing else, on a platform */
  def run[A, D[_]](B: Bulk[D])(p: A ! Tables): A =
    State.run(Heap.empty[D])(via[A, D, okay.Pure](B)(p))._2

/**
 * An operation `Bulk` does not have, added WITHOUT touching it: a new
 * signature in the row. `Sort.viaTables` is the platform-free answer
 * (collect, sort, hand back — correct anywhere); a platform with a
 * native sort answers it natively instead (okay-spark's
 * `SparkBulk.sort`), and the program does not change either way.
 */
enum Sort[+A] derives okay.Effect:
  case By[A, K](t: Tables.Table[A], key: A => K, ord: Ordering[K]) extends Sort[Tables.Table[A]]

object Sort:
  import Tables.Table

  extension [A](t: Table[A])
    inline def sortBy[K](key: A => K)(using ord: Ordering[K]): Table[A] ! Sort = effect(By(t, key, ord))
  extension [A, F[+_]](p: Table[A] ! F)(using In[Sort, F])
    def sortBy[K](key: A => K)(using ord: Ordering[K]): Table[A] ! F = p.flatMap(t => t.sortBy(key).at[F])

  /** the default: through the primitives, so it runs on any platform */
  def viaTables[A, G[+_]](p: A ! (Sort + G)): A ! (Tables + G) =
    !.interpret(p):
      [X] => (e: Sort[X]) => e match
        case By(t, key, ord) =>
          // `X >: Table[A]` is what the match refines under covariance; `!` is
          // invariant in its answer, so the widening is spelled as a map
          t.collect.plus[G].flatMap(c => Tables.of(c.elements.toVector.sortBy(key)(using ord)).plus[G].map(t => t: X))
