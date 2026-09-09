package okay

import okay.!.*
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
   * The handler's heap: the platform's values, threaded as State so the
   * residual program stays re-runnable. THE ONE CAST is `get`: a heap
   * keyed by slot cannot be typed, and every value in it was put there
   * by an operation whose answer type named the same A (see Refs.slot).
   */
  final case class Heap[D[_]](next: Int, slots: Map[Int, Any]):
    def put[A](d: D[A]): (Table[A], Heap[D]) = (next, Heap(next + 1, slots.updated(next, d)))
    def get[A](t: Table[A]): D[A] = slots(t).asInstanceOf[D[A]]
  object Heap:
    def empty[D[_]]: Heap[D] = Heap(0, Map.empty)

  // --------------------------------------------------------- sources
  inline def of[A](xs: Iterable[A]): Table[A] ! Tables = effect(Of(xs))
  inline def read(path: String): Table[okay.Csv.Row] ! Tables = effect(Read(path))

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
   * The platform, as a translation: every operation becomes one step
   * on a heap of `D` values, and the heap is `State`, so a native
   * extension (okay-spark's sort) can share it by translating into the
   * same state — no hook in this handler, no knowledge of it here.
   */
  def via[A, D[_], F[+_]](B: Bulk[D])(p: A ! (Tables + F)): A ! (State % Heap[D] + F) =
    !.interpret(p):
      [X] => (e: Tables[X]) => e match
        case Of(xs) => State.update[Heap[D], X](_.put(B.of(xs))).plus[F]
        case Read(path) => State.update[Heap[D], X](_.put(B.csv(path))).plus[F]
        case Select(t, f) => State.update[Heap[D], X](h => h.put(B.map(h.get(t))(f))).plus[F]
        case Expand(t, f) => State.update[Heap[D], X](h => h.put(B.flatMap(h.get(t))(f))).plus[F]
        case Where(t, q) => State.update[Heap[D], X](h => h.put(B.filter(h.get(t))(q))).plus[F]
        case Join(l, r) => State.update[Heap[D], X](h => h.put(B.join(h.get(l), h.get(r)))).plus[F]
        case Cache(t) => State.update[Heap[D], X](h => h.put(B.cache(h.get(t)))).plus[F]
        case Aggregate(t, agg) => State.get[Heap[D]].map(h => B.aggregate(h.get(t))(agg)).plus[F]
        case Collect(t) => State.get[Heap[D]].map(h => B.toChunks(h.get(t))).plus[F]

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
