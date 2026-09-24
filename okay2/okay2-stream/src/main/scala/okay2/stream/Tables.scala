package okay2.stream

import okay2._
import Chunks.ChunksOps

/**
 * A program of TABLES — the Scala 3 core's okay-stream `Tables`: the
 * effect builds a first-order PLAN, nothing runs until an action
 * (`cache`, `aggregate`, `collect`) forces a table, and at that moment
 * the whole lineage of that table is one tree a rewrite can see. The
 * handler is a translation into `State` over a `Heap`, run on any
 * `Bulk` platform.
 *
 * Each operation and each plan node carries its own translation as a
 * METHOD (`run`, `compile`, `optimize`): scalac 2 does not refine a
 * method's type parameters from a constructor pattern, and the core's
 * matches over the cases would need a cast per arm here. The one cast
 * left is the core's own: a heap keyed by slot (`Heap.plan`/`held`).
 */
sealed trait Tables extends Row { type Op[+A] = Tables.Op[A] }

object Tables {
  implicit val effect: Effect[Tables] = Effect.of[Tables]

  /** a slot on the handler's heap: typed for the program, a number for
   * the handler — the core's opaque `Int`, as a value class */
  final class Table[A] private[stream] (val slot: Int) extends AnyVal

  /** what an operation needs to become a heap step */
  final class Ctx[D[_]](val B: Bulk[D], val forced: Heap[D] => Table[_] => Plan[_], val rewrite: Boolean, val log: Plan[_] => Unit) {
    def force[X](h: Heap[D], t: Table[X]): D[X] = {
      val plan = if (rewrite) h.plan(t).optimize(B.size) else h.plan(t)
      log(plan)
      plan.compile(B, h)
    }
  }

  sealed abstract class Op[+X] {
    /** this operation as a step of the heap — spelled `Free[...]`,
     * which is covariant in its answer as the operation is */
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], X]
  }
  final case class Of[A](xs: Iterable[A]) extends Op[Table[A]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[A]] = State.update[Heap[D], Table[A]](_.put(Plan.Of(xs)))
  }
  final case class Read(path: String) extends Op[Table[Csv.Row]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[Csv.Row]] = State.update[Heap[D], Table[Csv.Row]](_.put(Plan.Read(path, None)))
  }
  final case class Columns(t: Table[Csv.Row], names: Set[String]) extends Op[Table[Csv.Row]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[Csv.Row]] = State.update[Heap[D], Table[Csv.Row]](h => h.put(Plan.Columns(h.plan(t), names)))
  }
  final case class Select[A, B](t: Table[A], f: A => B) extends Op[Table[B]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[B]] = State.update[Heap[D], Table[B]](h => h.put(Plan.Select(h.plan(t), f)))
  }
  final case class Expand[A, B](t: Table[A], f: A => IterableOnce[B]) extends Op[Table[B]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[B]] = State.update[Heap[D], Table[B]](h => h.put(Plan.Expand(h.plan(t), f)))
  }
  final case class Where[A](t: Table[A], p: A => Boolean) extends Op[Table[A]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[A]] = State.update[Heap[D], Table[A]](h => h.put(Plan.Where(h.plan(t), p)))
  }
  final case class Join[K, A, B](l: Table[(K, A)], r: Table[(K, B)]) extends Op[Table[(K, (A, B))]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[(K, (A, B))]] =
      State.update[Heap[D], Table[(K, (A, B))]](h => h.put(Plan.Join(h.plan(l), h.plan(r))))
  }
  final case class Cache[A](t: Table[A]) extends Op[Table[A]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Table[A]] = State.update[Heap[D], Table[A]](h => h.hold(c.B.cache(c.force(h, t))))
  }
  final case class Aggregate[A, Acc, Out](t: Table[A], agg: Aggregator[A, Acc, Out]) extends Op[Out] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Out] = State.get[Heap[D]].map(h => c.B.aggregate(c.force(h, t))(agg))
  }
  final case class Collect[A](t: Table[A]) extends Op[Chunks[A]] {
    private[stream] def run[D[_]](c: Ctx[D]): Free[State[Heap[D]], Chunks[A]] = State.get[Heap[D]].map(h => c.B.toChunks(c.force(h, t)))
  }

  /**
   * THE PLAN: first-order, so a rewrite can see ALL of it. A `Free`
   * program could not offer this (its continuations are functions); the
   * tree can, and the effect keeps the open vocabulary on top of it.
   */
  sealed abstract class Plan[A] {
    /** the platform's value for this plan */
    def compile[D[_]](B: Bulk[D], h: Heap[D]): D[A]
    /** THE REWRITES, bottom-up: a projection meets its `Read` (the
     * platform prunes at the parser); a join whose left side is
     * estimated smaller is turned around and its answer swapped back.
     * A function is opaque, and a rewrite that guessed through one
     * would be wrong silently */
    def optimize(size: String => Option[Long]): Plan[A]
  }

  object Plan {
    final case class Of[A](xs: Iterable[A]) extends Plan[A] {
      def compile[D[_]](B: Bulk[D], h: Heap[D]): D[A] = B.of(xs)
      def optimize(size: String => Option[Long]): Plan[A] = this
    }
    final case class Read(path: String, columns: Option[Set[String]]) extends Plan[Csv.Row] {
      def compile[D[_]](B: Bulk[D], h: Heap[D]): D[Csv.Row] = B.csv(path, columns)
      def optimize(size: String => Option[Long]): Plan[Csv.Row] = this
    }
    final case class Columns(p: Plan[Csv.Row], names: Set[String]) extends Plan[Csv.Row] {
      def compile[D[_]](B: Bulk[D], h: Heap[D]): D[Csv.Row] = B.map(p.compile(B, h))(row => row.filter { case (k, _) => names(k) })
      def optimize(size: String => Option[Long]): Plan[Csv.Row] = p.optimize(size) match {
        case Read(path, None) => Read(path, Some(names))
        case Read(path, Some(had)) => Read(path, Some(had & names))
        case Columns(q2, cs2) => Columns(q2, cs2 & names)
        case q2 => Columns(q2, names)
      }
    }
    final case class Select[A, B](p: Plan[A], f: A => B) extends Plan[B] {
      def compile[D[_]](Bk: Bulk[D], h: Heap[D]): D[B] = Bk.map(p.compile(Bk, h))(f)
      def optimize(size: String => Option[Long]): Plan[B] = Select(p.optimize(size), f)
    }
    final case class Expand[A, B](p: Plan[A], f: A => IterableOnce[B]) extends Plan[B] {
      def compile[D[_]](Bk: Bulk[D], h: Heap[D]): D[B] = Bk.flatMap(p.compile(Bk, h))(f)
      def optimize(size: String => Option[Long]): Plan[B] = Expand(p.optimize(size), f)
    }
    final case class Where[A](p: Plan[A], q: A => Boolean) extends Plan[A] {
      def compile[D[_]](B: Bulk[D], h: Heap[D]): D[A] = B.filter(p.compile(B, h))(q)
      def optimize(size: String => Option[Long]): Plan[A] = Where(p.optimize(size), q)
    }
    final case class Join[K, A, B](l: Plan[(K, A)], r: Plan[(K, B)]) extends Plan[(K, (A, B))] {
      def compile[D[_]](Bk: Bulk[D], h: Heap[D]): D[(K, (A, B))] = Bk.join(l.compile(Bk, h), r.compile(Bk, h))
      def optimize(size: String => Option[Long]): Plan[(K, (A, B))] = {
        val (l2, r2) = (l.optimize(size), r.optimize(size))
        (estimate(l2, size), estimate(r2, size)) match {
          // the small side to the right, the answer turned back — the
          // types do the bookkeeping, no cast
          case (Some(a), Some(b)) if a < b => Select[(K, (B, A)), (K, (A, B))](Join(r2, l2), kv => (kv._1, (kv._2._2, kv._2._1)))
          case _ => Join(l2, r2)
        }
      }
    }
    /** a table the platform already holds — a materialised boundary */
    final case class Held[A](slot: Int) extends Plan[A] {
      def compile[D[_]](B: Bulk[D], h: Heap[D]): D[A] = h.heldAt[A](slot)
      def optimize(size: String => Option[Long]): Plan[A] = this
    }

    /** the tree, one node per line, for the eye */
    def show(p: Plan[_], depth: Int = 0): String = {
      val pad = "  " * depth
      p match {
        case Of(xs) => s"${pad}Of(${xs.knownSize match { case -1 => "?"; case n => n.toString }})"
        case Read(path, cols) => s"${pad}Read(${path.split('/').last}${cols.fold("")(cs => ", " + cs.toSeq.sorted.mkString("[", " ", "]"))})"
        case Columns(q, cs) => s"${pad}Columns${cs.toSeq.sorted.mkString("[", " ", "]")}\n" + show(q, depth + 1)
        case Select(q, _) => s"${pad}Select\n" + show(q, depth + 1)
        case Expand(q, _) => s"${pad}Expand\n" + show(q, depth + 1)
        case Where(q, _) => s"${pad}Where\n" + show(q, depth + 1)
        case Join(l, r) => s"${pad}Join\n" + show(l, depth + 1) + "\n" + show(r, depth + 1)
        case Held(slot) => s"${pad}Held#$slot"
      }
    }

    /** what a plan is worth in bytes, when a platform can say — coarse
     * on purpose, it only has to order two sides */
    def estimate(p: Plan[_], size: String => Option[Long]): Option[Long] = p match {
      case Of(xs) => Option.when(xs.knownSize >= 0)(xs.knownSize.toLong)
      case Read(path, _) => size(path)
      case Columns(q, _) => estimate(q, size)
      case Select(q, _) => estimate(q, size)
      case Expand(q, _) => estimate(q, size)
      case Where(q, _) => estimate(q, size)
      case Join(l, r) => for { a <- estimate(l, size); b <- estimate(r, size) } yield a max b
      case Held(_) => None
    }

    /** the core's module-level spelling */
    def optimize[A](p: Plan[A], size: String => Option[Long]): Plan[A] = p.optimize(size)
  }

  /**
   * The handler's heap: PLANS for the tables the program built, the
   * platform's values for the ones it forced. Threaded as State so the
   * residual program stays re-runnable. THE ONE CAST is `plan` (and its
   * held twin): a heap keyed by slot cannot be typed, and every value in
   * it was put there by an operation whose answer type named the same A.
   */
  final case class Heap[D[_]](next: Int, plans: Map[Int, Plan[_]], held: Map[Int, Any]) {
    def put[A](p: Plan[A]): (Table[A], Heap[D]) = (new Table[A](next), Heap[D](next + 1, plans.updated(next, p), held))
    def plan[A](t: Table[A]): Plan[A] = plans(t.slot).asInstanceOf[Plan[A]]
    private[stream] def heldAt[A](slot: Int): D[A] = held(slot).asInstanceOf[D[A]]
    /** hold a platform value: a new table whose plan is the boundary */
    def hold[A](d: D[A]): (Table[A], Heap[D]) =
      (new Table[A](next), Heap[D](next + 1, plans.updated(next, Plan.Held[A](next)), held.updated(next, d)))
  }
  object Heap {
    def empty[D[_]]: Heap[D] = Heap[D](0, Map.empty, Map.empty)
  }

  // ---------------------------------------------------------------- the doors

  def of[A](xs: Iterable[A]): Table[A] ! Tables = Free.inject[Tables, Table[A]](Of(xs))
  def read(path: String): Table[Csv.Row] ! Tables = Free.inject[Tables, Table[Csv.Row]](Read(path))

  /** on a handle: each step a program of `Tables` (plain classes: a
   * value class may not wrap the value class `Table`) */
  implicit final class TableOps[A](t: Table[A]) {
    def select[B](f: A => B): Table[B] ! Tables = Free.inject[Tables, Table[B]](Select(t, f))
    def expand[B](f: A => IterableOnce[B]): Table[B] ! Tables = Free.inject[Tables, Table[B]](Expand(t, f))
    def where(p: A => Boolean): Table[A] ! Tables = Free.inject[Tables, Table[A]](Where(t, p))
    def cache: Table[A] ! Tables = Free.inject[Tables, Table[A]](Cache(t))
    def aggregate[Acc, Out](agg: Aggregator[A, Acc, Out]): Out ! Tables = Free.inject[Tables, Out](Aggregate(t, agg))
    def collect: Chunks[A] ! Tables = Free.inject[Tables, Chunks[A]](Collect(t))
  }
  implicit final class RowTableOps(t: Table[Csv.Row]) {
    /** keep these columns — structural, so the rewrite can push it into the read */
    def columns(names: String*): Table[Csv.Row] ! Tables = Free.inject[Tables, Table[Csv.Row]](Columns(t, names.toSet))
  }
  implicit final class JoinOps[K, A](l: Table[(K, A)]) {
    def join[B](r: Table[(K, B)]): Table[(K, (A, B))] ! Tables = Free.inject[Tables, Table[(K, (A, B))]](Join(l, r))
  }

  /** on a PROGRAM whose row holds `Tables` (`R <: Tables`: a program of
   * `Tables` alone is then a program of `R`, by contravariance) */
  implicit final class ProgramOps[A, R <: Tables](private val p: Free[R, Table[A]]) extends AnyVal {
    def select[B](f: A => B): Free[R, Table[B]] = p.flatMap(t => t.select(f))
    def expand[B](f: A => IterableOnce[B]): Free[R, Table[B]] = p.flatMap(t => t.expand(f))
    def where(q: A => Boolean): Free[R, Table[A]] = p.flatMap(t => t.where(q))
    def cache: Free[R, Table[A]] = p.flatMap(t => t.cache)
    def aggregate[Acc, Out](agg: Aggregator[A, Acc, Out]): Free[R, Out] = p.flatMap(t => t.aggregate(agg))
    def collect: Free[R, Chunks[A]] = p.flatMap(t => t.collect)
  }
  implicit final class RowProgramOps[R <: Tables](private val p: Free[R, Table[Csv.Row]]) extends AnyVal {
    def columns(names: String*): Free[R, Table[Csv.Row]] = p.flatMap(t => t.columns(names: _*))
  }
  implicit final class JoinProgramOps[K, A, R <: Tables](private val l: Free[R, Table[(K, A)]]) extends AnyVal {
    def join[B](r: Free[R, Table[(K, B)]]): Free[R, Table[(K, (A, B))]] = l.flatMap(lt => r.flatMap(rt => lt.join(rt)))
  }

  /** THE PLATFORM, AS A TRANSLATION: a building operation becomes a plan
   * node on the heap (nothing runs), an action forces the table it
   * names — rewrite, compile through `B`, run. `log` sees every plan as
   * it is forced */
  def via[A, D[_], F <: Row](B: Bulk[D], log: Plan[_] => Unit = (_: Plan[_]) => (), rewrite: Boolean = true)(p: Free[Tables with F, A])
                            (implicit d: Distinct[Tables with (State[Heap[D]] + F)]): A ! (State[Heap[D]] + F) = {
    val c = new Ctx[D](B, h => t => h.plan(t), rewrite, log)
    Effects.interpret[A, Tables, State[Heap[D]], F](p)(new Interpret[Tables, State[Heap[D]] + F] {
      def apply[X](e: Op[X]): X ! (State[Heap[D]] + F) = e.run(c)
    })(effect, d)
  }

  /** run a program of tables, and nothing else, on a platform */
  def run[A, D[_]](B: Bulk[D])(p: A ! Tables): A = State.run(Heap.empty[D])(via[A, D, Pure](B)(p))._2
}

/**
 * An operation `Bulk` does not have, added WITHOUT touching it: a new
 * signature in the row. `Sort.viaTables` is the platform-free answer
 * (collect, sort, hand back); a platform with a native sort answers it
 * natively instead, and the program does not change either way.
 */
sealed trait Sort extends Row { type Op[+A] = Sort.Op[A] }

object Sort {
  import Tables.Table
  implicit val effect: Effect[Sort] = Effect.of[Sort]

  sealed abstract class Op[+X] { private[stream] def viaTables: Free[Tables, X] }
  final case class By[A, K](t: Table[A], key: A => K, ord: Ordering[K]) extends Op[Table[A]] {
    private[stream] def viaTables: Free[Tables, Table[A]] = t.collect.flatMap(c => Tables.of(c.elements.toVector.sortBy(key)(ord)))
  }

  implicit final class SortOps[A](t: Table[A]) {
    def sortBy[K](key: A => K)(implicit ord: Ordering[K]): Table[A] ! Sort = Free.inject[Sort, Table[A]](By(t, key, ord))
  }
  implicit final class SortProgramOps[A, R <: Sort](private val p: Free[R, Table[A]]) extends AnyVal {
    def sortBy[K](key: A => K)(implicit ord: Ordering[K]): Free[R, Table[A]] = p.flatMap(t => t.sortBy(key))
  }

  /** the default: through the primitives, so it runs on any platform */
  def viaTables[A, G <: Row](p: Free[Sort with G, A])(implicit d: Distinct[Sort with (Tables + G)]): A ! (Tables + G) =
    Effects.interpret[A, Sort, Tables, G](p)(new Interpret[Sort, Tables + G] {
      def apply[X](e: Op[X]): X ! (Tables + G) = e.viaTables
    })(effect, d)
}
