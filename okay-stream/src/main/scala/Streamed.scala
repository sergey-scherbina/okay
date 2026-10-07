package okay


import okay.freer.*
import Chunks.elements
import Row.{In, at, plus}

/**
 * THE STREAM OPERATORS AS A SIGNATURE IN THE ROW (specs/streams-seam.md,
 * lane 2), beside `Tables` and `Sort`, by the extension rule of
 * specs/bulk.md: a new operation is a new signature, never a method on
 * `Bulk`. A program that joins two tables by key within an interval, or
 * windows one in event time, types in any row that carries `Streamed`
 * and names no platform; `viaTables` below is the platform-free answer
 * through `collect` and the local machines (`SortMerge`, `WindowJoin`,
 * `Windows`, `Chunks.zip`), correct anywhere, and a platform with a
 * native road answers instead — okay-cluster's `FlowBulk.streamed`
 * (the engine's `Flow.Join` and `Flow.Windowed`); Spark's and Flink's
 * are lanes of their own.
 *
 * WHAT A BOUNDED TABLE MEANS FOR EACH. `JoinSorted`: the sides are
 * non-decreasing in key (checked, as `Chunks.joinSorted` checks).
 * `JoinWithin`: the interval join — every pair sharing a key whose
 * event times are within `within` of each other; on a bounded table
 * each side is fed in ITS TIME ORDER, so no row is ever late and
 * `lateness` changes nothing (it is kept in the signature for the
 * platforms whose answer is a stream). `Windowed`: `Windows`' panes over
 * the rows in the TABLE'S order with the given lateness — the order is
 * the arrival order, which is what the engine's seeded partitions
 * reproduce at any parallelism (specs/dataflow.md). `Zip`: positional,
 * defined for a bounded table; on a partitioned platform it is defined
 * only when both sides are partitioned alike, and a platform says so.
 */
enum Streamed[+A] derives Effect:
  case JoinSorted[K, A, B](l: Tables.Table[(K, A)], r: Tables.Table[(K, B)], ord: Ordering[K])
    extends Streamed[Tables.Table[(K, (A, B))]]
  case JoinWithin[K, A, B](l: Tables.Table[(K, A)], r: Tables.Table[(K, B)],
                           within: Long, lateness: Long, atL: A => Long, atR: B => Long)
    extends Streamed[Tables.Table[(K, (A, B))]]
  case Windowed[K, A, Acc, O](t: Tables.Table[A], size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long, agg: Aggregator[A, Acc, O])
    extends Streamed[Tables.Table[Pane[K, O]]]
  case Zip[A, B](l: Tables.Table[A], r: Tables.Table[B]) extends Streamed[Tables.Table[(A, B)]]

object Streamed:
  import Tables.Table

  // ------------------------------------------- on a handle: ! Streamed
  extension [K, A](l: Table[(K, A)])
    inline def joinSorted[B](r: Table[(K, B)])(using ord: Ordering[K]): Table[(K, (A, B))] ! Streamed =
      effect(JoinSorted(l, r, ord))
    inline def joinWithin[B](r: Table[(K, B)], within: Long, lateness: Long)
                            (atL: A => Long, atR: B => Long): Table[(K, (A, B))] ! Streamed =
      effect(JoinWithin(l, r, within, lateness, atL, atR))
  extension [A](t: Table[A])
    inline def windowed[K, Acc, O](size: Long, slide: Long, lateness: Long)
                                  (key: A => K)(at: A => Long)
                                  (agg: Aggregator[A, Acc, O]): Table[Pane[K, O]] ! Streamed =
      effect(Windowed(t, size, slide, lateness, key, at, agg))
    inline def zip[B](r: Table[B]): Table[(A, B)] ! Streamed = effect(Zip(t, r))

  // ------------------------- on a program: any row that has Streamed
  extension [K, A, F[+_]](l: Table[(K, A)] ! F)(using In[Streamed, F])
    def joinSorted[B](r: Table[(K, B)] ! F)(using Ordering[K]): Table[(K, (A, B))] ! F =
      l.flatMap(lt => r.flatMap(rt => lt.joinSorted(rt).at[F]))
    def joinWithin[B](r: Table[(K, B)] ! F, within: Long, lateness: Long)
                     (atL: A => Long, atR: B => Long): Table[(K, (A, B))] ! F =
      l.flatMap(lt => r.flatMap(rt => lt.joinWithin(rt, within, lateness)(atL, atR).at[F]))
  extension [A, F[+_]](p: Table[A] ! F)(using In[Streamed, F])
    def windowed[K, Acc, O](size: Long, slide: Long, lateness: Long)
                           (key: A => K)(at: A => Long)
                           (agg: Aggregator[A, Acc, O]): Table[Pane[K, O]] ! F =
      p.flatMap(t => t.windowed(size, slide, lateness)(key)(at)(agg).at[F])
    def zip[B](r: Table[B] ! F): Table[(A, B)] ! F =
      p.flatMap(t => r.flatMap(rt => t.zip(rt).at[F]))

  // ------------------------------------------------ the local machines
  /** the interval join of two bounded sides, each fed in its time order */
  def joinWithinLocal[K, A, B](ls: Iterable[(K, A)], rs: Iterable[(K, B)], within: Long, lateness: Long,
                               atL: A => Long, atR: B => Long): Vector[(K, (A, B))] =
    val j = new WindowJoin[K, A, B](within, lateness, atL, atR)
    val out = Vector.newBuilder[(K, (A, B))]
    val emit: ((K, (A, B))) => Unit = o => { out += o; () }
    for (k, a) <- ls.toVector.sortBy((_, a) => atL(a)) do j.left(k, a)(emit)
    j.leftEnd()
    for (k, b) <- rs.toVector.sortBy((_, b) => atR(b)) do j.right(k, b)(emit)
    j.rightEnd()
    out.result()

  /** `Windows`' panes over rows in their given order */
  def windowedLocal[K, A, Acc, O](xs: Iterable[A], size: Long, slide: Long, lateness: Long,
                                  key: A => K, at: A => Long, agg: Aggregator[A, Acc, O]): Vector[Pane[K, O]] =
    val w = new Windows[K, A, Acc, O](size, slide, lateness, key, at, agg)
    val out = Vector.newBuilder[Pane[K, O]]
    for x <- xs do w.add(x)(p => { out += p; () })
    w.close()(p => { out += p; () })
    out.result()

  /** the default: through the primitives, so it runs on any platform */
  def viaTables[A, G[+_]](p: A ! Streamed + G): A ! Tables + G =
    !.interpret(p):
      [X] => (e: Streamed[X]) => e match
        case JoinSorted(l, r, ord) =>
          l.collect.plus[G].flatMap(cl => r.collect.plus[G].flatMap(cr =>
            Tables.of(Chunks.joinSorted(cl, cr)(using ord).elements.toVector).plus[G].map(t => t: X)))
        case JoinWithin(l, r, within, lateness, atL, atR) =>
          l.collect.plus[G].flatMap(cl => r.collect.plus[G].flatMap(cr =>
            Tables.of(joinWithinLocal(cl.elements.toVector, cr.elements.toVector, within, lateness, atL, atR))
              .plus[G].map(t => t: X)))
        case Windowed(t, size, slide, lateness, key, at, agg) =>
          t.collect.plus[G].flatMap(c =>
            Tables.of(windowedLocal(c.elements.toVector, size, slide, lateness, key, at, agg)).plus[G].map(t => t: X))
        case Zip(l, r) =>
          l.collect.plus[G].flatMap(cl => r.collect.plus[G].flatMap(cr =>
            Tables.of(Chunks.zip(cl, cr).elements.toVector).plus[G].map(t => t: X)))
