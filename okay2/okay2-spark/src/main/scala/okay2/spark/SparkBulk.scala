package okay2.spark

import okay2._
import okay2.stream.{Bulk, Csv, Sort, Tables}
import okay2.stream.Tables.{Ctx, Heap, Table}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession

/**
 * The `Bulk` seam on Spark (the Scala 3 core's `okay.spark.SparkBulk`):
 * one `RDD[Any]` under an "opaque" wrapper, so a program written
 * against `Bulk[D]` needs no `ClassTag` per intermediate type — the
 * evidence Spark would otherwise ask for at every `map`. Every element
 * IS an object on Spark's side; this says so in the type and pays for
 * it once, in `elem`, the one cast.
 *
 * `Rows[A]` is a VALUE CLASS with a package-private constructor, not
 * an `opaque type` — Scala 2.13 has none. This is the same
 * simulation `okay2.stream.Tables.Table[A]` already uses for ITS own
 * "opaque `Int`", not a new trick.
 */
object SparkBulk {

  final class Rows[A] private[spark] (val rdd: RDD[Any]) extends AnyVal

  private implicit val ctAny: scala.reflect.ClassTag[Any] = scala.reflect.ClassTag.Any

  private def elem[A](x: Any): A = x.asInstanceOf[A]
  private def wrap[A](rdd: RDD[Any]): Rows[A] = new Rows[A](rdd)

  /** on the COMPANION OBJECT, not the `SparkBulk` class: a task
   * closure calling an INSTANCE method captures the whole instance
   * (here, a `SparkSession` too) — `Task not serializable`, measured
   * on the first real run of the pruned-columns test. A reference to
   * this object alone, holding no per-instance state, serializes fine. */
  private def rowOf(names: Vector[String], r: org.apache.spark.sql.Row): Csv.Row =
    names.iterator.zip(r.toSeq.iterator.map(v => if (v == null) "" else v.toString)).toMap

  /** right sides up to this many rows are broadcast rather than shuffled */
  val broadcastRows: Int = 100000

  def apply(spark: SparkSession): SparkBulk = new SparkBulk(spark)

  final class SparkBulk(spark: SparkSession) extends Bulk[Rows] {
    def of[A](xs: Iterable[A]): Rows[A] = wrap(spark.sparkContext.parallelize(xs.toSeq: Seq[Any]))

    /** what the plan rewrite orders joins by: the file, when it is one */
    override def size(path: String): Option[Long] = {
      val f = new java.io.File(path)
      if (f.isFile) Some(f.length) else None
    }

    /** pruned at the PARSER: Spark's CSV reader only materialises the
     * columns selected, which is what the rewrite pushed down for */
    override def csv(path: String, columns: Option[Set[String]]): Rows[Csv.Row] = columns match {
      case None => csv(path)
      case Some(cs) =>
        val all = spark.read.option("header", "true").csv(path)
        val keep = all.columns.filter(c => cs(c.stripPrefix("﻿")))
        val df = all.select(keep.map(org.apache.spark.sql.functions.col).toIndexedSeq: _*)
        val names = keep.map(_.stripPrefix("﻿")).toVector
        wrap(df.rdd.map(r => rowOf(names, r): Any))
    }

    /** Spark's own CSV reader, the header as names; a BOM on the first
     * column is stripped, an absent value is the empty string */
    def csv(path: String): Rows[Csv.Row] = {
      val df = spark.read.option("header", "true").csv(path)
      val names = df.columns.map(_.stripPrefix("﻿")).toVector
      wrap(df.rdd.map(r => rowOf(names, r): Any))
    }

    def map[A, B](d: Rows[A])(f: A => B): Rows[B] = wrap(d.rdd.map(x => f(elem[A](x)): Any))
    def flatMap[A, B](d: Rows[A])(f: A => IterableOnce[B]): Rows[B] =
      wrap(d.rdd.flatMap(x => f(elem[A](x)): IterableOnce[Any]))
    def filter[A](d: Rows[A])(p: A => Boolean): Rows[A] = wrap(d.rdd.filter(x => p(elem[A](x))))

    /**
     * A small right side is BROADCAST, a large one shuffled — the
     * decision Spark SQL makes by size, made here by a bounded probe
     * (`take(broadcastRows + 1)` scans only until it has seen enough).
     */
    def join[K, A, B](l: Rows[(K, A)], r: Rows[(K, B)]): Rows[(K, (A, B))] = {
      val lp: RDD[(Any, Any)] = l.rdd.map(x => elem[(Any, Any)](x))
      val probe = r.rdd.take(broadcastRows + 1)
      if (probe.length <= broadcastRows) {
        val small = spark.sparkContext.broadcast(
          probe.iterator.map(elem[(Any, Any)]).toSeq.groupMap(_._1)(_._2))
        wrap(lp.flatMap { case (k, a) =>
          small.value.getOrElse(k, Nil).iterator.map(b => (k, (a, b)): Any)
        })
      } else {
        val rp: RDD[(Any, Any)] = r.rdd.map(x => elem[(Any, Any)](x))
        wrap(RDD.rddToPairRDDFunctions(lp).join(rp).map(x => x: Any))
      }
    }

    def cache[A](d: Rows[A]): Rows[A] = wrap(d.rdd.persist(org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK))

    def aggregate[A, Acc, Out](d: Rows[A])(agg: Aggregator[A, Acc, Out]): Out = {
      val acc: Any = d.rdd.aggregate[Any](agg.init)(
        (acc, x) => agg.add(elem[Acc](acc), elem[A](x)),
        (a, b) => agg.merge(elem[Acc](a), elem[Acc](b)))
      agg.present(elem[Acc](acc))
    }

    def toChunks[A](d: Rows[A]): okay2.stream.Chunks[A] =
      okay2.stream.Chunks.fromIterator(d.rdd.toLocalIterator.map(elem[A]))

    /**
     * `Sort` answered NATIVELY, the same way the Scala 3 core does:
     * Spark's own sort over the SAME `Tables.Heap` `Tables.via` threads
     * through `State`. Nothing in `Tables`'s own handler knows this
     * exists — both translate into `State[Heap[Rows]]`, which is how an
     * operation joins a platform without joining the platform's
     * contract. `Ctx` is built with the SAME defaults `Tables.via`
     * itself uses (`rewrite = true`, no logging): `Sort`'s own row
     * carries no way to thread a caller's `Ctx` through, so this is the
     * honest limit of matching a caller's own rewrite/log choice.
     */
    def sort[A, G <: Row](p: Free[Sort with G, A])
                         (implicit T: TypeableK[Sort], d: Distinct[Sort with (State[Heap[Rows]] + G)])
    : Free[State[Heap[Rows]] with G, A] = {
      def sorted[X, K](h: Heap[Rows], t: Table[X], key: X => K, ord: Ordering[K]): (Table[X], Heap[Rows]) = {
        val c = new Ctx[Rows](this, hh => tt => hh.plan(tt), true, (_: Tables.Plan[_]) => ())
        val keyed: RDD[(Any, Any)] = c.force(h, t).rdd.map(x => (key(elem[X](x)): Any, x): (Any, Any))
        val byKey: Ordering[Any] = Ordering.fromLessThan[Any]((a, b) => ord.lt(elem[K](a), elem[K](b)))
        h.hold[X](wrap(RDD.rddToOrderedRDDFunctions(keyed)(byKey, scala.reflect.ClassTag.Any, scala.reflect.ClassTag.Any)
          .sortByKey().values))
      }
      Effects.interpret[A, Sort, State[Heap[Rows]], G](p)(new Interpret[Sort, State[Heap[Rows]] + G] {
        def apply[X](e: Sort.Op[X]): X ! (State[Heap[Rows]] + G) = e match {
          // ERASURE CAST, ISOLATED (no-casts-without-necessity): scalac
          // 2 does not refine a method's type parameter (X) from a
          // constructor pattern (Sort.By[a, k]), so X is not known to
          // equal Table[a] here even though it always does — the same
          // trap this codebase's OWN row dispatch hits, with the same
          // fix ("Bind(Inject(e), k) instantiates the answer to Any"):
          // build the correctly-typed value first, cast the whole
          // thing once at the type checker's boundary, never the
          // runtime's.
          case b: Sort.By[a, k] =>
            State.update[Heap[Rows], Table[a]](h => sorted[a, k](h, b.t, b.key, b.ord))
              .plus[G].asInstanceOf[X ! (State[Heap[Rows]] + G)]
        }
      })(T, d)
    }
  }
}
