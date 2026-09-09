package okay.spark

import okay.*
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, Encoder, SparkSession}

/**
 * Spark via the P1 contract (specs/external-systems.md): an okay
 * Aggregator's (init, add, merge) IS Spark's (zero, seqOp, combOp) —
 * one definition, local Chunks execution or cluster execution, equal
 * results. Aggregator extends Serializable in the core precisely so
 * these closures can ship as Spark tasks.
 *
 * Encoders for the Dataset side are the caller's for now: our
 * Schema-algebra derivation (ExpressionEncoder-grade, staged — see
 * specs/codecs.md) lands with okay-codec and will plug in here.
 */
object SparkInterop {

  /** run an okay Aggregator over an RDD, distributed */
  def aggregate[In, Acc, Out](rdd: RDD[In])(agg: Aggregator[In, Acc, Out])
                             (using scala.reflect.ClassTag[Acc]): Out =
    agg.present(rdd.aggregate(agg.init)(agg.add, agg.merge))

  /** the same aggregator per key */
  def aggregateByKey[K, In, Acc, Out](rdd: RDD[(K, In)])(agg: Aggregator[In, Acc, Out])
                                     (using scala.reflect.ClassTag[K],
                                      scala.reflect.ClassTag[In],
                                      scala.reflect.ClassTag[Acc]): Map[K, Out] =
    org.apache.spark.rdd.RDD.rddToPairRDDFunctions(rdd)
      .aggregateByKey(agg.init)(agg.add, agg.merge)
      .collect().map((k, acc) => (k, agg.present(acc))).toMap

  /** an okay Aggregator as a Dataset-side (typed-column) aggregator */
  def toSpark[In, Acc, Out](agg: Aggregator[In, Acc, Out])
                           (using accE: Encoder[Acc], outE: Encoder[Out])
  : org.apache.spark.sql.expressions.Aggregator[In, Acc, Out] =
    new org.apache.spark.sql.expressions.Aggregator[In, Acc, Out]:
      def zero: Acc = agg.init
      def reduce(b: Acc, a: In): Acc = agg.add(b, a)
      def merge(b1: Acc, b2: Acc): Acc = agg.merge(b1, b2)
      def finish(b: Acc): Out = agg.present(b)
      def bufferEncoder: Encoder[Acc] = accE
      def outputEncoder: Encoder[Out] = outE

  /** aggregate a whole typed Dataset with an okay Aggregator */
  def aggregate[In, Acc, Out](ds: Dataset[In])(agg: Aggregator[In, Acc, Out])
                             (using Encoder[Acc], Encoder[Out]): Out =
    ds.select(toSpark(agg).toColumn).collect().head
}

/**
 * The `Bulk` seam on Spark (specs/bulk.md): one `RDD[Any]` under an
 * opaque wrapper, so a program written against `Bulk[D]` needs no
 * `ClassTag` per intermediate type — the evidence Spark would
 * otherwise ask for at every `map`. Every element IS an object on
 * Spark's side; this says so in the type and pays for it once, in
 * `elem`, the one cast (the `Refs.slot` precedent: the wrapper is the
 * proof, the cast is erased).
 *
 * What the `Any` costs and does not: a `ClassTag[Long]` would have let
 * `collect` build a `long[]`; the seam never asks for an array — it
 * leaves through `toLocalIterator`, one partition at a time — so the
 * only difference is a boxed element where Spark boxes it anyway.
 */
object SparkBulk:
  opaque type Rows[A] = RDD[Any]

  private given scala.reflect.ClassTag[Any] = scala.reflect.ClassTag.Any

  /** the one cast: an element of `Rows[A]` is an `A` by construction */
  private inline def elem[A](x: Any): A = x.asInstanceOf[A]

  def apply(spark: SparkSession): okay.Bulk[Rows] = new okay.Bulk[Rows]:
    def of[A](xs: Iterable[A]): Rows[A] = spark.sparkContext.parallelize(xs.toSeq)

    /** Spark's own CSV reader, the header as names; a BOM on the first
     * column is stripped, an absent value is the empty string */
    def csv(path: String): Rows[okay.Csv.Row] =
      val df = spark.read.option("header", "true").csv(path)
      val names = df.columns.map(_.stripPrefix("﻿")).toVector
      df.rdd.map(r => names.iterator.zip(r.toSeq.iterator.map(v => if v == null then "" else v.toString)).toMap)

    def map[A, B](d: Rows[A])(f: A => B): Rows[B] = d.map(x => f(elem[A](x)))
    def flatMap[A, B](d: Rows[A])(f: A => IterableOnce[B]): Rows[B] = d.flatMap(x => f(elem[A](x)))
    def filter[A](d: Rows[A])(p: A => Boolean): Rows[A] = d.filter(x => p(elem[A](x)))

    def join[K, A, B](l: Rows[(K, A)], r: Rows[(K, B)]): Rows[(K, (A, B))] =
      val lp: RDD[(Any, Any)] = l.map(x => elem[(Any, Any)](x))
      val rp: RDD[(Any, Any)] = r.map(x => elem[(Any, Any)](x))
      RDD.rddToPairRDDFunctions(lp).join(rp).map(x => x)

    def cache[A](d: Rows[A]): Rows[A] = d.persist(org.apache.spark.storage.StorageLevel.MEMORY_AND_DISK)

    def aggregate[A, Acc, Out](d: Rows[A])(agg: Aggregator[A, Acc, Out]): Out =
      val acc: Any = d.aggregate[Any](agg.init)(
        (acc, x) => agg.add(elem[Acc](acc), elem[A](x)),
        (a, b) => agg.merge(elem[Acc](a), elem[Acc](b)))
      agg.present(elem[Acc](acc))

    def toChunks[A](d: Rows[A]): okay.Chunks[A] =
      okay.Chunks.fromIteratorWith(d.toLocalIterator.map(elem[A]))(okay.ChunkBuf.factory[A](64))(64)

  /**
   * `Sort` answered NATIVELY (specs/bulk.md, the effect layer): Spark's
   * own sort over the same heap `Tables.via` threads. Nothing in the
   * `Tables` handler knows this exists — both translate into the same
   * `State % Heap[Rows]`, which is how an operation joins a platform
   * without joining the platform's contract. Keys travel as `Any` like
   * elements do, and come back through the same `elem`.
   */
  def sort[A, F[+_]](p: A ! (okay.Sort + F)): A ! (okay.State % okay.Tables.Heap[Rows] + F) =
    import okay.RowLift.plus
    def sorted[X, K](h: okay.Tables.Heap[Rows], t: okay.Tables.Table[X], key: X => K, ord: Ordering[K])
    : (okay.Tables.Table[X], okay.Tables.Heap[Rows]) =
      val keyed: RDD[(Any, Any)] = h.get(t).map(x => (key(elem[X](x)): Any, x))
      val byKey = Ordering.fromLessThan[Any]((a, b) => ord.lt(elem[K](a), elem[K](b)))
      h.put[X](RDD.rddToOrderedRDDFunctions(keyed)(using byKey, scala.reflect.ClassTag.Any, scala.reflect.ClassTag.Any)
        .sortByKey().values)
    okay.!.interpret(p):
      [X] => (e: okay.Sort[X]) => e match
        case okay.Sort.By(t, key, ord) => okay.State.update[okay.Tables.Heap[Rows], X](h => sorted(h, t, key, ord)).plus[F]
