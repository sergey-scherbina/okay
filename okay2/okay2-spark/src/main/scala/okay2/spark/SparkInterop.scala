package okay2.spark

import okay2._
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, Encoder}

/**
 * Spark via the P1 contract (the Scala 3 core's `okay.spark.SparkInterop`,
 * specs/external-systems.md): an okay `Aggregator`'s (init, add, merge)
 * IS Spark's (zero, seqOp, combOp) — one definition, local `Chunks`
 * execution or cluster execution, equal results. `Aggregator` extends
 * `Serializable` in the core precisely so these closures can ship as
 * Spark tasks.
 *
 * NO CROSS-VERSION SHIM: this whole build is already Scala 2.13, the
 * same Scala Spark itself publishes for — the Scala 3 twin needs
 * `CrossVersion.for3Use2_13`, an explicit `scala-reflect` pin, and a
 * documented `ArraySeq` two-stdlib serialization trap
 * (`Distributed.feed`, docs/benchmarks.md's cluster-mode section);
 * none of that exists on this side, by construction.
 */
object SparkInterop {

  /** run an okay Aggregator over an RDD, distributed */
  def aggregate[In, Acc, Out](rdd: RDD[In])(agg: Aggregator[In, Acc, Out])
                             (implicit ct: scala.reflect.ClassTag[Acc]): Out =
    agg.present(rdd.aggregate(agg.init)(agg.add, agg.merge))

  /** the same aggregator per key */
  def aggregateByKey[K, In, Acc, Out](rdd: RDD[(K, In)])(agg: Aggregator[In, Acc, Out])
                                     (implicit ctK: scala.reflect.ClassTag[K],
                                      ctIn: scala.reflect.ClassTag[In],
                                      ctAcc: scala.reflect.ClassTag[Acc]): Map[K, Out] =
    org.apache.spark.rdd.RDD.rddToPairRDDFunctions(rdd)
      .aggregateByKey(agg.init)(agg.add, agg.merge)
      .collect().map { case (k, acc) => (k, agg.present(acc)) }.toMap

  /** an okay Aggregator as a Dataset-side (typed-column) aggregator */
  def toSpark[In, Acc, Out](agg: Aggregator[In, Acc, Out])
                           (implicit accE: Encoder[Acc], outE: Encoder[Out])
  : org.apache.spark.sql.expressions.Aggregator[In, Acc, Out] =
    new org.apache.spark.sql.expressions.Aggregator[In, Acc, Out] {
      def zero: Acc = agg.init
      def reduce(b: Acc, a: In): Acc = agg.add(b, a)
      def merge(b1: Acc, b2: Acc): Acc = agg.merge(b1, b2)
      def finish(b: Acc): Out = agg.present(b)
      def bufferEncoder: Encoder[Acc] = accE
      def outputEncoder: Encoder[Out] = outE
    }

  /** aggregate a whole typed Dataset with an okay Aggregator */
  def aggregate[In, Acc, Out](ds: Dataset[In])(agg: Aggregator[In, Acc, Out])
                             (implicit accE: Encoder[Acc], outE: Encoder[Out]): Out =
    ds.select(toSpark(agg).toColumn).collect().head
}
