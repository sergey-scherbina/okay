package okay.spark

import okay.Aggregator
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{Dataset, Encoder}

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
