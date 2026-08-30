package okay.flink

import okay.Aggregator
import org.apache.flink.api.common.functions.AggregateFunction

/**
 * Flink via the P1 contract (specs/external-systems.md): an okay
 * Aggregator's (init, add, merge, present) IS Flink's
 * AggregateFunction (createAccumulator, add, merge, getResult) —
 * field for field. One definition: local Chunks, Spark, or a Flink
 * window. Serializability comes from the core (Aggregator extends
 * Serializable).
 */
object FlinkInterop {

  /** an okay Aggregator as a Flink AggregateFunction */
  def toFlink[In, Acc, Out](agg: Aggregator[In, Acc, Out]): AggregateFunction[In, Acc, Out] =
    new AggregateFunction[In, Acc, Out]:
      def createAccumulator(): Acc = agg.init
      def add(value: In, accumulator: Acc): Acc = agg.add(accumulator, value)
      def getResult(accumulator: Acc): Out = agg.present(accumulator)
      def merge(a: Acc, b: Acc): Acc = agg.merge(a, b)
}
