package okay.flink.wroclaw;

/**
 * The accumulator of the windowed statistic, as a Flink user writes
 * one: a POJO of three primitive fields, mutated in place.
 *
 * WHY THIS EXISTS AT ALL (bench-engine-native-arithmetic). This lane
 * used to accumulate through {@code FlinkInterop.toFlink(Job.stats)} —
 * the very okay {@code Aggregator} the in-process lane folds with —
 * and that is a fine claim about the INTEROP but a poor benchmark
 * row: it hands Flink an accumulator of
 * {@code ((Long, Long), Option[Int])}, a Scala tuple tree Flink's type
 * extractor cannot read, so its window STATE was serialized by Kryo.
 * A benchmark that quietly picks the competitor's slowest serializer
 * is not measuring the competitor.
 *
 * A POJO — public class, public no-arg constructor, public fields —
 * is what Flink's extractor is looking for, and it is why
 * {@link Depart} and {@link Ride} are Java too. Flink's
 * {@code AggregateFunction} explicitly permits mutating the
 * accumulator in {@code add}, so this is not a shortcut around the
 * contract: it IS the contract on that side of the seam.
 *
 * {@code okay.flink.FlinkInterop.toFlink} keeps its own test, where
 * "one aggregator answers on every engine" is a claim about the
 * interop rather than a row in a table.
 */
public class StatsAcc {
    public long n;
    public long sum;
    public int max;

    public StatsAcc() {
        this.n = 0L;
        this.sum = 0L;
        this.max = Integer.MIN_VALUE;
    }
}
