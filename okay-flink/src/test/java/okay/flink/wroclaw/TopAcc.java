package okay.flink.wroclaw;

/**
 * The ranking's accumulator, as a Flink user writes one: the five
 * slots as arrays of primitives, kept sorted, with no collection and
 * no boxing in it.
 *
 * The okay lane says this with {@code Aggregator.topK(5)}, whose
 * accumulator is a {@code List[(Int, Job.Stats)]} — and handing THAT
 * to Flink means Kryo serializes a linked list of tuples of case
 * classes on every window merge. This is four primitive arrays of
 * five, which Flink serializes natively.
 *
 * The order is the job's own: greatest mean first, ties broken by the
 * smaller route index — a total order, so this ranking and okay's are
 * the same list, and the benchmark asserts the hash of it.
 */
public class TopAcc {
    /** how many of the five slots are filled */
    public int size;
    public int[] route = new int[5];
    public long[] n = new long[5];
    public long[] sum = new long[5];
    public int[] max = new int[5];

    public TopAcc() {}

    /** the mean of slot i — the ranking's key */
    public double meanAt(int i) {
        return n[i] == 0L ? 0.0 : (double) sum[i] / (double) n[i];
    }

    /** is (mean, route) greater than slot i, in the job's total order? */
    public boolean greaterThan(int i, double mean, int r) {
        double m = meanAt(i);
        if (mean != m) return mean > m;
        return r < route[i];
    }

    /** insert one route's statistic, keeping the five greatest */
    public void offer(int r, long count, long total, int mx) {
        double mean = count == 0L ? 0.0 : (double) total / (double) count;
        int at = size;
        while (at > 0 && greaterThan(at - 1, mean, r)) at--;
        if (at >= 5) return;                       // smaller than all five
        int last = Math.min(size, 4);              // where the shift stops
        for (int i = last; i > at; i--) {
            route[i] = route[i - 1];
            n[i] = n[i - 1];
            sum[i] = sum[i - 1];
            max[i] = max[i - 1];
        }
        route[at] = r;
        n[at] = count;
        sum[at] = total;
        max[at] = mx;
        if (size < 5) size++;
    }
}
