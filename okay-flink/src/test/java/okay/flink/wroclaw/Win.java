package okay.flink.wroclaw;

/**
 * One closed window: what the shared aggregator said about it, plus
 * the window's own start and key. A POJO because it crosses the
 * second keyBy — the ranking stage of the job.
 */
public class Win {
    /** window start, epoch millis */
    public long win;
    /** the route or the stop, whichever this window was keyed by */
    public int key;
    public long n;
    public long sum;
    public int max;
    public boolean tram;

    public Win() {}

    public Win(long win, int key, long n, long sum, int max, boolean tram) {
        this.win = win;
        this.key = key;
        this.n = n;
        this.sum = sum;
        this.max = max;
        this.tram = tram;
    }
}
