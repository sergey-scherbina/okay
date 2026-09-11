package okay.wroclaw;

/**
 * A departure after the enrichment stage: the same event, plus what
 * the routes table knows about its route. A POJO for the same reason
 * {@link Depart} is one — it crosses a keyBy in the Flink lane, and
 * the element that crosses a shuffle decides the serializer.
 */
public class Ride implements java.io.Serializable {
    public long ts;
    public int route;
    public int stop;
    public int vehicle;
    public int delay;
    /** from the routes table: route_type2_id 31 */
    public boolean tram;

    public Ride() {}

    public Ride(long ts, int route, int stop, int vehicle, int delay, boolean tram) {
        this.ts = ts;
        this.route = route;
        this.stop = stop;
        this.vehicle = vehicle;
        this.delay = delay;
        this.tram = tram;
    }
}
