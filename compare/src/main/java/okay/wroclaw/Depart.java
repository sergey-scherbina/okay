package okay.wroclaw;

/**
 * One scheduled departure that actually happened: a vehicle leaving a
 * stop at an event time, some seconds off its timetable.
 *
 * WHY THIS IS JAVA, AND A POJO. It is the element BOTH lanes carry,
 * and its shape decides Flink's serializer. A Scala case class has
 * final fields, no no-arg constructor and no get/set pair, so Flink's
 * type extraction cannot see a POJO in it and falls back to Kryo —
 * which at parallelism &gt; 1 is a handicap the benchmark would have
 * imposed on Flink rather than measured. Public fields plus a no-arg
 * constructor is what a Flink user writes; the okay lane carries the
 * very same objects, so neither side is favoured by the choice.
 */
public class Depart implements java.io.Serializable {
    /** event time: the moment the vehicle left, epoch millis */
    public long ts;
    /** route index into the routes table (the enrichment's key) */
    public int route;
    /** stop id, as published */
    public int stop;
    /** which run of the route this is — one trip is one vehicle */
    public int vehicle;
    /** seconds off the timetable; negative is early */
    public int delay;

    public Depart() {}

    public Depart(long ts, int route, int stop, int vehicle, int delay) {
        this.ts = ts;
        this.route = route;
        this.stop = stop;
        this.vehicle = vehicle;
        this.delay = delay;
    }
}
