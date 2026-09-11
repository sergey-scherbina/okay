package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema
import scala.collection.mutable

/**
 * A `Sink` WHOSE PARTIAL CAN CROSS A PROCESS BOUNDARY
 * (specs/dataflow.md, stage 4).
 *
 * The whole addition is one member: a `Schema` for `W`, what leaves a
 * partition. Everything else a distributed run needs — how a
 * partition accumulates, when it may finish a pane by itself, how
 * partials merge — is already the `Sink`, and none of it changes
 * because the partial travelled.
 *
 * WHY A SCHEMA AND NOT SERIALIZATION. Claim 3 of the spec: nothing
 * ships a closure, and no reflective serializer is involved. What
 * crosses is described by a value the author supplies, so a partial
 * that cannot cross is a compile error at the point the sink is
 * built, not a `NotSerializableException` inside a task on another
 * machine.
 *
 * WHAT ACTUALLY CROSSES IS SMALL, and that is the completeness rule
 * paying twice. A partition has already folded every pane it could
 * finish alone into its terminal accumulator, so it hands over ONE
 * value for all of them plus the boundary panes. On the Wrocław job
 * that is one summary standing for 1.7 million panes, and about a
 * hundred thousand accumulators beside it.
 *
 * `and` composes two Wires into one, exactly as `Sink.and` does, and
 * the pair's Schema with them.
 */
abstract class Wire[A, R] extends Sink[A, R]:

  /** how `W` — and only `W` — travels */
  def wire: Schema[W]

  /**
   * HOW THE COORDINATOR'S OWN STATE TRAVELS (specs/dataflow.md,
   * stage 8) — the same idea as `wire`, one level up.
   *
   * `W` is what leaves a PARTITION; `S` is what the coordinator has
   * folded so far, and until stage 8 nothing ever asked it to be a
   * value. A journalled coordinator does: its death is only
   * survivable if what it holds can be written down.
   *
   * Every `S` in `Sink` is a value in disguise — a fold's is its
   * accumulator, a keyed sink's is a map of accumulators, a windowed
   * sink's is the open panes plus what has been retired — so this
   * member costs one `SIso` each and no change to how anything runs.
   */
  def state: Schema[S]

  /** two wired sinks over one pass, still wired */
  final def and[R2](that: Wire[A, R2]): Wire[A, (R, R2)] =
    val self = this
    val n = self.times.length
    new Wire[A, (R, R2)]:
      type P = (self.P, that.P)
      type W = (self.W, that.W)
      type S = (self.S, that.S)
      def wire: Schema[W] = Wire.pair(self.wire, that.wire)
      def state: Schema[S] = Wire.pair(self.state, that.state)
      def empty: S = (self.empty, that.empty)
      def absorb(st: S, ws: Vector[W], watermark: Long): S =
        (self.absorb(st._1, ws.map(_._1), watermark),
         that.absorb(st._2, ws.map(_._2), watermark))
      def emit(st: S): (R, R2) = (self.emit(st._1), that.emit(st._2))
      def slack: Long = math.max(self.slack, that.slack)
      def times: Vector[A => Long] = self.times ++ that.times
      def start(bounds: Vector[Bounds]): P =
        (self.start(bounds.take(n)), that.start(bounds.drop(n)))
      def step(p: P, a: A): Unit = { self.step(p._1, a); that.step(p._2, a) }
      def finish(p: P): W = (self.finish(p._1), that.finish(p._2))
      def peek(p: P): W = (self.peek(p._1), that.peek(p._2))
      def drops(ws: Vector[W]): Long =
        self.drops(ws.map(_._1)) + that.drops(ws.map(_._2))
      def merged(ws: Vector[W]): Long =
        self.merged(ws.map(_._1)) + that.merged(ws.map(_._2))
      override def committed(epoch: Int): Unit =
        { self.committed(epoch); that.committed(epoch) }
      override def recovered(epoch: Int): Unit =
        { self.recovered(epoch); that.recovered(epoch) }

object Wire {

  /** no key and no window — the terminal's accumulator is the partial */
  def fold[A, Acc, R](into: Aggregator[A, Acc, R])(using s: Schema[Acc]): Wire[A, R] =
    val local = Sink.fold(into)
    new Wire[A, R]:
      type P = local.P
      type W = local.W
      type S = local.S
      def wire: Schema[W] = s
      // a fold has nothing open: the coordinator's state IS the
      // accumulator, and the Schema for it is the one already given
      def state: Schema[S] = s
      def empty: S = local.empty
      def absorb(st: S, ws: Vector[W], watermark: Long): S = local.absorb(st, ws, watermark)
      def emit(st: S): R = local.emit(st)
      def slack: Long = local.slack
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def peek(p: P): W = local.peek(p)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)
      // the two moments reach the SINK this wire wraps — a writing
      // sink behind a Wire must hear them (specs/dataflow.md, stage 9)
      override def committed(epoch: Int): Unit = local.committed(epoch)
      override def recovered(epoch: Int): Unit = local.recovered(epoch)

  /** one accumulator per key: the partial is the key/accumulator pairs */
  def keyed[A, K, Acc, O, IAcc, R](key: A => K, agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[(K, O), IAcc, R])
                                  (using sk: Schema[K], sa: Schema[Acc]): Wire[A, R] =
    val local = Sink.keyed(key, agg)(into)
    new Wire[A, R]:
      type P = local.P
      type W = local.W
      type S = local.S
      def wire: Schema[W] = Schema.SVector(() => pair(sk, sa))
      def state: Schema[S] = keys(sk, sa)
      def empty: S = local.empty
      def absorb(st: S, ws: Vector[W], watermark: Long): S = local.absorb(st, ws, watermark)
      def emit(st: S): R = local.emit(st)
      def slack: Long = local.slack
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def peek(p: P): W = local.peek(p)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)
      // the two moments reach the SINK this wire wraps — a writing
      // sink behind a Wire must hear them (specs/dataflow.md, stage 9)
      override def committed(epoch: Int): Unit = local.committed(epoch)
      override def recovered(epoch: Int): Unit = local.recovered(epoch)

  /**
   * An event-time windowed aggregation.
   *
   * Three Schemas, and each one is on the smallest thing it could be:
   * the KEY and the pane ACCUMULATOR for the boundary panes, and the
   * TERMINAL's accumulator for the single value standing in for every
   * pane this partition finished by itself.
   */
  def windowed[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean)
                                     (into: Aggregator[Pane[K, O], IAcc, R])
                                     (using sk: Schema[K], sa: Schema[Acc],
                                      si: Schema[IAcc]): Wire[A, R] =
    val local = Sink.windowed(size, slide, lateness, key, at, agg, seeded)(into)
    new Wire[A, R]:
      type P = local.P
      type W = local.W
      type S = local.S
      def wire: Schema[W] = handed(sk, sa, si)
      def state: Schema[S] = open(sk, sa, si)
      def empty: S = local.empty
      def absorb(st: S, ws: Vector[W], watermark: Long): S = local.absorb(st, ws, watermark)
      def emit(st: S): R = local.emit(st)
      def slack: Long = local.slack
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def peek(p: P): W = local.peek(p)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)
      // the two moments reach the SINK this wire wraps — a writing
      // sink behind a Wire must hear them (specs/dataflow.md, stage 9)
      override def committed(epoch: Int): Unit = local.committed(epoch)
      override def recovered(epoch: Int): Unit = local.recovered(epoch)

  def tumbling[A, K, Acc, O, IAcc, R](size: Long, lateness: Long,
                                      key: A => K, at: A => Long,
                                      agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                     (into: Aggregator[Pane[K, O], IAcc, R])
                                     (using Schema[K], Schema[Acc], Schema[IAcc]): Wire[A, R] =
    windowed(size, size, lateness, key, at, agg, seeded)(into)

  def sliding[A, K, Acc, O, IAcc, R](size: Long, slide: Long, lateness: Long,
                                     key: A => K, at: A => Long,
                                     agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                                    (into: Aggregator[Pane[K, O], IAcc, R])
                                    (using Schema[K], Schema[Acc], Schema[IAcc]): Wire[A, R] =
    windowed(size, slide, lateness, key, at, agg, seeded)(into)

  /**
   * A WINDOWED WIRE WHOSE PANES ARE WRITTEN OUT, AT A DISTANCE
   * (specs/dataflow.md, stage 6c).
   *
   * The promise and its two qualifiers are `Sink.writing`'s, and the
   * distance is what makes them worth stating: the writer runs WHERE
   * THE PANE IS RETIRED, which is not one place. A pane the
   * completeness rule let a partition finish alone is written on that
   * WORKER; a boundary pane is written on the COORDINATOR when the
   * watermark passes it. So a `write` here is a closure over whatever
   * the worker process can reach — a table, a topic, a file — and
   * emphatically not over the submitting process's memory.
   *
   * Nothing about that is new machinery: `Job` has always built its
   * sink from parameters ON the worker, so the writer is constructed
   * there like everything else and no closure crosses the wire.
   */
  def writing[A, K, Acc, O](size: Long, slide: Long, lateness: Long,
                            key: A => K, at: A => Long,
                            agg: Aggregator[A, Acc, O], seeded: Boolean)
                           (write: Pane[K, O] => Unit)
                           (using Schema[K], Schema[Acc]): Wire[A, Long] =
    windowed(size, slide, lateness, key, at, agg, seeded)(Sink.writes(write))

  def tumblingTo[A, K, Acc, O](size: Long, lateness: Long,
                               key: A => K, at: A => Long,
                               agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                              (write: Pane[K, O] => Unit)
                              (using Schema[K], Schema[Acc]): Wire[A, Long] =
    writing(size, size, lateness, key, at, agg, seeded)(write)

  def slidingTo[A, K, Acc, O](size: Long, slide: Long, lateness: Long,
                              key: A => K, at: A => Long,
                              agg: Aggregator[A, Acc, O], seeded: Boolean = true)
                             (write: Pane[K, O] => Unit)
                             (using Schema[K], Schema[Acc]): Wire[A, Long] =
    writing(size, slide, lateness, key, at, agg, seeded)(write)

  /**
   * A STAGING SINK AT A DISTANCE (specs/dataflow.md, stage 9).
   *
   * The contract is `Sink.staging`'s and so is the refusal: it
   * belongs to `Cluster.stream`, where every pane retires at the
   * coordinator, and a batch `Cluster.run` finishes panes on the
   * WORKERS, where `finish` will refuse to hand over a partition
   * holding staged panes rather than dropping them.
   *
   * `move` therefore runs in the coordinator's process — which is the
   * one place that knows an epoch is final — and, like every other
   * writer here, it is built from the job's parameters on whichever
   * machine is running the coordinator. No closure crosses.
   */
  def staging[A, K, Acc, O](size: Long, slide: Long, lateness: Long,
                            key: A => K, at: A => Long,
                            agg: Aggregator[A, Acc, O])
                           (move: (Int, Vector[Pane[K, O]]) => Unit)
                           (using sk: Schema[K], sa: Schema[Acc]): Wire[A, Long] =
    val local = Sink.staging(size, slide, lateness, key, at, agg)(move)
    new Wire[A, Long]:
      type P = local.P
      type W = local.W
      type S = local.S
      def wire: Schema[W] = handed(sk, sa, Schema.SLong)
      def state: Schema[S] = open(sk, sa, Schema.SLong)
      def empty: S = local.empty
      def absorb(st: S, ws: Vector[W], watermark: Long): S = local.absorb(st, ws, watermark)
      def emit(st: S): Long = local.emit(st)
      def slack: Long = local.slack
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def peek(p: P): W = local.peek(p)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)
      override def committed(epoch: Int): Unit = local.committed(epoch)
      override def recovered(epoch: Int): Unit = local.recovered(epoch)

  def tumblingStaged[A, K, Acc, O](size: Long, lateness: Long,
                                   key: A => K, at: A => Long,
                                   agg: Aggregator[A, Acc, O])
                                  (move: (Int, Vector[Pane[K, O]]) => Unit)
                                  (using Schema[K], Schema[Acc]): Wire[A, Long] =
    staging(size, size, lateness, key, at, agg)(move)

  // -----------------------------------------------------------------
  // Schemas built from Schema VALUES
  //
  // `Schema.derived` cannot serve here: it summons its fields'
  // instances at the CALL SITE, and a Wire has its halves as values —
  // which is the whole point, since a partial's Schema has to travel
  // with the sink that produced it rather than be found again wherever
  // the sink is used.
  //
  // `SProduct.make` is typed `Seq[Any] => A`, so every instance of it,
  // including the one the compiler derives, converts positionally.
  // The casts below are that signature and nothing more: the i-th
  // element IS the i-th field's type, because `parts` — three lines
  // down — is what produced it.
  // -----------------------------------------------------------------

  private[cluster] def pair[X, Y](x: Schema[X], y: Schema[Y]): Schema[(X, Y)] =
    Schema.SProduct[(X, Y)]("Pair",
      Vector("_1" -> (() => x), "_2" -> (() => y)),
      vs => (vs(0).asInstanceOf[X], vs(1).asInstanceOf[Y]),
      p => Seq(p._1, p._2))

  private[cluster] def triple[X, Y, Z](x: Schema[X], y: Schema[Y], z: Schema[Z])
  : Schema[(X, Y, Z)] =
    Schema.SProduct[(X, Y, Z)]("Triple",
      Vector("_1" -> (() => x), "_2" -> (() => y), "_3" -> (() => z)),
      vs => (vs(0).asInstanceOf[X], vs(1).asInstanceOf[Y], vs(2).asInstanceOf[Z]),
      p => Seq(p._1, p._2, p._3))

  /**
   * THE COORDINATOR'S STATE, AS A VALUE (specs/dataflow.md, stage 8).
   *
   * Both of these are `SIso` — the codec's newtype node — and not a
   * hand-written product, because what has to travel is a MUTABLE
   * map: the coordinator updates one key per boundary pane per epoch,
   * so its state wants to be a `HashMap` and its checkpoint wants to
   * be a vector. `SIso` says exactly that and nothing else in the
   * engine knows the difference. No casts: `to` and `from` are
   * ordinary total functions.
   */
  private[cluster] def keys[K, Acc](sk: Schema[K], sa: Schema[Acc])
  : Schema[mutable.HashMap[K, Acc]] =
    Schema.SIso[mutable.HashMap[K, Acc], Vector[(K, Acc)]](
      () => Schema.SVector(() => pair(sk, sa)),
      v => Right(mutable.HashMap.from(v)),
      m => m.toVector)()

  /** the open panes, and everything already retired */
  private[cluster] def open[K, Acc, IAcc](sk: Schema[K], sa: Schema[Acc], si: Schema[IAcc])
  : Schema[Sink.Open[K, Acc, IAcc]] =
    Schema.SIso[Sink.Open[K, Acc, IAcc], (Vector[(Long, K, Acc)], IAcc)](
      () => pair(Schema.SVector(() => triple(Schema.SLong, sk, sa)), si),
      { case (panes, acc) => Right(Sink.Open(
        mutable.HashMap.from(panes.map { case (start, k, a) => ((start, k), a) }), acc)) },
      o => (o.panes.toVector.map { case ((start, k), a) => (start, k, a) }, o.acc))()

  private[cluster] def handed[K, Acc, IAcc](sk: Schema[K], sa: Schema[Acc], si: Schema[IAcc])
  : Schema[Sink.Handed[K, Acc, IAcc]] =
    Schema.SProduct[Sink.Handed[K, Acc, IAcc]]("Handed",
      Vector(
        "boundary" -> (() => Schema.SVector(() => triple(Schema.SLong, sk, sa))),
        "finished" -> (() => si),
        "late" -> (() => Schema.SLong)),
      vs => Sink.Handed(
        vs(0).asInstanceOf[Vector[(Long, K, Acc)]],
        vs(1).asInstanceOf[IAcc],
        vs(2).asInstanceOf[Long]),
      h => Seq(h.boundary, h.finished, h.late))
}
