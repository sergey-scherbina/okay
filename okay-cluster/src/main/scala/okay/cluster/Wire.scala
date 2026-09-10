package okay.cluster

import okay.{Aggregator, Pane}
import okay.codec.Schema

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

  /** two wired sinks over one pass, still wired */
  final def and[R2](that: Wire[A, R2]): Wire[A, (R, R2)] =
    val self = this
    val n = self.times.length
    new Wire[A, (R, R2)]:
      type P = (self.P, that.P)
      type W = (self.W, that.W)
      def wire: Schema[W] = Wire.pair(self.wire, that.wire)
      def times: Vector[A => Long] = self.times ++ that.times
      def start(bounds: Vector[Bounds]): P =
        (self.start(bounds.take(n)), that.start(bounds.drop(n)))
      def step(p: P, a: A): Unit = { self.step(p._1, a); that.step(p._2, a) }
      def finish(p: P): W = (self.finish(p._1), that.finish(p._2))
      def result(ws: Vector[W]): (R, R2) =
        (self.result(ws.map(_._1)), that.result(ws.map(_._2)))
      def drops(ws: Vector[W]): Long =
        self.drops(ws.map(_._1)) + that.drops(ws.map(_._2))
      def merged(ws: Vector[W]): Long =
        self.merged(ws.map(_._1)) + that.merged(ws.map(_._2))

object Wire {

  /** no key and no window — the terminal's accumulator is the partial */
  def fold[A, Acc, R](into: Aggregator[A, Acc, R])(using s: Schema[Acc]): Wire[A, R] =
    val local = Sink.fold(into)
    new Wire[A, R]:
      type P = local.P
      type W = local.W
      def wire: Schema[W] = s
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def result(ws: Vector[W]): R = local.result(ws)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)

  /** one accumulator per key: the partial is the key/accumulator pairs */
  def keyed[A, K, Acc, O, IAcc, R](key: A => K, agg: Aggregator[A, Acc, O])
                                  (into: Aggregator[(K, O), IAcc, R])
                                  (using sk: Schema[K], sa: Schema[Acc]): Wire[A, R] =
    val local = Sink.keyed(key, agg)(into)
    new Wire[A, R]:
      type P = local.P
      type W = local.W
      def wire: Schema[W] = Schema.SVector(() => pair(sk, sa))
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def result(ws: Vector[W]): R = local.result(ws)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)

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
      def wire: Schema[W] = handed(sk, sa, si)
      def times: Vector[A => Long] = local.times
      def start(bounds: Vector[Bounds]): P = local.start(bounds)
      def step(p: P, a: A): Unit = local.step(p, a)
      def finish(p: P): W = local.finish(p)
      def result(ws: Vector[W]): R = local.result(ws)
      def drops(ws: Vector[W]): Long = local.drops(ws)
      def merged(ws: Vector[W]): Long = local.merged(ws)

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
