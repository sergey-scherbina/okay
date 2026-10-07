package okay


import okay.freer.*


import okay.std.*
import scala.annotation.tailrec
import scala.collection.mutable

/**
 * `Bulk.local`, PARALLEL IN ONE PROCESS (bulk-local-parallel, specs/bulk.md):
 * the same `Bulk[Chunks]` — a program that names no platform runs on it
 * unchanged — with the three operations that parallelise for real spread
 * over `parallelism` fibres of the given `Scheduler`. On JVM and Native,
 * the platforms with threads; JS has one thread and no such instance,
 * which is said by its absence rather than by a parameter that does
 * nothing there.
 *
 * - `read(path, format)`: a file's SPLITS (a Parquet file's row groups)
 *   read on `parallelism` fibres ahead of the consumer, each split one
 *   chunk, emitted in split order — decoding is the work, and the splits
 *   are independent.
 * - `aggregate`: the chunks pulled in BATCHES of `batch`, each batch folded
 *   on a fibre from `agg.init`, `parallelism` batches in flight, the
 *   partials merged in INPUT ORDER as they are joined — so an associative
 *   but non-commutative `Sequential` is as right here as in one thread.
 * - `join`: the right side folded into partial hash maps on fibres and
 *   merged; the left side streamed through a window of `parallelism`
 *   chunks joined on fibres, output in the left side's order.
 *
 * Everything else — `of`, `csv`, `map`, `flatMap`, `filter`, `cache`,
 * `toChunks` — is `Bulk.local`'s, lazy and fused, as before: a CSV's
 * bottleneck is its one line iterator, and a fibre per chunk behind it
 * buys nothing. `Bulk.local` itself is unchanged.
 */
object BulkParallel:
  /** chunks per `aggregate` task: a 64-element chunk is too small a task
   * to be worth a fibre on its own */
  val batch: Int = 16

  def apply(parallelism: Int = Runtime.getRuntime.availableProcessors(),
            lines: String => Iterator[String] = _ => Iterator.empty,
            bytes: String => Option[Long] = _ => None)(using S: Scheduler): Bulk[Chunks] =
    require(parallelism >= 1, "a parallel Bulk has at least one fibre")
    val base = Bulk.local(lines, bytes)
    new Bulk[Chunks]:
      def of[A](xs: Iterable[A]): Chunks[A] = base.of(xs)
      def csv(path: String): Chunks[Csv.Row] = base.csv(path)
      override def csv(path: String, columns: Option[Set[String]]): Chunks[Csv.Row] = base.csv(path, columns)
      override def size(path: String): Option[Long] = base.size(path)
      def map[A, B](d: Chunks[A])(f: A => B): Chunks[B] = base.map(d)(f)
      def flatMap[A, B](d: Chunks[A])(f: A => IterableOnce[B]): Chunks[B] = base.flatMap(d)(f)
      def filter[A](d: Chunks[A])(p: A => Boolean): Chunks[A] = base.filter(d)(p)
      def cache[A](d: Chunks[A]): Chunks[A] = base.cache(d)
      def toChunks[A](d: Chunks[A]): Chunks[A] = d
      override def joinSorted[K, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)])(using Ordering[K]): Chunks[(K, (A, B))] =
        base.joinSorted(l, r)

      /** the splits on fibres, `parallelism` ahead, in split order */
      override def read[A](path: String, format: Bulk.Format[A]): Chunks[A] = Chunks.defer:
        val splits = format.splits(path)
        def go(inflight: Vector[Fiber[Chunk[A]]], next: Int): Chunks[A] = Chunks.defer:
          var q = inflight
          var i = next
          while q.length < parallelism && i < splits.length do
            val s = splits(i)
            q = q :+ S.fork(() => async(ChunkBuf.of(format.read(path, s))))
            i += 1
          q match
            case h +: t => Writer.tell(h.join()).flatMap(_ => go(t, i))
            case _ => Chunks.end
        go(Vector.empty, 0)

      def aggregate[A, Acc, Out](d: Chunks[A])(agg: Aggregator[A, Acc, Out]): Out =
        def fold(cs: Vector[Chunk[A]]): Acc =
          var acc = agg.init
          for c <- cs do
            var i = 0
            while i < c.length do { acc = agg.add(acc, c(i)); i += 1 }
          acc
        /** up to `batch` chunks off the front */
        @tailrec def take(r: Chunks[A], got: Vector[Chunk[A]]): (Vector[Chunk[A]], Chunks[A] | Null) =
          if got.length >= batch then (got, r)
          else Chunks.pull(r) match
            case Some((c, r2)) => take(r2, got :+ c)
            case None => (got, null)
        var acc = agg.init
        var rest: Chunks[A] | Null = d
        val inflight = mutable.Queue.empty[Fiber[Acc]]
        while rest != null || inflight.nonEmpty do
          while rest != null && inflight.length < parallelism do
            val (cs, r) = take(rest.nn, Vector.empty)
            if cs.nonEmpty then inflight.enqueue(S.fork(() => async(fold(cs))))
            rest = r
          if inflight.nonEmpty then acc = agg.merge(acc, inflight.dequeue().join())
        agg.present(acc)

      def join[K, A, B](l: Chunks[(K, A)], r: Chunks[(K, B)]): Chunks[(K, (A, B))] = Chunks.defer:
        // the right side: partial maps on fibres, merged — an Aggregator
        // whose merge is order-free, so `aggregate`'s road is the road
        val right = aggregate(r)(new Aggregator[(K, B), mutable.HashMap[K, mutable.ArrayBuffer[B]], Map[K, Seq[B]]]:
          def init = mutable.HashMap.empty[K, mutable.ArrayBuffer[B]]
          def add(m: mutable.HashMap[K, mutable.ArrayBuffer[B]], kb: (K, B)) =
            m.getOrElseUpdate(kb._1, mutable.ArrayBuffer.empty) += kb._2; m
          def merge(a: mutable.HashMap[K, mutable.ArrayBuffer[B]], b: mutable.HashMap[K, mutable.ArrayBuffer[B]]) =
            for (k, bs) <- b do a.getOrElseUpdate(k, mutable.ArrayBuffer.empty) ++= bs
            a
          def present(m: mutable.HashMap[K, mutable.ArrayBuffer[B]]) = m.view.mapValues(_.toSeq).toMap)
        def probe(c: Chunk[(K, A)]): Chunk[(K, (A, B))] =
          ChunkBuf.of(c.iterator.flatMap((k, a) => right.getOrElse(k, Nil).iterator.map(b => (k, (a, b)))))
        def go(inflight: Vector[Fiber[Chunk[(K, (A, B))]]], rest: Chunks[(K, A)] | Null): Chunks[(K, (A, B))] = Chunks.defer:
          var q = inflight
          var rr = rest
          while q.length < parallelism && rr != null do
            Chunks.pull(rr.nn) match
              case Some((c, r2)) => q = q :+ S.fork(() => async(probe(c))); rr = r2
              case None => rr = null
          q match
            case h +: t => Writer.tell(h.join()).flatMap(_ => go(t, rr))
            case _ => Chunks.end
        go(Vector.empty, l)
