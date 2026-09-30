package okay2.stream

import scala.collection.mutable.ArrayBuffer
import okay2._
import okay2.async._

/**
 * The sort-merge join by key, as ONE machine — the Scala 2 twin of the
 * core's `SortMerge` (specs/stream-join.md): two sides non-decreasing
 * in key, one cursor each, the right RUN of equal keys held while the
 * left side streams against it, nothing held beyond that run and one
 * lookahead element. `step` decides everything it can, emits through
 * its callback, and answers what the next decision needs. A row is
 * matched only against a CLOSED run. Sortedness is CHECKED: a key
 * smaller than the one before it on the same side fails with both
 * keys named, after everything decided before it.
 */
final class SortMerge[K, A, B, O](matched: (K, A, B) => O,
                                  leftOnly: Option[(K, A) => O],
                                  rightOnly: Option[(K, B) => O])
                                 (implicit ord: Ordering[K]) {
  import SortMerge.Need

  private var lk: K = _
  private var la: A = _
  private var lHas = false
  private var lDone = false
  private var lLast: K = _
  private var lSeen = false

  private var rk: K = _
  private val run = ArrayBuffer.empty[B]
  private var runMatched = false
  private var nk: K = _
  private var nb: B = _
  private var nHas = false
  private var rDone = false
  private var rLast: K = _
  private var rSeen = false

  /** the rows held now: the right run and its lookahead */
  def held: Int = run.length + (if (nHas) 1 else 0)

  private def check(side: String, seen: Boolean, last: K, k: K): Unit =
    if (seen && ord.lt(k, last))
      throw new IllegalArgumentException(s"joinSorted: the $side side is not sorted — key $k after $last")

  def left(k: K, a: A): Unit = {
    check("left", lSeen, lLast, k)
    lLast = k; lSeen = true
    lk = k; la = a; lHas = true
  }

  def leftEnd(): Unit = lDone = true

  def right(k: K, b: B): Unit = {
    check("right", rSeen, rLast, k)
    rLast = k; rSeen = true
    if (run.isEmpty) { rk = k; run += b; runMatched = false }
    else if (ord.equiv(k, rk)) run += b
    else { nk = k; nb = b; nHas = true }
  }

  def rightEnd(): Unit = rDone = true

  private def runClosed: Boolean = nHas || rDone

  private def releaseRun(emit: O => Unit): Unit = {
    if (!runMatched) rightOnly.foreach { f =>
      var i = 0
      while (i < run.length) { emit(f(rk, run(i))); i += 1 }
    }
    run.clear()
    runMatched = false
  }

  private def promote(): Unit = { rk = nk; run += nb; runMatched = false; nHas = false }

  private def leftDecided(emit: O => Unit): Unit = {
    leftOnly.foreach(f => emit(f(lk, la)))
    lHas = false
  }

  def step(emit: O => Unit): Need = {
    var need: Need = null
    while (need == null) {
      if (!lHas) {
        if (!lDone) need = Need.Left
        else if (rightOnly.isEmpty) need = Need.Done
        else {
          if (run.nonEmpty) releaseRun(emit)
          if (nHas) promote()
          else if (rDone) need = Need.Done
          else need = Need.Right
        }
      }
      else if (run.isEmpty) {
        if (nHas) promote()
        else if (rDone) {
          if (leftOnly.isEmpty) need = Need.Done
          else leftDecided(emit)
        }
        else need = Need.Right
      }
      else if (!runClosed) need = Need.Right
      else {
        val c = ord.compare(lk, rk)
        if (c == 0) {
          var i = 0
          while (i < run.length) { emit(matched(lk, la, run(i))); i += 1 }
          runMatched = true
          lHas = false
        }
        else if (c < 0) leftDecided(emit)
        else releaseRun(emit)
      }
    }
    need
  }
}

object SortMerge {
  sealed trait Need
  object Need {
    case object Left extends Need
    case object Right extends Need
    case object Done extends Need
  }

  def inner[K: Ordering, A, B]: SortMerge[K, A, B, (K, (A, B))] =
    new SortMerge((k, a, b) => (k, (a, b)), None, None)

  def left[K: Ordering, A, B]: SortMerge[K, A, B, (K, (A, Option[B]))] =
    new SortMerge((k, a, b) => (k, (a, Some(b))), Some((k, a) => (k, (a, None))), None)

  def full[K: Ordering, A, B]: SortMerge[K, A, B, (K, (Option[A], Option[B]))] =
    new SortMerge((k, a, b) => (k, (Some(a), Some(b))),
                  Some((k, a) => (k, (Some(a), None))),
                  Some((k, b) => (k, (None, Some(b)))))

  /**
   * The chunk driver: one tree step per LEFT chunk, right chunks pulled
   * as the machine asks, the rows emitted while that left chunk was
   * decided told as one chunk. `fresh` makes the machine at the first
   * step — a `Chunks` is a value, and running it twice must not share
   * one machine.
   */
  def chunks[K, A, B, O](l: Chunks[(K, A)], r: Chunks[(K, B)])(fresh: () => SortMerge[K, A, B, O]): Chunks[O] = {
    def go(m: SortMerge[K, A, B, O], out: ArrayBuffer[O],
           ca: Chunk[(K, A)], ia: Int, ra: Chunks[(K, A)],
           cb: Chunk[(K, B)], ib: Int, rb: Chunks[(K, B)]): Chunks[O] = Chunks.defer {
      var ia2 = ia
      var cb2 = cb; var ib2 = ib; var rb2 = rb
      var next: (Chunk[(K, A)], Chunks[(K, A)]) = null
      var done = false
      while (next == null && !done) {
        m.step(o => { out += o; () }) match {
          case Need.Done => done = true
          case Need.Left =>
            if (ia2 < ca.length) { val (k, a) = ca(ia2); ia2 += 1; m.left(k, a) }
            else Chunks.pull(ra) match {
              case None => m.leftEnd()
              case Some(pulled) => next = pulled
            }
          case Need.Right =>
            if (ib2 < cb2.length) { val (k, b) = cb2(ib2); ib2 += 1; m.right(k, b) }
            else Chunks.pull(rb2) match {
              case None => m.rightEnd()
              case Some((c, rest)) => cb2 = c; ib2 = 0; rb2 = rest
            }
        }
      }
      val told: Chunks[O] =
        if (out.isEmpty) Chunks.end[O]
        else { val c = ChunkBuf.of(out); out.clear(); Writer.tell(c) }
      if (done) told
      else { val (c, rest) = next; told.flatMap(_ => go(m, out, c, 0, rest, cb2, ib2, rb2)) }
    }
    Chunks.defer(go(fresh(), ArrayBuffer.empty[O], Chunks.emptyChunk, 0, l, Chunks.emptyChunk, 0, r))
  }

  /**
   * The live driver, in okay2's `Source.zip` shape: each side buffered
   * onto a fiber of its own, one receive per need on the consumer's
   * thread, the rows a step emitted told one by one, BOTH channels
   * closed at the end so a feeder parked on a full buffer wakes and
   * ends. No early-stop release: okay2 has no cancel scope, as its zip
   * and merge have none either.
   */
  def source[K, A, B, O](s: Source[(K, A)], t: Source[(K, B)], capacity: Int)
                        (fresh: () => SortMerge[K, A, B, O])
                        (implicit sch: Scheduler, cb: CanBlock): Source[O] = {
    type R = Writer[O] + Async
    type L[W] = Unit ! (Writer[W] + Async)
    pure[R, Unit](()).flatMap { _ =>
      val m = fresh()
      val out = ArrayBuffer.empty[O]
      val cl = Channel.buffer[(K, A), L, Async](capacity)(s)(Stream.writerStreamIn[Unit, Async], Async.handler(cb), sch)
      val cr = Channel.buffer[(K, B), L, Async](capacity)(t)(Stream.writerStreamIn[Unit, Async], Async.handler(cb), sch)
      def tellAll(i: Int): Unit ! R =
        if (i >= out.length) { out.clear(); pure[R, Unit](()) }
        else Writer.tell(out(i)).at[R].flatMap(_ => tellAll(i + 1))
      def end: Unit ! R = { cl.close(); cr.close(); pure[R, Unit](()) }
      def go: Unit ! R =
        m.step(o => { out += o; () }) match {
          case Need.Done => tellAll(0).flatMap(_ => end)
          case Need.Left =>
            tellAll(0).flatMap(_ => cl.receive.at[R]).flatMap {
              case None => m.leftEnd(); go
              case Some((k, a)) => m.left(k, a); go
            }
          case Need.Right =>
            tellAll(0).flatMap(_ => cr.receive.at[R]).flatMap {
              case None => m.rightEnd(); go
              case Some((k, b)) => m.right(k, b); go
            }
        }
      go
    }
  }
}
