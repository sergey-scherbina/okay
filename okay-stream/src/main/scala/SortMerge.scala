package okay


import okay.freer.*


import okay.std.*
import okay.std.given
import scala.collection.mutable.ArrayBuffer
import scala.compiletime.uninitialized

/**
 * The sort-merge join by key, as ONE machine (specs/stream-join.md):
 * two sides non-decreasing in key, one cursor each, the right RUN of
 * equal keys held while the left side streams against it, nothing
 * held beyond that run and one lookahead element. The machine decides;
 * it does not pull. `step` decides everything it can, emits through
 * its callback, and answers what the next decision needs — a left
 * element, a right element, or nothing — so a driver over chunk
 * cursors (`SortMerge.chunks`) and a driver over two channels
 * (`SortMerge.source`) are each a dozen lines about their carrier and
 * nothing about merging.
 *
 * `matched` shapes a pair; `leftOnly` / `rightOnly` shape an unmatched
 * row, or are null for a join that drops that side (the inner join
 * drops both; a left join drops unmatched right rows). A row is
 * matched only against a CLOSED run — one the right side has moved
 * past, or ended after — because an open run may still grow, and a
 * left row matched early would miss the growth.
 *
 * Sortedness is CHECKED: a key smaller than the one before it on the
 * same side fails with both keys named, at that row, after everything
 * decided before it. Blasgen & Eswaran (1977) is the algorithm; what
 * the join does with an unmatched row is the spec's business.
 */
final class SortMerge[K, A, B, O](matched: (K, A, B) => O,
                                  leftOnly: ((K, A) => O) | Null,
                                  rightOnly: ((K, B) => O) | Null)
                                 (using ord: Ordering[K]) {
  import SortMerge.Need

  // the left row in hand, not yet decided
  private var lk: K = uninitialized
  private var la: A = uninitialized
  private var lHas = false
  private var lDone = false
  private var lLast: K = uninitialized
  private var lSeen = false

  // the right run: its key, its rows, whether a left row matched it
  private var rk: K = uninitialized
  private val run = ArrayBuffer.empty[B]
  private var runMatched = false
  // the right row that CLOSED the run — the first of the next one
  private var nk: K = uninitialized
  private var nb: B = uninitialized
  private var nHas = false
  private var rDone = false
  private var rLast: K = uninitialized
  private var rSeen = false

  /** the rows held now: the right run and its lookahead */
  def held: Int = run.length + (if nHas then 1 else 0)

  private def check(side: String, seen: Boolean, last: K, k: K): Unit =
    if seen && ord.lt(k, last) then
      throw new IllegalArgumentException(
        s"joinSorted: the $side side is not sorted — key $k after $last")

  def left(k: K, a: A): Unit =
    check("left", lSeen, lLast, k)
    lLast = k; lSeen = true
    lk = k; la = a; lHas = true

  def leftEnd(): Unit = lDone = true

  def right(k: K, b: B): Unit =
    check("right", rSeen, rLast, k)
    rLast = k; rSeen = true
    if run.isEmpty then { rk = k; run += b; runMatched = false }
    else if ord.equiv(k, rk) then run += b
    else { nk = k; nb = b; nHas = true }

  def rightEnd(): Unit = rDone = true

  /** the run cannot grow: something after it has arrived, or the end */
  private def runClosed: Boolean = nHas || rDone

  private def releaseRun(emit: O => Unit): Unit =
    if !runMatched && rightOnly != null then
      var i = 0
      while i < run.length do { emit(rightOnly.nn(rk, run(i))); i += 1 }
    run.clear()
    runMatched = false

  /** the lookahead becomes the next run */
  private def promote(): Unit =
    rk = nk; run += nb; runMatched = false; nHas = false

  private def leftDecided(emit: O => Unit): Unit =
    if leftOnly != null then emit(leftOnly.nn(lk, la))
    lHas = false

  def step(emit: O => Unit): Need =
    var need: Need | Null = null
    while need == null do
      if !lHas then
        if !lDone then need = Need.Left
        else if rightOnly == null then need = Need.Done
        else
          // the left side ended: every right row from here is unmatched
          if run.nonEmpty then releaseRun(emit)
          if nHas then promote()
          else if rDone then need = Need.Done
          else need = Need.Right
      else if run.isEmpty then
        if nHas then promote()
        else if rDone then
          // no right row will ever come: the inner join is over, the
          // others answer the left side alone
          if leftOnly == null then need = Need.Done
          else leftDecided(emit)
        else need = Need.Right
      else if !runClosed then need = Need.Right
      else
        val c = ord.compare(lk, rk)
        if c == 0 then
          var i = 0
          while i < run.length do { emit(matched(lk, la, run(i))); i += 1 }
          runMatched = true
          lHas = false
        else if c < 0 then leftDecided(emit)
        else releaseRun(emit)
    need.nn
}

object SortMerge {
  enum Need { case Left, Right, Done }

  def inner[K, A, B](using Ordering[K]): SortMerge[K, A, B, (K, (A, B))] =
    new SortMerge((k, a, b) => (k, (a, b)), null, null)

  def left[K, A, B](using Ordering[K]): SortMerge[K, A, B, (K, (A, Option[B]))] =
    new SortMerge((k, a, b) => (k, (a, Some(b))), (k, a) => (k, (a, None)), null)

  def full[K, A, B](using Ordering[K]): SortMerge[K, A, B, (K, (Option[A], Option[B]))] =
    new SortMerge((k, a, b) => (k, (Some(a), Some(b))),
                  (k, a) => (k, (Some(a), None)),
                  (k, b) => (k, (None, Some(b))))


  /**
   * The chunk driver: one tree step per LEFT chunk, right chunks pulled
   * as the machine asks for them (a pure step each), the rows emitted
   * while that left chunk was decided told as one chunk. `fresh` makes
   * the machine at the first step — a `Chunks` is a value, and running
   * it twice must not share one machine (the `Stage.chunked` precedent).
   */
  def chunks[K, A, B, O](l: Chunks[(K, A)], r: Chunks[(K, B)])
                        (fresh: () => SortMerge[K, A, B, O]): Chunks[O] =
    def go(m: SortMerge[K, A, B, O], out: ArrayBuffer[O],
           ca: Chunk[(K, A)], ia: Int, ra: Chunks[(K, A)],
           cb: Chunk[(K, B)], ib: Int, rb: Chunks[(K, B)]): Chunks[O] = Chunks.defer:
      var ia2 = ia
      var cb2 = cb; var ib2 = ib; var rb2 = rb
      // the left chunk that replaces `ca`, when it is exhausted and the
      // side is not — the loop stops there so this chunk's rows are told
      var next: (Chunk[(K, A)], Chunks[(K, A)]) | Null = null
      var done = false
      while next == null && !done do
        m.step(o => { out += o; () }) match
          case Need.Done => done = true
          case Need.Left =>
            if ia2 < ca.length then
              val (k, a) = ca(ia2); ia2 += 1; m.left(k, a)
            else Chunks.pull(ra) match
              case None => m.leftEnd()
              case Some(pulled) => next = pulled
          case Need.Right =>
            if ib2 < cb2.length then
              val (k, b) = cb2(ib2); ib2 += 1; m.right(k, b)
            else Chunks.pull(rb2) match
              case None => m.rightEnd()
              case Some((c, rest)) => cb2 = c; ib2 = 0; rb2 = rest
      val told: Chunks[O] =
        if out.isEmpty then Chunks.end
        else
          val c = ChunkBuf.of(out)
          out.clear()
          Writer.tell(c)
      if done then told
      else
        val (c, rest) = next.nn
        told.flatMap(_ => go(m, out, c, 0, rest, cb2, ib2, rb2))

    Chunks.defer(go(fresh(), ArrayBuffer.empty[O],
                    Chunks.emptyChunk, 0, l, Chunks.emptyChunk, 0, r))

  /**
   * The live driver, in `Source.zip`'s shape (specs/source-zip.md, its
   * Decisions — repeated rather than shared, so `zip` stays as its tests
   * pin it): each side `Channel.buffer`ed onto a fiber of its own, one
   * receive per need on the consumer's thread, the rows a step emitted
   * told one by one. The scope is entered in front and EXITED in the end
   * branch, and `go` names it, so a collection mid-run cannot release it
   * (source-zip-lost-pairs). At the end BOTH channels are closed: an
   * inner join ends when either side ends, and the other side's feeder,
   * parked on its full buffer, wakes and ends.
   */
  def source[K, A, B, O](s: Source[(K, A)], t: Source[(K, B)], capacity: Int)
                        (fresh: () => SortMerge[K, A, B, O])
                        (using Scheduler, CanBlock, Wait, Pause): Source[O] =
    type R = Writer % O + Async
    def receive[X](c: Channel[X]): Option[X] ! R =
      okay.freer.effect[R, Option[X]](Async.Await[Option[X]] { k => c.receiveAsync(k); () => c.cancelReceive(k) })
    okay.freer.pure[R, Unit](()).flatMap: _ =>
      val m = fresh()
      val out = ArrayBuffer.empty[O]
      val cl = Channel.buffer[(K, A), Source, Async](capacity)(s)
      val cr = Channel.buffer[(K, B), Source, Async](capacity)(t)
      val scope = Async.CancelScope(Merge.closing(cl, cr))
      def tellAll(i: Int): Unit ! R =
        if i >= out.length then { out.clear(); okay.freer.pure[R, Unit](()) }
        else okay.freer.effect[R, Unit](Writer(out(i))).flatMap(_ => tellAll(i + 1))
      def end: Unit ! R =
        cl.close(); cr.close()
        okay.freer.effect[R, Unit](Async.Run(Async.Exit(scope)))
      def go: Unit ! R =
        m.step(o => { out += o; () }) match
          case Need.Done => tellAll(0).flatMap(_ => end)
          case Need.Left =>
            tellAll(0).flatMap(_ => receive(cl)).flatMap:
              case None => m.leftEnd(); go
              case Some((k, a)) => m.left(k, a); go
          case Need.Right =>
            tellAll(0).flatMap(_ => receive(cr)).flatMap:
              case None => m.rightEnd(); go
              case Some((k, b)) => m.right(k, b); go
      okay.freer.effect[R, Unit](Async.Run(Async.Enter(scope))).flatMap(_ => go)
}
