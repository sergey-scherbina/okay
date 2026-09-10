package okay.cluster

import okay.*
import okay.codec.{Codecs, Schema}

/**
 * THE WORKER PROTOCOL (specs/dataflow.md, stage 4b).
 *
 * Two requests, and they are not a design so much as a transcription:
 * they are exactly the two passes `Flows.fan` already makes over each
 * partition in one process. The first learns the shape of a
 * partition's event times; the second runs it under the bounds the
 * first made computable.
 *
 * WHY TWO ROUND TRIPS, when one would obviously be cheaper. Because a
 * partition's watermark must be the STREAM's watermark at that point
 * (stage 1's theorem), and a worker cannot know what the partitions
 * before it saw. The coordinator is the only party that can compute a
 * prefix maximum, so it must hear from every partition before any of
 * them may fold. The alternative is not a faster protocol, it is a
 * different answer.
 */
enum Req:
  /** the pre-pass over one partition */
  case Extent(job: String, params: Array[Byte], part: Int, of: Int)
  /** run one partition under the bounds the coordinator computed */
  case Run(job: String, params: Array[Byte], part: Int, of: Int, bounds: Vector[Bounds])
  /** what this worker's build knows how to run */
  case Known

  // --- streaming (specs/dataflow.md, stage 6a) ---------------------

  /** begin an epoch-by-epoch run of one partition, and keep its
   * operator state between rounds */
  case Open(job: String, params: Array[Byte], part: Int, of: Int, session: Long)
  /**
   * Advance this partition to EPOCH `epoch` — by up to `take`
   * elements per epoch — and hand back what it closed in that one.
   *
   * The index is what makes recovery the ordinary case rather than a
   * second mechanism (specs/dataflow.md, stage 6b). A session already
   * at that epoch answers; a session behind it catches up silently by
   * replaying and discarding; a worker with no session at all is
   * given the job and then does the same. So a replacement worker
   * needs no snapshot of an operator's insides — it rebuilds them
   * from the recipe, which a partition has been since stage 1.
   */
  case Advance(session: Long, take: Int, bounds: Vector[Bounds], epoch: Int)
  /** the stream is over: close what is open and let the state go */
  case Close(session: Long)

enum Resp:
  case Extents(cols: Vector[Flows.Extent])
  /** the partial, encoded by the sink's own `wire` — the coordinator
   * decodes it with the same Schema, and nothing in between looks
   * inside */
  case Partial(bytes: Array[Byte])
  case Names(names: Vector[String])
  /** one epoch's partial, and the partition's own extent so far — the
   * coordinator needs the second to compute the watermark, which in a
   * stream is the MINIMUM over the partitions rather than the maximum
   * over everything */
  case Epoch(bytes: Array[Byte], extent: Vector[Flows.Extent], drained: Boolean)
  case Opened(session: Long)
  case Failed(why: String)

object Req:
  given Schema[Bounds] = Schema.derived
  given Schema[Req] = Schema.derived

object Resp:
  given Schema[Flows.Extent] = Schema.derived
  given Schema[Resp] = Schema.derived

/**
 * THE COORDINATOR.
 *
 * A worker is `Req => Resp`, which is what `Cluster.Worker` has been
 * since P7: in-process, or a socket away, and the driver cannot tell.
 * That is deliberate — every test below runs the same code with
 * in-process workers first, so a failure across four JVMs is a
 * failure of the transport and never of the arithmetic.
 */
object Cluster {

  /** where a partition's work is done: send a request, get an answer.
   * A worker that is gone THROWS, as it always has here. */
  type Serve = Req => Resp

  /**
   * Run a registered job across workers, partition i on worker
   * `i % workers.length`.
   *
   * The answer is the same `Run[R]` a local fan produces, and that is
   * the whole claim of this stage: the drop count and the merged
   * count mean the same thing whether the partitions ran here or on
   * four machines.
   */
  def run[P, R](job: Job[P, R], p: P, parts: Int, workers: Vector[Serve])
               (using Scheduler): Run[R] ! Async =
    require(parts > 0, "a job has at least one partition")
    require(workers.nonEmpty, "a job needs at least one worker")
    val encoded = Codecs.cbor(job.params).encode(p)
    val sink = job.sink(p)
    val living = Living(workers.length)

    val bounds: Vector[Vector[Bounds]] ! Async =
      if sink.times.isEmpty then pure[Async, Vector[Vector[Bounds]]](Vector.fill(parts)(Vector.empty))
      else
        Flows.spread(parts)(i =>
          ask(workers, living, i, Req.Extent(job.name, encoded, i, parts)) match
            case Resp.Extents(cols) => cols
            case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
            case other => throw IllegalStateException(s"partition $i answered $other to a pre-pass"))
          .map(Flows.edges)

    bounds.flatMap: bs =>
      Flows.spread(parts) { i =>
        ask(workers, living, i, Req.Run(job.name, encoded, i, parts, bs(i))) match
          case Resp.Partial(bytes) =>
            Codecs.cbor(sink.wire).decode(bytes) match
              case Right(w) => w
              case Left(why) => throw IllegalStateException(s"partition $i's partial: $why")
          case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
          case other => throw IllegalStateException(s"partition $i answered $other to a run")
      }.map: ws =>
        val out = Run(sink.result(ws), sink.drops(ws), parts, 1, sink.merged(ws),
          living.retries, living.lost)
        sink.committed(1)   // a batch run is ONE epoch, and it is over
        out

  /**
   * ASK A LIVING WORKER, AND KEEP ASKING (specs/dataflow.md, stage 5).
   *
   * A thrown error is a DEAD WORKER: the transport failed, the
   * process is gone, and the same request on a survivor is the right
   * next move. The worker leaves the rotation and the partition is
   * recomputed elsewhere — which costs nothing structural, because a
   * partition is a thunk and its partial is a pure function of the
   * four things every worker is given: the parameters, the index, the
   * count and the bounds. There is no lineage graph to walk and no
   * checkpoint to restore, because nothing was mutated.
   *
   * A `Resp.Failed` is NOT retried. It is the worker's considered
   * answer — it decoded the request and refused — and since every
   * worker runs the same build, asking another one produces the
   * identical refusal. Retrying a deterministic "no" four times is
   * not resilience, it is noise in front of the same message.
   *
   * NOT exactly-once EXECUTION: a worker that dies after computing
   * but before its reply arrives has its partition computed twice.
   * That is correct because the coordinator keeps exactly one partial
   * per partition — exactly-once OUTCOME, the words specs/persist.md
   * already settled on.
   */
  private def ask(workers: Vector[Serve], living: Living, part: Int, req: Req): Resp =
    def go(tried: Int, first: Throwable | Null): Resp =
      living.pick(part + tried) match
        case None =>
          val why = IllegalStateException(
            s"partition $part: no workers left (${workers.length} were given)")
          if first != null then why.initCause(first.nn): Unit
          throw why
        case Some(w) =>
          try
            val out = workers(w)(req)
            living.answered(w)
            out
          catch case t: Throwable =>
            living.failed(w)
            go(tried + 1, if first == null then t else first)
    go(0, null)

  /**
   * Who is still answering.
   *
   * Shared by every partition's fibre, so it is synchronized — and
   * that is the whole of the concurrency here. Two fibres may bury
   * the same worker; the second is a no-op, which is what
   * `filterNot` gives for free.
   */
  private final class Living(n: Int, tolerance: Int = Living.Tolerance):
    private var alive: Vector[Int] = (0 until n).toVector
    private var buried: Long = 0L
    private val failures = Array.fill(n)(0)
    private var attempts: Long = 0L

    /** a survivor for this attempt, or None when there are none */
    def pick(turn: Int): Option[Int] = synchronized {
      if alive.isEmpty then None else Some(alive(math.floorMod(turn, alive.length)))
    }

    /**
     * A FAILURE IS NOT YET A DEATH (dataflow-reconnect).
     *
     * Burying on the first throw made a run unable to survive a
     * transient blip on every worker — stage 5's first seeded test
     * found that by asking for exactly it, and it stayed a named
     * limit for five stages. A worker is buried after `tolerance`
     * CONSECUTIVE failures instead, and any success clears its count,
     * so a machine that hiccups stays in the rotation and one that is
     * gone is still buried after a bounded number of attempts —
     * bounded across the whole run, not per request, because the
     * count is the worker's rather than the caller's.
     *
     * The partition still moves to a survivor on every failure. This
     * changes who is asked NEXT TIME, not who answers now.
     */
    def failed(w: Int): Unit = synchronized {
      attempts += 1
      if alive.contains(w) then
        failures(w) += 1
        if failures(w) >= tolerance then { alive = alive.filterNot(_ == w); buried += 1 }
    }

    /** whatever this worker had against it, it has just answered */
    def answered(w: Int): Unit = synchronized {
      if failures(w) != 0 then failures(w) = 0
    }

    /** how many workers were buried — reported so a suite can assert
     * that recovery HAPPENED rather than infer it from the answer
     * being right */
    def retries: Long = synchronized(buried)

    /** how many attempts were lost to a failure, buried or forgiven */
    def lost: Long = synchronized(attempts)

  private object Living:
    /**
     * HOW MANY CONSECUTIVE FAILURES ARE A DEATH.
     *
     * Three, and the number is a judgement rather than a
     * measurement: one is what the engine did and could not survive a
     * blip, and a large number keeps asking a corpse. What makes
     * three cheap is that the count is per WORKER and per RUN, so a
     * worker that is really gone costs three attempts once, not three
     * per partition.
     */
    val Tolerance: Int = 3

  /**
   * A worker made of a registry: answer a request by looking the job
   * up and running it. This is what a served process does, and it is
   * also what an in-process worker does — one function, so a test
   * without sockets exercises the same code a socket does.
   */
  val local: Serve = {
    case Req.Known => Resp.Names(Jobs.names)
    case Req.Open(name, params, part, of, session) =>
      Jobs.find(name) match
        case None => Resp.Failed(s"no job named '$name' in this build; it knows ${Jobs.names}")
        case Some(job) =>
          job.openAt(params, part, of) match
            case Right(st) => { Sessions.put(session, st); Resp.Opened(session) }
            case Left(why) => Resp.Failed(s"parameters for '$name': $why")
    case Req.Advance(session, take, bounds, epoch) =>
      Sessions.get(session) match
        case None => Resp.Failed(s"no session $session on this worker")
        case Some(st) => st.advance(take, bounds, epoch)
    case Req.Close(session) =>
      Sessions.get(session) match
        case None => Resp.Failed(s"no session $session on this worker")
        case Some(st) =>
          val out = st.finish()
          Sessions.drop(session)
          out
    case Req.Extent(name, params, part, of) =>
      Jobs.find(name) match
        case None => Resp.Failed(s"no job named '$name' in this build; it knows ${Jobs.names}")
        case Some(job) =>
          job.extentAt(params, part, of) match
            case Right(cols) => Resp.Extents(cols)
            case Left(why) => Resp.Failed(s"parameters for '$name': $why")
    case Req.Run(name, params, part, of, bounds) =>
      Jobs.find(name) match
        case None => Resp.Failed(s"no job named '$name' in this build; it knows ${Jobs.names}")
        case Some(job) =>
          job.partialAt(params, part, of, bounds) match
            case Right(bytes) => Resp.Partial(bytes)
            case Left(why) => Resp.Failed(s"parameters for '$name': $why")
  }

  /**
   * STREAM A JOB, EPOCH BY EPOCH (specs/dataflow.md, stage 6a).
   *
   * Each round advances every partition by up to `take` elements. The
   * workers keep their operator state between rounds — an open pane
   * is open across an epoch boundary, which is the whole difference
   * between this and running the batch driver in a loop.
   *
   * THE WATERMARK IS THE MINIMUM OVER THE PARTITIONS, and that is not
   * a detail. In a batch run the coordinator hears from every
   * partition before anything folds, so the greatest event time is
   * known. In a stream it knows only what each partition has reported
   * SO FAR, and a partition that has not advanced holds the whole
   * stream back — because whatever it has not read yet may still fall
   * into a pane the others consider closed.
   *
   * A pane the watermark has passed is retired by `Sink.absorb` and
   * leaves the coordinator's memory; one still open meets the next
   * epoch's partials. That is what keeps a stream's coordinator
   * bounded rather than growing with the run.
   *
   * The run ends when every partition reports itself drained, which
   * for a bounded source is the batch answer — and the suite asserts
   * exactly that, because a streaming answer that differs from the
   * batch one is wrong rather than different.
   */
  def stream[P, R](job: Job[P, R], p: P, parts: Int, workers: Vector[Serve], take: Int,
                   journal: Checkpoint = Checkpoint.none)
                  (using Scheduler): Run[R] ! Async =
    require(parts > 0, "a job has at least one partition")
    require(workers.nonEmpty, "a job needs at least one worker")
    require(take > 0, "an epoch advances by at least one element")
    val encoded = Codecs.cbor(job.params).encode(p)
    val sink = job.sink(p)
    val living = Living(workers.length)
    val folded = Codecs.cbor(Folded.given_Schema_Folded)
    val held = Codecs.cbor(sink.state)
    // the ids this run's sessions carry — inherited from the journal
    // when there is one, so a resumed coordinator picks up the
    // sessions its predecessor opened rather than stranding them
    val resuming: Option[Folded] = journal.latest.flatMap { (at, bytes) =>
      folded.decode(bytes) match
        case Right(f) => Some(f)
        case Left(why) => throw IllegalStateException(s"the journal at epoch $at: $why")
    }
    val base = resuming.fold(System.nanoTime())(_.base)
    val sessions = Vector.tabulate(parts)(i => base + i)
    def opening(i: Int): Req.Open = Req.Open(job.name, encoded, i, parts, sessions(i))

    def commit(round: Int, st: sink.S, seen: Vector[Vector[Flows.Extent]],
               drops: Long, merged: Long): Unit =
      journal.save(round,
        folded.encode(Folded(round, seen, drops, merged, held.encode(st), base)))

    // NO UPFRONT OPEN. The first `Advance` finds no session and opens
    // one, which is the identical path a replacement worker takes —
    // so the recovery road IS the road, exercised on every run rather
    // than only when something has died.
    locally:
      def epoch(state: sink.S, seen: Vector[Vector[Flows.Extent]], drops: Long, merged: Long,
                round: Int): Run[R] ! Async =
        // NO LOCAL COMPLETENESS IN A STREAM, and this is the one
        // place the streaming engine had to stop copying the batch
        // one.
        //
        // A partition may finish a pane by itself when it knows that
        // nothing earlier can still arrive — `end <= hi(i) - back`.
        // In a batch run every partition's extent is known before
        // anything folds, so `back` is the stream's true
        // backwardness. In a stream `back` is only what has been SEEN
        // so far, which is an under-estimate of what is to come, so
        // `upper` is too generous and a pane is finished before its
        // last elements arrive. It is then presented twice, once for
        // each half of its data: the totals still agree and the pane
        // COUNT goes up, which is exactly what the batch comparison
        // caught (3214 panes against 3204, same sum).
        //
        // So a streaming partition finishes nothing locally, and the
        // COORDINATOR retires panes on the global watermark instead.
        // That is the ordinary architecture of a stream processor,
        // and the completeness rule stays what it is: a batch
        // optimisation that needs the whole extent to be legal.
        //
        // `Bounds(MinValue, MinValue)`: the lower bound is also the
        // watermark SEED, so it must be MinValue — a partition in a
        // stream starts where it starts and has no earlier stream to
        // inherit from. (MaxValue there seeds the watermark to
        // infinity and every element is late: 20 000 of 20 000
        // dropped, which is how that mistake announced itself.) The
        // upper bound at MinValue is what finishes nothing locally.
        val bs = Vector.fill(parts)(sink.times.map(_ => Bounds(Long.MinValue, Long.MinValue)))
        Flows.spread(parts) { i =>
          advancing(workers, living, i, opening(i),
            Req.Advance(sessions(i), take, bs(i), round)) match
            case e: Resp.Epoch => e
            case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
            case other => throw IllegalStateException(s"partition $i answered $other to an advance")
        }.flatMap { es =>
          val ws = es.zipWithIndex.map { (e, i) =>
            Codecs.cbor(sink.wire).decode(e.bytes) match
              case Right(w) => w
              case Left(why) => throw IllegalStateException(s"partition $i's partial: $why")
          }
          val grown = seen.indices.toVector.map(i => merge(seen(i), es(i).extent))
          // NOT `drained`: the SOURCES being exhausted is not the
          // stream being over. Every operator still holds the panes
          // its own watermark never closed, and those come out on the
          // Close below. Treating the last epoch as the end retires
          // the fast partition's half of a boundary pane before the
          // slow partition's half arrives, and the two are then
          // presented as two panes — all ten of them in window 99000,
          // which is exactly where the two partitions meet.
          val mark = watermark(grown, sink.slack)
          val next = sink.absorb(state, ws, mark)
          val d = drops + sink.drops(ws)
          val m = merged + sink.merged(ws)
          // THE COMMIT, and it is one line because the loop is
          // lock-step: every partition has contributed exactly rounds
          // 1..round, so what the coordinator holds now is a
          // consistent cut with nothing in flight (stage 8).
          // THE SINK HEARS IT FIRST, and the order is the contract
          // (specs/dataflow.md, stage 9). A writer told after the
          // journal would lose an epoch whenever the coordinator died
          // in between — the journal would say the epoch happened and
          // the writer would never have been asked for it. Told
          // BEFORE, the worst case is being asked for the same epoch
          // twice, which a writer that records the epoch number with
          // its data ignores.
          sink.committed(round)
          commit(round, next, grown, d, m)
          if es.forall(_.drained) then
            // THE CLOSE CARRIES A PARTIAL. Every pane still open when
            // the source ran out is swept out by `finish` and comes
            // back here; the first version of this driver used the
            // close only to release the session and threw that
            // partial away, which lost fifteen panes of three
            // thousand and is what the batch comparison caught.
            //
            // (And the close is a PROGRAM: building it and dropping
            // it also left every session alive on every worker, which
            // `Sessions.count` caught separately.)
            Flows.spread(parts) { i =>
              ask(workers, living, i, Req.Close(sessions(i))) match
                case e: Resp.Epoch => e
                case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
                case other => throw IllegalStateException(s"partition $i answered $other to a close")
            }.map { last =>
              val lw = last.zipWithIndex.map { (e, i) =>
                Codecs.cbor(sink.wire).decode(e.bytes) match
                  case Right(w) => w
                  case Left(why) => throw IllegalStateException(s"partition $i's last partial: $why")
              }
              val end = sink.absorb(next, lw, Long.MaxValue)
              val dd = d + sink.drops(lw)
              val mm = m + sink.merged(lw)
              // the sweep is an epoch too: a coordinator that dies
              // between the last Close and the answer resumes here,
              // asks for one more epoch, is told everything is
              // drained, and re-answers the same value
              // THE SWEEP IS ITS OWN EPOCH, and the number has to
              // move: a writer that recognises a repeat by its epoch
              // number would drop the close's panes as a duplicate of
              // the last round's, which is exactly what happened the
              // first time this was written.
              sink.committed(round + 1)
              commit(round + 1, end, grown, dd, mm)
              Run(sink.emit(end), dd, parts, 1, mm, living.retries, living.lost)
            }
          else epoch(next, grown, d, m, round + 1)
        }

      /**
       * WHERE THIS RUN BEGINS — and it is the same loop either way.
       *
       * A journal with something in it hands back the fold, the
       * per-partition extents and the two counters as of epoch N, and
       * the loop starts at N+1. The WORKERS need nothing new: 6b's
       * `Advance` carries the epoch INDEX, so a worker with no
       * session opens one and replays to it, discarding what the
       * coordinator has already folded. A resumed coordinator is a
       * replacement worker for everyone at once.
       */
      resuming match
        case None => epoch(sink.empty, Vector.fill(parts)(Vector.empty), 0L, 0L, 1)
        case Some(f) =>
          held.decode(f.state) match
            case Left(why) =>
              throw IllegalStateException(s"the journal's fold at epoch ${f.epoch}: $why")
            case Right(st) =>
              // everything a previous coordinator did AFTER this
              // epoch was lost with it and never happened — the one
              // thing a writer needs to know before the first pane of
              // the resumed run reaches it
              sink.recovered(f.epoch)
              epoch(st, f.seen, f.drops, f.merged, f.epoch + 1)

  /**
   * RUN THE JOB IF THIS PROCESS IS THE COORDINATOR
   * (specs/dataflow.md, stage 10).
   *
   * Stage 8 made a successor possible and stage 9 made its writes
   * safe; this is who starts one. The lease is taken, the journal is
   * fenced by the term it answers, the stream runs, and the seat is
   * given up at the end. `None` means somebody else holds it.
   *
   * IT DOES NOT WAIT TO BE ELECTED, and that is deliberate rather
   * than unfinished. A retry loop needs a clock, a backoff and a
   * decision about how long to keep trying — every one of which
   * belongs to whatever supervises this process, not to a dataflow
   * engine. One attempt composes into any of them:
   *
   * {{{
   * while running do
   *   Cluster.leading(job, params, parts, workers, take, journal, lease)
   *     .runWith match
   *       case Some(run) => report(run)          // the stream ended
   *       case None      => sleep(a while)       // somebody else leads
   * }}}
   *
   * A coordinator DEPOSED mid-run throws `Checkpoint.Deposed` out of
   * here at its next epoch, which is the correct end of that attempt:
   * its successor already holds the journal, and everything this one
   * folded since the last commit was never recorded.
   */
  def leading[P, R](job: Job[P, R], p: P, parts: Int, workers: Vector[Serve], take: Int,
                    journal: Checkpoint, lease: Lease)
                   (using Scheduler): Option[Run[R]] ! Async =
    lease.take() match
      case None => pure[Async, Option[Run[R]]](None)
      case Some(term) =>
        stream(job, p, parts, workers, take, Checkpoint.fenced(term, lease, journal))
          .map { run => lease.release(term); Some(run) }

  /**
   * ADVANCE ONE PARTITION, WHEREVER IT CAN BE DONE
   * (specs/dataflow.md, stage 6b).
   *
   * Two things can go wrong and they are nearly the same thing:
   *
   *   - the worker answers "no session": it has never served this
   *     partition, or it lost the state. It is given the job and
   *     asked again for the SAME epoch.
   *   - the worker is gone: it is buried, and the next survivor is
   *     asked for the SAME epoch, opening a session there first.
   *
   * The same epoch, both times, and that is the whole correctness
   * argument: a partition's epoch partial is a pure function of
   * (parameters, index, count, epoch size, epoch number), so asking
   * somebody else the same question gets the same answer. Asking for
   * the NEXT epoch instead would lose one epoch's data and fail
   * nothing, which is the mistake this comment exists to prevent.
   */
  private def advancing(workers: Vector[Serve], living: Living, part: Int,
                        open: Req.Open, adv: Req.Advance): Resp =
    def onceOn(w: Int): Resp =
      workers(w)(adv) match
        case Resp.Failed(why) if why.startsWith("no session") =>
          workers(w)(open) match
            case Resp.Opened(_) => workers(w)(adv)
            case other => other
        case other => other

    def go(tried: Int, first: Throwable | Null): Resp =
      living.pick(part + tried) match
        case None =>
          val why = IllegalStateException(
            s"partition $part: no workers left (${workers.length} were given)")
          if first != null then why.initCause(first.nn): Unit
          throw why
        case Some(w) =>
          try
            val out = onceOn(w)
            living.answered(w)
            out
          catch case t: Throwable =>
            living.failed(w)
            go(tried + 1, if first == null then t else first)
    go(0, null)

  /** a partition's extent so far, taking this epoch's into the last */
  private def merge(a: Vector[Flows.Extent], b: Vector[Flows.Extent]): Vector[Flows.Extent] =
    if a.isEmpty then b else if b.isEmpty then a
    else a.indices.toVector.map { j =>
      Flows.Extent(math.max(a(j).max, b(j).max), math.min(a(j).min, b(j).min),
        math.max(a(j).back, b(j).back))
    }

  /**
   * THE LEAST EVENT TIME THAT CAN STILL ARRIVE.
   *
   * The minimum over the partitions of (what each has seen MINUS how
   * far it has been known to go back). Two halves, and both are
   * load-bearing:
   *
   *   - the MINIMUM, because a partition that has read less may still
   *     produce something earlier than the others' greatest, and a
   *     partition that has read nothing at all may produce anything;
   *   - minus the DECLARED lateness, because even a partition that
   *     has read far can still produce an element below its own
   *     greatest. The first version used the raw maximum and the
   *     second used the OBSERVED backwardness; both retire a pane
   *     before its last elements arrive, and a pane retired early is
   *     presented twice, once for each half of its data (3214 panes
   *     against 3204, with the sums equal — which is what the batch
   *     comparison caught, twice).
   *
   *     The observed backwardness is not a bound on the future: it is
   *     what has been seen so far and it grows. The declared lateness
   *     is the user's own contract about what may still arrive, and
   *     it is the only number here that is true of what has not.
   *
   * There is no "and now it is over" case here on purpose: that
   * belongs to the CLOSE, which is the only moment at which every
   * operator has swept out what it still held.
   */
  private def watermark(seen: Vector[Vector[Flows.Extent]], slack: Long): Long =
    if seen.exists(_.isEmpty) then Long.MinValue
    else
      val least = seen.map(_.map(_.max).min).min
      if least == Long.MinValue then Long.MinValue else least - slack

  /** the work seam of P7, unchanged: one executor of chunk work */
  type Worker[A, Acc] = Chunk[A] => Acc

  /**
   * Drive a replayable chunked source over the workers, round-robin
   * over the living: a worker that throws is dead and leaves the
   * rotation, its chunk is recomputed on a survivor, the partials
   * merge by the aggregator's combOp (order-free by the P1 contract).
   * No workers left = the exception propagates: nothing to hide.
   */
  def distribute[A, Acc](source: Chunks[A], workers: Vector[Worker[A, Acc]])
                        (zero: Acc, merge: (Acc, Acc) => Acc): Acc =
    var alive = workers.indices.toVector
    var acc = zero
    var rest = source
    var turn = 0
    var pulled = Chunks.pull(rest)
    while pulled.isDefined do
      val (c, r) = pulled.get
      var done = false
      while !done do
        if alive.isEmpty then throw new IllegalStateException("no workers left")
        val w = alive(turn % alive.size)
        turn += 1
        try
          acc = merge(acc, workers(w)(c))
          done = true
        catch case _: Throwable =>
          alive = alive.filterNot(_ == w)   // dead; the chunk goes to a survivor
      rest = r
      pulled = Chunks.pull(rest)
    acc
}
