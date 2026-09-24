package okay.cluster

import okay.*
import okay.codec.{Codecs, Digest, Schema}

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
  /** the pre-pass over one partition. `digest` is the coordinator's
   * own `okay.codec.Digest` of the job's partial Schema, encoded —
   * empty when the coordinator has not been built with the door
   * check (specs/federation.md, stage 3), which skips it entirely,
   * the same "hasn't asked yet" shape as `Checkpoint.none` */
  case Extent(job: String, params: Array[Byte], part: Int, of: Int, digest: Array[Byte] = Array.emptyByteArray)
  /** run one partition under the bounds the coordinator computed */
  case Run(job: String, params: Array[Byte], part: Int, of: Int, bounds: Vector[Bounds],
           digest: Array[Byte] = Array.emptyByteArray)
  /** what this worker's build knows how to run */
  case Known

  // --- streaming (specs/dataflow.md, stage 6a) ---------------------

  /** begin an epoch-by-epoch run of one partition, and keep its
   * operator state between rounds */
  case Open(job: String, params: Array[Byte], part: Int, of: Int, session: Long,
            /** where to open: `from` elements in and already at
             * `epoch` — zero and zero is the start, and what a
             * windowed sink always asks for (stage 11 box 2) */
            from: Long = 0L, epoch: Int = 0,
            /** the door check (specs/federation.md, stage 3) — see `Extent` */
            digest: Array[Byte] = Array.emptyByteArray)
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
  /** `build` is an opaque string a SERVING side may stamp onto its
   * answer to `Req.Known` — empty by default, so nothing that does
   * not opt in changes. okay-pool (specs/cluster-pool.md stage 1)
   * uses it to detect a rolling update mixing two artifacts inside
   * one run: a coordinator probes each peer's `Known` before handing
   * it work, and a peer whose build differs is excluded rather than
   * asked to compute alongside a different version of the same job. */
  case Names(names: Vector[String], build: String = "")
  /** one epoch's partial, and the partition's own extent so far — the
   * coordinator needs the second to compute the watermark, which in a
   * stream is the MINIMUM over the partitions rather than the maximum
   * over everything */
  case Epoch(bytes: Array[Byte], extent: Vector[Flows.Extent], drained: Boolean,
             /** elements consumed so far — the position a seekable
              * sink's replacement opens at */
             consumed: Long = 0L)
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
   * A JOB'S CONSIDERED "NO" for a partition (specs/federation.md,
   * stage 1). Thrown from a flow's partition thunk when this process
   * must not compute that partition — it is another party's — and
   * answered as `Resp.Failed`, which the coordinator does not retry
   * elsewhere. Distinct from any other throwable on purpose: a crash
   * in-process still propagates (and is retried as a death), a
   * refusal never does.
   */
  final class Refused(why: String) extends RuntimeException(why)

  /**
   * A WORKER THAT ANSWERS TO ITS OWNER (specs/federation.md, stage 2).
   *
   * Stage 1 showed two parties computing one answer without either
   * one's records leaving. It did so with workers that run ANY job
   * their build knows, for ANYBODY who asks — which is fine between
   * processes one person started, and is the whole question between
   * organisations. This is the door.
   *
   * Two checks, in this order and both BEFORE the request reaches the
   * job:
   *
   *   - is the caller a coordinator this party recognises?
   *   - is this a job this party allows?
   *
   * The order matters for what a refusal tells an outsider. An
   * unrecognised caller learns only that it is not recognised — never
   * which jobs the party allows, which would be a directory of its
   * business handed to a stranger.
   *
   * WHY IT IS A `Serve` WRAPPER AND NOT A FIELD ON `Req`. The caller
   * is a property of the CONNECTION, not of each message: a socket
   * authenticates once and every request on it comes from the party
   * that authenticated. Putting an identity in `Req` would put it
   * where the sender controls it, which is the one place it must not
   * be. Here the transport supplies it and the message cannot argue.
   *
   * A REFUSAL IS `Resp.Failed`, not a throw, and that is the same
   * distinction stage 1 had to make: the coordinator does not carry a
   * refusal to another worker, so a party that says no is not treated
   * as a party that died. `Cluster.Refused` says it in process, and
   * this answers it directly.
   *
   * WHAT THIS IS NOT. It is not authentication: `caller` is whoever
   * the transport says it is, and establishing that is
   * `okay-security`'s business (a `Capability` narrows without the
   * issuer, which is the shape a delegated submission wants). This is
   * the AUTHORISATION half, and it is deliberately dull — a set
   * membership test in front of a door that had none.
   */
  def guarded(jobs: Set[String], coordinators: Set[String])
             (caller: String)(base: Serve): Serve = req =>
    if !coordinators.contains(caller) then
      Resp.Failed(s"this party does not recognise the coordinator '$caller'")
    else
      named(req) match
        case Some(job) if !jobs.contains(job) =>
          Resp.Failed(s"this party does not run the job '$job'; it allows " +
            jobs.toVector.sorted.mkString("[", ", ", "]"))
        case _ => base(req)

  /** the job a request names, where it names one. `Advance` and
   * `Close` name a SESSION, which was admitted when it was opened —
   * so the job check has already happened for them, and the
   * coordinator check above still has not. */
  private def named(req: Req): Option[String] = req match
    case Req.Extent(job, _, _, _, _) => Some(job)
    case Req.Run(job, _, _, _, _, _) => Some(job)
    case Req.Open(job, _, _, _, _, _, _, _) => Some(job)
    case Req.Known | Req.Advance(_, _, _, _) | Req.Close(_) => None

  /**
   * THE DOOR CHECK (specs/federation.md, stage 3): a job whose
   * partial Schema this party would produce differently from what
   * the coordinator's own `Digest` expects is refused before a byte
   * of it runs — the same shape `guarded` uses for identity, applied
   * to the wire's shape instead. OPT IN, like `guarded`: a party that
   * wraps its `Serve` with this pays for the check; one that does not
   * pays nothing, and the coordinator's digest (always sent, computed
   * once per run) simply goes unread.
   *
   * An EMPTY digest skips the check — a coordinator built before this
   * box, or one that never populates it, changes nothing for a party
   * that opts in; a job NOT FOUND is left to the ordinary "no job
   * named" answer downstream rather than duplicated here.
   */
  def schemaChecked(base: Serve): Serve = req =>
    digestOf(req) match
      case None => base(req)
      case Some((_, _, digest)) if digest.isEmpty => base(req)
      case Some((jobName, params, digest)) =>
        Jobs.find(jobName) match
          case None => base(req)
          case Some(job) =>
            job.wireSchema(params) match
              case Left(why) => Resp.Failed(s"parameters for '$jobName': $why")
              case Right(mine) =>
                Codecs.cbor(summon[Schema[Digest]]).decode(digest) match
                  case Left(why) => Resp.Failed(s"the coordinator's schema digest for '$jobName': $why")
                  case Right(theirs) =>
                    val v = Digest.compare(mine, theirs).backward
                    if v.compatible then base(req)
                    else Resp.Failed(s"the coordinator cannot decode '$jobName''s partial:\n" +
                      v.reasons.mkString("\n"))

  private def digestOf(req: Req): Option[(String, Array[Byte], Array[Byte])] = req match
    case Req.Extent(job, params, _, _, digest) => Some((job, params, digest))
    case Req.Run(job, params, _, _, _, digest) => Some((job, params, digest))
    case Req.Open(job, params, _, _, _, _, _, digest) => Some((job, params, digest))
    case _ => None

  /**
   * Run a registered job across workers, partition i on worker
   * `i % workers.length`.
   *
   * The answer is the same `Run[R]` a local fan produces, and that is
   * the whole claim of this stage: the drop count and the merged
   * count mean the same thing whether the partitions ran here or on
   * four machines.
   */
  def run[P, R](job: Job[P, R], p: P, parts: Int, workers: Vector[Serve],
                /** consecutive failures that bury a worker — see
                 * `Living.Tolerance` for the default and dataflow-netem
                 * for what it means on a lossy wire */
                tolerance: Int = Living.Tolerance)
               (using Scheduler): Run[R] ! Async =
    require(parts > 0, "a job has at least one partition")
    require(workers.nonEmpty, "a job needs at least one worker")
    require(tolerance > 0, "a worker is buried after at least one failure")
    val encoded = Codecs.cbor(job.params).encode(p)
    val sink = job.sink(p)
    val living = Living(workers.length, tolerance)
    // THE DOOR CHECK'S OTHER HALF (specs/federation.md, stage 3):
    // computed ONCE per run and attached to every request that names
    // this job. A party who never opts into `schemaChecked` never
    // decodes it; the cost of ALWAYS sending it is one small,
    // structural CBOR encode per run, not per partition or element.
    val digest = Codecs.cbor(summon[Schema[Digest]]).encode(Digest.of(sink.wire))

    val bounds: Vector[Vector[Bounds]] ! Async =
      if sink.times.isEmpty then pure[Async, Vector[Vector[Bounds]]](Vector.fill(parts)(Vector.empty))
      else
        Flows.spread(parts)(i =>
          ask(workers, living, i, Req.Extent(job.name, encoded, i, parts, digest)) match
            case Resp.Extents(cols) => cols
            case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
            case other => throw IllegalStateException(s"partition $i answered $other to a pre-pass"))
          .map(Flows.edges)

    bounds.flatMap: bs =>
      Flows.spread(parts) { i =>
        ask(workers, living, i, Req.Run(job.name, encoded, i, parts, bs(i), digest)) match
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
     * measurement — and dataflow-netem then measured what it does on
     * a LOSSY wire, where a failure is a packet and not a machine:
     * the count converts loss into burials from about 30% loss up,
     * and burying a worker whose wire is merely lossy is what ends a
     * run. It is a default for a wire that is not known to be lossy;
     * `Cluster.run` and `Cluster.stream` take it as a parameter.
     * The original reasoning: one is what the engine did and could not survive a
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
   *
   * A `Refused` thrown by the job is answered as `Resp.Failed`, here
   * as over a socket (`Served.handle` does the same for every
   * throwable): a refusal is the worker's considered answer and must
   * reach the coordinator as one, or `ask` would carry it to the next
   * worker as if the first had died — which for a federated job means
   * asking party A to compute party B's share (specs/federation.md).
   */
  val local: Serve = req =>
    try serving(req)
    catch case r: Refused => Resp.Failed(r.getMessage)

  private val serving: Serve = {
    case Req.Known => Resp.Names(Jobs.names)
    case Req.Open(name, params, part, of, session, from, epoch, _) =>
      Jobs.find(name) match
        case None => Resp.Failed(s"no job named '$name' in this build; it knows ${Jobs.names}")
        case Some(job) =>
          job.openAt(params, part, of, from, epoch) match
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
    case Req.Extent(name, params, part, of, _) =>
      Jobs.find(name) match
        case None => Resp.Failed(s"no job named '$name' in this build; it knows ${Jobs.names}")
        case Some(job) =>
          job.extentAt(params, part, of) match
            case Right(cols) => Resp.Extents(cols)
            case Left(why) => Resp.Failed(s"parameters for '$name': $why")
    case Req.Run(name, params, part, of, bounds, _) =>
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
                   journal: Checkpoint = Checkpoint.none, term: Long = 0L,
                   tolerance: Int = Living.Tolerance)
                  (using Scheduler): Run[R] ! Async =
    require(parts > 0, "a job has at least one partition")
    require(workers.nonEmpty, "a job needs at least one worker")
    require(take > 0, "an epoch advances by at least one element")
    require(tolerance > 0, "a worker is buried after at least one failure")
    val encoded = Codecs.cbor(job.params).encode(p)
    val sink = job.sink(p)
    val living = Living(workers.length, tolerance)
    // the door check's other half (specs/federation.md, stage 3) —
    // see `Cluster.run`'s identical line
    val digest = Codecs.cbor(summon[Schema[Digest]]).encode(Digest.of(sink.wire))
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
    // A RESCALE (stage 13) is a resume whose new width differs from
    // the journal's. It mints FRESH session ids rather than inheriting
    // the predecessor's: those sessions were reading the OLD cut's
    // slices, and reusing them under the new cut would read the wrong
    // elements. (A same-width resume still inherits, so it strands
    // none — the property TestResume pins.)
    val rescaling: Boolean = resuming.exists(_.seen.length != parts)
    val base =
      if rescaling then System.nanoTime()
      else resuming.fold(System.nanoTime())(_.base)
    val sessions = Vector.tabulate(parts)(i => base + i)
    /**
     * WHERE A SESSION OPENS — and it is the same question on a resume
     * and on a replacement worker mid-run, which is why it is one
     * function. A seekable sink's session opens at the position the
     * last absorbed epoch left it, already at that epoch; a windowed
     * sink's opens at zero and replays (stage 11 box 2).
     *
     * A RESCALE opens at a position too, even for a windowed sink: the
     * position is the clean global PREFIX the old run consumed, the
     * same for every new partition, and a striped source skips it and
     * re-stripes the rest (stage 13). So a re-cut reads only the
     * suffix — no replay — and the fold carries the prefix.
     */
    def opening(i: Int, positions: Vector[Long], absorbed: Int,
                seen: Vector[Vector[Flows.Extent]], marks: Vector[Seek]): Req.Open =
      if (sink.seekable || rescaling) && positions.nonEmpty then
        Req.Open(job.name, encoded, i, parts, sessions(i), positions(i), absorbed, digest)
      else
        val at = seeking(i, seen, marks)
        if at < 0 then Req.Open(job.name, encoded, i, parts, sessions(i), digest = digest)
        else Req.Open(job.name, encoded, i, parts, sessions(i),
          marks(at).positions(i), marks(at).epoch, digest)

    // -- ROAD B: where a sink with a HORIZON opens -------------------
    //   (specs/dataflow.md, stage 11 box 2b)
    //
    // A windowed partition keeps its open panes inside itself, so a
    // session at a position has none of them and every session
    // replayed from zero. It does not have to. Partition i's own
    // operator closes a pane when `start + size <= max_i - lateness`,
    // so every pane STILL OPEN there starts above `max_i -
    // sink.horizon` and holds only elements above that point. A
    // session opened at an epoch where partition i's maximum was
    // already that far back therefore skips nothing an open pane
    // wants: the panes that DID hold a skipped element are all closed
    // again before the requested epoch, and a catch-up discards
    // exactly those (Job's `advance`).
    //
    // The cut is per PARTITION, not global, because the operator that
    // holds the panes is per partition — a global watermark would be
    // the slowest partition's clock and would refuse every seek on a
    // feed whose partitions cover different times.
    //
    // A mark records the HIGHEST event time over the sink's time
    // columns and the cut is taken from the LOWEST, so a sink reading
    // two clocks (`and`) is offered a mark only when both are past it.
    def high(e: Vector[Flows.Extent]): Long =
      if e.isEmpty then Long.MinValue else e.map(_.max).max
    def low(e: Vector[Flows.Extent]): Long =
      if e.isEmpty then Long.MinValue else e.map(_.max).min
    /** the newest mark partition `i` may open at, or -1 for none */
    def seeking(i: Int, seen: Vector[Vector[Flows.Extent]], marks: Vector[Seek]): Int =
      if sink.horizon <= 0L || marks.isEmpty then -1
      else
        val now = low(seen(i))
        if now == Long.MinValue then -1
        else
          val cut = now - sink.horizon
          marks.lastIndexWhere(m => m.maxes(i) != Long.MinValue && m.maxes(i) <= cut)

    /**
     * THE MARKS THIS EPOCH LEAVES BEHIND, pruned.
     *
     * Everything older than the oldest partition's own target is
     * unreachable for ever — a cut only rises — so it is dropped. The
     * cap is a second bound for a feed whose partitions run at wildly
     * different event times: dropping the OLDEST mark costs a
     * partition seek distance and can never cost correctness, since
     * the fallback is the replay from zero that was the only road
     * before this box.
     */
    def marking(seen: Vector[Vector[Flows.Extent]], marks: Vector[Seek],
                round: Int, stood: Vector[Long]): Vector[Seek] =
      if sink.seekable || sink.horizon <= 0L then Vector.empty
      else
        val grown = marks :+ Seek(round, stood, Vector.tabulate(parts)(i => high(seen(i))))
        val oldest = (0 until parts).map(i => seeking(i, seen, grown)).min
        val kept = if oldest <= 0 then grown else grown.drop(oldest)
        if kept.length <= Seeks then kept else kept.takeRight(Seeks)

    def commit(round: Int, st: sink.S, seen: Vector[Vector[Flows.Extent]],
               drops: Long, merged: Long, positions: Vector[Long],
               marks: Vector[Seek], over: Boolean = false): Unit =
      journal.save(round,
        folded.encode(Folded(round, seen, drops, merged, held.encode(st), base, term, over,
          positions, marks)))

    // NO UPFRONT OPEN. The first `Advance` finds no session and opens
    // one, which is the identical path a replacement worker takes —
    // so the recovery road IS the road, exercised on every run rather
    // than only when something has died.
    locally:
      def epoch(state: sink.S, seen: Vector[Vector[Flows.Extent]], drops: Long, merged: Long,
                positions: Vector[Long], marks: Vector[Seek], below: Long,
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
          advancing(workers, living, i, opening(i, positions, round - 1, seen, marks),
            Req.Advance(sessions(i), take, bs(i), round)) match
            case e: Resp.Epoch => e
            case Resp.Failed(why) => throw IllegalStateException(s"partition $i: $why")
            case other => throw IllegalStateException(s"partition $i answered $other to an advance")
        }.flatMap { es =>
          val ws = es.zipWithIndex.map { (e, i) =>
            Codecs.cbor(sink.wire).decode(e.bytes) match
              // SIFTED, and `below` is `Long.MinValue` for every run
              // that is not a windowed re-cut — which keeps every
              // pane, so this costs those runs one comparison a pane
              // and changes nothing. After a re-cut it is the
              // watermark the coordinator's retirement used, and it
              // is what stops a replayed element counting twice
              // (specs/dataflow.md stage 13 box 2).
              case Right(w) => sink.sift(w, below)
              case Left(why) => throw IllegalStateException(s"partition $i's partial: $why")
          }
          val grown = seen.indices.toVector.map(i => merge(seen(i), es(i).extent))
          val stood = es.map(_.consumed)
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
          val left = marking(grown, marks, round, stood)
          commit(round, next, grown, d, m, stood, left)
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
                  case Right(w) => sink.sift(w, below)
                  case Left(why) => throw IllegalStateException(s"partition $i's last partial: $why")
              }
              val end = sink.absorb(next, lw, Long.MaxValue)
              val dd = d + sink.drops(lw)
              val mm = m + sink.merged(lw)
              // the sweep is an epoch too, and it is the one that
              // records the run as OVER. Stage 8 said a coordinator
              // that died between the last Close and the answer could
              // resume here, ask for one more epoch and re-answer the
              // same value; it could not, and dataflow-durable's test
              // found it — see `Folded.done`.
              // THE SWEEP IS ITS OWN EPOCH, and the number has to
              // move: a writer that recognises a repeat by its epoch
              // number would drop the close's panes as a duplicate of
              // the last round's, which is exactly what happened the
              // first time this was written.
              sink.committed(round + 1)
              commit(round + 1, end, grown, dd, mm, stood, left, over = true)
              Run(sink.emit(end), dd, parts, 1, mm, living.retries, living.lost)
            }
          else epoch(next, grown, d, m, stood, left, below, round + 1)
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
        case None => epoch(sink.empty, Vector.fill(parts)(Vector.empty), 0L, 0L,
          Vector.fill(parts)(0L), Vector.empty, Long.MinValue, 1)
        case Some(f) =>
          held.decode(f.state) match
            case Left(why) =>
              throw IllegalStateException(s"the journal's fold at epoch ${f.epoch}: $why")
            case Right(st) if f.done =>
              // THE RUN WAS ALREADY OVER. Asking the workers for one
              // more epoch would open fresh sessions, replay the
              // whole source, discard every pane the catch-up closed
              // and hand back only what is open at the end — which
              // retires the tail panes a second time out of one
              // partition's half. The answer is in the state; take it
              // (dataflow-durable).
              pure[Async, Run[R]](
                Run(sink.emit(st), f.drops, parts, 1, f.merged, living.retries, living.lost))
            case Right(st) =>
              // everything a previous coordinator did AFTER this
              // epoch was lost with it and never happened — the one
              // thing a writer needs to know before the first pane of
              // the resumed run reaches it
              sink.recovered(f.epoch)
              // RESCALE (specs/dataflow.md, stage 13). The journal's
              // per-partition vectors are as WIDE as the run that wrote
              // them; a resume at a different `parts` re-cuts the source
              // and they no longer fit. The FOLD (`st`) carries across
              // untouched — it is keyed by (window, key), not by
              // partition, which is the whole reason a re-cut is
              // possible at all.
              //
              // Only a STRIPED source can be re-cut (`Job.rescalable`).
              // Its consumed elements are a clean global PREFIX [0, G)
              // after a lockstep run — the same set whatever the
              // partition count — where G is the SUM of the
              // per-partition positions (lockstep, so they tile [0, G)
              // with no gap). Each NEW partition j then skips the count
              // of its own elements that fall in [0, G) — ceil((G-j) /
              // parts) — and reads the rest, so the re-striped
              // partitions read exactly xs[G..] between them and the
              // fold carries xs[..G). No replay: the skips are exact.
              //
              // Two refusals, both to avoid a quietly wrong answer:
              //   - a CONTIGUOUS cut has no global prefix to skip (its
              //     positions are offsets into slices a re-cut redraws);
              //   - a WINDOWED sink keeps open panes in the WORKER, not
              //     in the journal (they are rebuilt by replay on a
              //     same-width resume), so a re-cut that does not replay
              //     would lose the panes open at the stop point. A
              //     keyed or fold sink keeps everything in the fold and
              //     rescales cleanly. (Windowed rescale needs those
              //     open panes journalled — stage 13's box 2.)
              val (seen0, pos0) =
                if f.seen.length == parts then (f.seen, f.positions)
                else if !job.rescalable then throw IllegalStateException(
                  s"job '${job.name}' cannot rescale ${f.seen.length} -> $parts partitions " +
                    "(specs/dataflow.md stage 13): its source is a contiguous cut, whose " +
                    "per-partition positions do not survive a re-cut. Only a striped source " +
                    "(Job.rescalable) has a clean global prefix to resume from.")
                else if sink.seekable then
                  val g = f.positions.sum
                  (Vector.fill(parts)(Vector.empty[Flows.Extent]),
                   Vector.tabulate(parts)(j => math.max(0L, (g - j + parts - 1) / parts)))
                else
                  // A WINDOWED RE-CUT REPLAYS ITS OPEN PANES RATHER
                  // THAN READING THEM OUT OF A JOURNAL (stage 13 box
                  // 2, the second answer). Every pane still open
                  // starts above the horizon mark's maximum, so new
                  // sessions opened at that mark's global prefix
                  // rebuild all of them — and the coordinator empties
                  // its partial copies and sifts out whatever reaches
                  // a pane it has already retired.
                  //
                  // The cut is GLOBAL here where box 2b's is per
                  // partition, and for the same reason it was per
                  // partition there: each new partition reads a stripe
                  // of the WHOLE range, so the clock a re-cut session
                  // runs on is the stream's, not one slice's.
                  val hi = f.seen.map(e => if e.isEmpty then Long.MinValue else e.map(_.max).max).max
                  val lo = f.seen.map(e => if e.isEmpty then Long.MinValue else e.map(_.max).min).min
                  val cut = if lo == Long.MinValue then Long.MinValue else lo - sink.horizon
                  val target = f.marks.reverseIterator.find(m =>
                    m.positions.length == f.seen.length &&
                      m.maxes.max != Long.MinValue && m.maxes.max <= cut)
                  val m = target.getOrElse(throw IllegalStateException(
                    s"job '${job.name}' cannot rescale a WINDOWED sink yet " +
                      "(specs/dataflow.md stage 13, box 2): its open panes live in the worker, " +
                      "and a re-cut rebuilds them by replaying from the horizon mark — but no " +
                      s"mark in the journal is a horizon (${sink.horizon}) below where the run " +
                      s"stands (event time $hi, cut $cut). A run too young to have one, or a " +
                      "sink with no horizon at all, still has to keep its width."))
                  val gm = m.positions.sum
                  (Vector.fill(parts)(Vector.empty[Flows.Extent]),
                   Vector.tabulate(parts)(j => math.max(0L, (gm - j + parts - 1) / parts)))
              // THE MARKS COME BACK TOO, and only when they still
              // fit: a rescale re-cuts the partitions and a mark's
              // positions are the OLD cut's, so a re-cut starts
              // collecting fresh ones.
              val marks0 =
                if f.seen.length == parts then f.marks.filter(_.positions.length == parts)
                else Vector.empty
              // A WINDOWED RE-CUT, AND THE TWO RULES THAT MAKE IT
              // EXACT (specs/dataflow.md stage 13 box 2). The open
              // panes here are partial copies of panes the replay is
              // about to rebuild in full, so they go; and everything
              // the replay hands that belongs to a pane ALREADY
              // RETIRED — at or below the watermark retirement used —
              // is sifted out for the rest of the run, because after
              // the resume no such pane can be contributed to again
              // except by a replayed element that has been counted.
              val recut = f.seen.length != parts && !sink.seekable
              val below = if recut then watermark(f.seen, sink.slack) else Long.MinValue
              val st0 = if recut then sink.reopen(st) else st
              epoch(st0, seen0, f.drops, f.merged, pos0, marks0, below, f.epoch + 1)

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
        // the term goes two ways: into the FENCE, which refuses a
        // commit once the lease is gone, and into every RECORD, so a
        // stale commit that got past the fence is shadowed rather
        // than read back (dataflow-durable)
        stream(job, p, parts, workers, take, Checkpoint.fenced(term, lease, journal), term)
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

  /**
   * HOW MANY SEEK MARKS A JOURNAL RECORD CARRIES AT MOST
   * (specs/dataflow.md, stage 11 box 2b).
   *
   * Pruning by the oldest partition's target already bounds this on
   * any feed whose partitions keep pace; the cap bounds it on one
   * where they do not. A mark is three numbers per partition, so 64
   * of them at 8 partitions is under 16 KB — the same order as the
   * one-off 16 KB the measurement priced the seek at.
   */
  private val Seeks = 64

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
