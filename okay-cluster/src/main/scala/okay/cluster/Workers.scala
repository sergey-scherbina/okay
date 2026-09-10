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

enum Resp:
  case Extents(cols: Vector[Flows.Extent])
  /** the partial, encoded by the sink's own `wire` — the coordinator
   * decodes it with the same Schema, and nothing in between looks
   * inside */
  case Partial(bytes: Array[Byte])
  case Names(names: Vector[String])
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
        Run(sink.result(ws), sink.drops(ws), parts, 1, sink.merged(ws), living.retries)

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
          try workers(w)(req)
          catch case t: Throwable =>
            living.bury(w)
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
  private final class Living(n: Int):
    private var alive: Vector[Int] = (0 until n).toVector
    private var buried: Long = 0L

    /** a survivor for this attempt, or None when there are none */
    def pick(turn: Int): Option[Int] = synchronized {
      if alive.isEmpty then None else Some(alive(math.floorMod(turn, alive.length)))
    }

    def bury(w: Int): Unit = synchronized {
      if alive.contains(w) then { alive = alive.filterNot(_ == w); buried += 1 }
    }

    /** how many attempts were lost to a dead worker — reported so a
     * suite can assert that recovery HAPPENED rather than infer it
     * from the answer being right */
    def retries: Long = synchronized(buried)

  /**
   * A worker made of a registry: answer a request by looking the job
   * up and running it. This is what a served process does, and it is
   * also what an in-process worker does — one function, so a test
   * without sockets exercises the same code a socket does.
   */
  val local: Serve = {
    case Req.Known => Resp.Names(Jobs.names)
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
