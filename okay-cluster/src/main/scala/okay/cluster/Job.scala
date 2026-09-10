package okay.cluster

import okay.Chunks
import okay.codec.{Codecs, Schema}

/**
 * WHAT A WORKER CAN BE ASKED TO RUN, FOUND BY NAME
 * (specs/dataflow.md, stage 4b).
 *
 * This is Claim 3 of the spec made into a type. A worker is asked for
 * a NAME and a Schema-encoded parameter, and it builds the plan
 * itself. No function, no closure, no serialized lambda, no class
 * loader trick, and therefore none of the failure modes that come
 * with them: a job that cannot be described is refused where it is
 * registered rather than inside a task on another machine.
 *
 * THE PRICE, SAID PLAINLY. Every worker must run the SAME ARTIFACT.
 * You cannot type a lambda into a console and have it run on the
 * cluster; you deploy a build and submit a name. That is the same
 * bargain Flink strikes with a submitted jar, made explicit here
 * rather than hidden behind a serializer that usually works.
 *
 * `flow` takes the partition count because a partition is a RECIPE
 * (`Flow.Src` holds thunks): the worker for partition 3 of 8 builds
 * only what it needs and reads only that, which is what makes a
 * source that is a file, an offset range or a table split behave the
 * same as an array slice.
 */
abstract class Job[P, R]:
  /** the element type the plan and the sink agree on */
  type A

  /** how a submission names this job */
  def name: String

  /** how its parameters travel */
  def params: Schema[P]

  /** the plan, for these parameters, cut into `parts` partitions */
  def flow(p: P, parts: Int): Flow[A]

  /** what the plan computes, and how its partials travel */
  def sink(p: P): Wire[A, R]

  // ---------------------------------------------------------------
  // What a WORKER calls. Both take and answer bytes, which is what
  // lets the registry hold `Job[?, ?]` and a worker serve a job whose
  // parameter and answer types it cannot name.
  // ---------------------------------------------------------------

  /** the pre-pass over one partition: three longs per event-time
   * column, which is everything the coordinator needs before any
   * partition may start */
  final def extentAt(bytes: Array[Byte], part: Int, of: Int)
  : Either[String, Vector[Flows.Extent]] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val s = sink(p)
      if s.times.isEmpty then Vector.empty
      else Flows.extent(Flows.partition(flow(p, of), part), s.times)
    }

  /**
   * OPEN A STREAMING SESSION over one partition
   * (specs/dataflow.md, stage 6a).
   *
   * The state that survives between epochs is exactly two things: how
   * much of the partition is left to read, and the sink's working
   * state `P`. Neither ever leaves the worker — what leaves each
   * epoch is a `W`, the same value a batch run hands over.
   */
  final def openAt(bytes: Array[Byte], part: Int, of: Int): Either[String, Session] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val s = sink(p)
      new Session:
        private var rest: Chunks[A] = Flows.partition(flow(p, of), part)
        private var state: s.P | Null = null
        private var extent: Vector[Flows.Extent] = Vector.empty

        private var at: Int = 0

        /**
         * Catch up to `epoch`, then answer for it.
         *
         * A session already there re-answers the same partial, which
         * is what a retry after a lost REPLY must get. One behind
         * replays and discards until it arrives — that is a fresh
         * worker rebuilding what a dead one held, and it is the same
         * loop either way.
         */
        def advance(take: Int, bounds: Vector[Bounds], epoch: Int): Resp =
          if epoch <= at then last.nn
          else
            while at < epoch - 1 do { step(take, bounds): Unit; at += 1 }
            val out = step(take, bounds)
            at = epoch
            last = out
            out

        private var last: Resp | Null = null

        private def step(take: Int, bounds: Vector[Bounds]): Resp =
          // the operator is built at the FIRST epoch, when the
          // coordinator's bounds are known — before that it has no
          // watermark to be seeded with
          val st = if state == null then { state = s.start(bounds); state.nn } else state.nn
          var read = 0
          var drained = false
          // ONE pull per chunk. `Chunks.pull` on an iterator-backed
          // source CONSUMES, so asking twice — once to test and once
          // to take — reads a chunk and throws it away. That is what
          // the first version of this loop did, and every test in
          // TestStream failed on it.
          while read < take && !drained do
            Chunks.pull(rest) match
              case Some((c, r)) =>
                var i = 0
                while i < c.length do { s.step(st, c(i)); i += 1 }
                extent = grow(extent, Flows.extent(Chunks.fromIterator(c.iterator), s.times))
                read += c.length
                rest = r
              case None => drained = true
          // an epoch hands over what the operator has closed SO FAR;
          // `finish` is the same call the batch driver makes, and the
          // panes still open stay in the operator for the next epoch
          Resp.Epoch(Codecs.cbor(s.wire).encode(s.peek(st)), extent, drained)

        def finish(): Resp =
          val st = if state == null then s.start(Vector.empty) else state.nn
          Resp.Epoch(Codecs.cbor(s.wire).encode(s.finish(st)), extent, true)
    }

  private def grow(a: Vector[Flows.Extent], b: Vector[Flows.Extent]): Vector[Flows.Extent] =
    if a.isEmpty then b else if b.isEmpty then a
    else a.indices.toVector.map { j =>
      Flows.Extent(math.max(a(j).max, b(j).max), math.min(a(j).min, b(j).min),
        math.max(a(j).back, b(j).back))
    }

  /** run one partition and hand back its partial, encoded */
  final def partialAt(bytes: Array[Byte], part: Int, of: Int, bounds: Vector[Bounds])
  : Either[String, Array[Byte]] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val s = sink(p)
      val st = s.start(bounds)
      Chunks.foldLeft(Flows.partition(flow(p, of), part))(())((_, a) => s.step(st, a))
      Codecs.cbor(s.wire).encode(s.finish(st))
    }

/**
 * One partition's streaming state, living on the worker between
 * epochs. It is deliberately opaque to the registry: `Jobs.find`
 * answers a `Job[?, ?]`, and a session is the only thing that can be
 * held without naming that job's types.
 */
trait Session:
  /** advance to (and answer for) the given epoch, replaying to catch
   * up if this session is behind it */
  def advance(take: Int, bounds: Vector[Bounds], epoch: Int): Resp
  def finish(): Resp

/** the sessions this worker is holding */
object Sessions {
  private val open = scala.collection.mutable.LongMap.empty[Session]
  def put(id: Long, s: Session): Unit = synchronized(open.update(id, s))
  def get(id: Long): Option[Session] = synchronized(open.get(id))
  def drop(id: Long): Unit = synchronized(open.remove(id): Unit)
  def count: Int = synchronized(open.size)
}

/**
 * The registry a worker looks in. Registration is an ordinary side
 * effect at start-up — a worker's `main` registers what that build
 * knows how to run, and a name it does not know is an answer, not a
 * crash.
 */
object Jobs {
  private val known = scala.collection.mutable.LinkedHashMap.empty[String, Job[?, ?]]

  def register(job: Job[?, ?]): Unit = synchronized {
    known.update(job.name, job)
  }

  def find(name: String): Option[Job[?, ?]] = synchronized(known.get(name))

  def names: Vector[String] = synchronized(known.keys.toVector)

  /** for tests that register the same job twice */
  def clear(): Unit = synchronized(known.clear())
}
