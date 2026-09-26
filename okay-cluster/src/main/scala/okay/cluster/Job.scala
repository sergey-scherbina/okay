package okay.cluster

import okay.*
import okay.codec.{Codecs, Json, Schema}

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

  /**
   * THE ANSWER'S OWN SCHEMA (okay-pool, specs/cluster-pool.md stage 1).
   *
   * Everything else on this class can be driven from the OUTSIDE
   * without ever naming `R`: `wireSchema`/`extentAt`/`openAt`/
   * `partialAt` all take and answer bytes, which is what lets
   * `Jobs.find` hand back a `Job[?, ?]` a worker can still serve. A
   * pool's `GET /pool/runs/{id}` needs the one thing those methods do
   * not give — the PRESENTED value, `Run[R].value`, turned into JSON
   * for a caller who does not compile against this job's types — and
   * that needs a `Schema[R]` from somewhere. There is no route to one
   * without asking for it: `R` is a free type parameter this class
   * uses only through `sink(p)`'s `emit`, and a `Wire`'s own Schema
   * describes the PARTIAL (`W`/`Acc`), not the answer.
   */
  def answer: Schema[R]

  /** the plan, for these parameters, cut into `parts` partitions */
  def flow(p: P, parts: Int): Flow[A]

  /**
   * CAN THIS JOB CHANGE ITS PARTITION COUNT MID-STREAM?
   * (specs/dataflow.md, stage 13.)
   *
   * True only when `flow` STRIPES its source — assigns global element
   * `i` to partition `i % parts`, reading in order — so that after a
   * lockstep epoch the consumed elements are a clean global PREFIX,
   * the same set whatever the partition count. A re-cut then skips
   * that prefix and re-stripes the rest, and the coordinator's fold
   * (keyed by window, not by partition) carries across untouched.
   *
   * A contiguous cut (`Flow.slices`) cannot: its per-partition
   * positions are offsets into slices a re-cut redraws, so there is
   * no prefix to skip. Such a job leaves this false and the engine
   * refuses a width change rather than compute a wrong answer.
   */
  def rescalable: Boolean = false

  /** what the plan computes, and how its partials travel */
  def sink(p: P): Wire[A, R]

  // ---------------------------------------------------------------
  // What a WORKER calls. Both take and answer bytes, which is what
  // lets the registry hold `Job[?, ?]` and a worker serve a job whose
  // parameter and answer types it cannot name.
  // ---------------------------------------------------------------

  /**
   * THIS JOB'S OWN PARTIAL SCHEMA, DECODED FOR THESE PARAMETERS
   * (specs/federation.md, stage 3 — "schema at the door").
   *
   * A `Wire#wire` can depend on the parameters (a window's size, a
   * key's own type), so the check needs `p` decoded first. Used at
   * the door: a party compares this against the coordinator's own
   * `okay.codec.Digest` of the same job before running a byte of it.
   */
  final def wireSchema(bytes: Array[Byte]): Either[String, Schema[?]] =
    Codecs.cbor(params).decode(bytes).map(p => sink(p).wire)

  /** the pre-pass over one partition: three longs per event-time
   * column, which is everything the coordinator needs before any
   * partition may start */
  final def extentAt(bytes: Array[Byte], part: Int, of: Int)
  : Either[String, Vector[Flows.Extent]] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val s = sink(p)
      if s.times.isEmpty then Vector.empty
      else Scope.using(sc => Flows.extent(Flows.partition(flow(p, of), part, 0L, sc), s.times))
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
  final def openAt(bytes: Array[Byte], part: Int, of: Int,
                   from: Long = 0L, epoch: Int = 0): Either[String, Session] =
    Codecs.cbor(params).decode(bytes).map { p =>
      val s = sink(p)
      new Session:
        // OPENED AT A POSITION (stage 11 box 2): `from` elements in,
        // already at `epoch`, so the first `advance` asks for the next
        // one and nothing is replayed. Only a seekable sink's
        // coordinator asks for this; a windowed one opens at zero.
        // the partition's life is the SESSION's: whatever a stage holds for
        // it is given back at `finish`, which every close goes through
        private val scope = Scope()
        private var rest: Chunks[A] = Flows.partition(flow(p, of), part, from, scope)
        private var state: s.P | Null = null
        private var extent: Vector[Flows.Extent] = Vector.empty
        private var consumed: Long = from

        private var at: Int = epoch

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
                consumed += c.length
                rest = r
              case None => drained = true
          // an epoch hands over what the operator has closed SO FAR;
          // `finish` is the same call the batch driver makes, and the
          // panes still open stay in the operator for the next epoch
          Resp.Epoch(Codecs.cbor(s.wire).encode(s.peek(st)), extent, drained, consumed)

        def finish(): Resp =
          try
            val st = if state == null then s.start(Vector.empty) else state.nn
            Resp.Epoch(Codecs.cbor(s.wire).encode(s.finish(st)), extent, true, consumed)
          finally scope.close()
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
      Scope.using(sc => Chunks.foldLeft(Flows.partition(flow(p, of), part, 0L, sc))(())((_, a) => s.step(st, a)))
      Codecs.cbor(s.wire).encode(s.finish(st))
    }

  /**
   * COORDINATE THIS JOB FROM OUTSIDE, NAMING NEITHER `P` NOR `R`
   * (okay-pool, specs/cluster-pool.md stage 1 — "the run id is the
   * journal name").
   *
   * A pool holds `Job[?, ?]` values, the same existential `Jobs.find`
   * already answers, and must still call `Cluster.leading` on one and
   * report its answer as JSON. That needs `P` and `R` bound to the
   * SAME job, which only a method living here — where `this.type`
   * fixes both — can do without a cast: `wireSchema`/`extentAt`/
   * `partialAt` are the existing proof that the pattern works.
   *
   * Decoding `paramsJson` happens BEFORE anything is scheduled, so a
   * bad submission is a `Left` an HTTP door answers 400 with, never a
   * failed `Async` program. `answer` (above) is what turns the run's
   * `R` into `Json` on the way out, so the `Either`'s `Right` needs no
   * `R` in its own type either — `Job.Answer` names none.
   */
  final def lead(paramsJson: Json, parts: Int, peers: Vector[Cluster.Serve], take: Int,
                 checkpoint: Checkpoint, lease: Lease,
                 /** peers re-resolved at every epoch boundary
                  * (specs/cluster-pool.md, stage 5) — see `Cluster.stream`.
                  * `None` is the fixed `peers` above, every round. */
                 resolve: Option[() => Vector[Cluster.Serve] ! Async] = None,
                 onRefusedRescale: (Int, Int) => Unit = (_, _) => ())
                (using okay.Scheduler): Either[String, Option[Job.Answer] ! Async] =
    Codecs.json(params).decode(paramsJson).map { p =>
      Cluster.leading(this, p, parts, peers, take, checkpoint, lease, resolve, onRefusedRescale).map(_.map { run =>
        Job.Answer(Codecs.writeJson(run.value)(using answer), run.dropped, run.merged, run.retried, run.failed)
      })
    }

  /**
   * THE ANSWER OF A FINISHED RUN, FROM ITS JOURNAL ALONE — no worker
   * asked, no lease taken (okay-pool, specs/cluster-pool.md stage 1,
   * "the run id is the journal name"). A `Folded` with `done = true`
   * already carries everything the answer needs: `state` is the
   * coordinator's own fold, describable by `sink(p).state` for the
   * SAME reason `wireSchema` can describe a partial without a live
   * value in hand.
   *
   * `retried`/`failed` come back zero here, and that is a stated
   * limit rather than an oversight: `Folded` does not carry them —
   * they are `Living`'s own counters, scoped to one `Cluster.leading`
   * ATTEMPT, not persisted across a resume, exactly as a resumed
   * `Cluster.stream` already starts a fresh `Living` of its own. A
   * caller reading a bare journal was never inside any attempt, so it
   * has nothing else to report.
   */
  final def answerOf(paramsJson: Json, folded: Folded): Either[String, Job.Answer] =
    Codecs.json(params).decode(paramsJson).flatMap { p =>
      val s = sink(p)
      Codecs.cbor(s.state).decode(folded.state).map { st =>
        Job.Answer(Codecs.writeJson(s.emit(st))(using answer), folded.drops, folded.merged, 0L, 0L)
      }
    }

object Job:
  /** a finished run, with its answer already JSON — the shape `lead`
   * hands back so a caller never has to name this job's `R` */
  final case class Answer(value: String, dropped: Long, merged: Long, retried: Long, failed: Long)
    derives Schema

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
