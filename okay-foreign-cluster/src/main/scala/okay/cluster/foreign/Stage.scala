package okay.cluster.foreign

import okay.{Chunk, Chunks}
import okay.cluster.{Cluster, Flow}
import okay.foreign.{Condition, ForeignWorker, Pool, Pools, PyWorkers}
import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

/**
 * A batch of typed rows through something outside the JVM, as one frame
 * (specs/foreign-map-reduce.md). `ForeignStage` is the one implementation
 * over every wire language (foreign-one-runtime); a test's fake is another.
 */
trait Batcher[A, B]:
  /** what a failure names: `py:scoring:scale`, `r:model:fit` */
  def name: String
  def apply(rows: Vector[A]): Either[Batcher.Failed, Vector[B]]

object Batcher:
  final case class Failed(kind: String, message: String)

  /** the kinds that are the WIRE's, not the function's (the line
   * okay-foreign-workflow's oracle draws): retried on a fresh
   * interpreter; anything else is the function's own answer */
  val transient: Set[String] = Set("WorkerDied", "timeout", "WorkerUnavailable", "WireError")

/**
 * THE STAGE: a `Flow.Local` node whose chunk transformer rechunks to
 * `batch` rows and sends each chunk through the batcher. Nothing in the
 * engine — partitions, epochs, resume, the exchange — learns a new node:
 * "anything per-partition" was already a chunk transformer.
 *
 * Two failures, two roads, both the cluster's own (Workers.scala):
 *  - the FUNCTION's failure is thrown as `Cluster.Refused` — the worker's
 *    considered answer, which the coordinator does not carry to the next
 *    worker as if this one had died. A deterministic error is not retried
 *    into a wrong answer.
 *  - a WIRE failure is retried here, `attempts` times on a fresh
 *    interpreter (a chunk is a pure map, so at-least-once is exact); past
 *    that it is thrown as an ordinary error, which the coordinator reads
 *    as a dead worker and recomputes the partition on a survivor.
 */
object Stage:
  /** rows per frame: one Python round trip per this many rows */
  val Batch = 4096
  /** interpreters per stage per worker JVM, at most */
  val Workers: Int = math.max(1, Runtime.getRuntime.availableProcessors)

  def through[A, B](in: Flow[A], batcher: Batcher[A, B], batch: Int, attempts: Int): Flow[B] =
    require(batch > 0, "a batch holds at least one row")
    require(attempts > 0, "a chunk is attempted at least once")
    Flow.Local(in, batcher.name, (c: Chunks[A]) =>
      Chunks.mapWith(Chunks.rechunk(c)(batch))(chunk => one(batcher, chunk, attempts)))

  private def one[A, B](b: Batcher[A, B], chunk: Chunk[A], attempts: Int): Chunk[B] =
    ArraySeq.untagged.from(Attempts.run(b.name, attempts)(b(chunk.toVector)))

/** the two failure roads, shared by the map stage and the reduce: a
 * transient failure retried `attempts` times, then an ordinary error (a
 * dead worker to the coordinator); any other failure a `Cluster.Refused`
 * naming the stage (the worker's considered answer, not retried) */
object Attempts:
  def run[X](name: String, attempts: Int)(f: => Either[Batcher.Failed, X]): X =
    @tailrec def go(left: Int): X =
      f match
        case Right(x) => x
        case Left(fl) if Batcher.transient(fl.kind) =>
          if left > 1 then go(left - 1)
          else throw IllegalStateException(
            s"the stage '$name' could not reach its interpreter in $attempts attempts (${fl.kind}: ${fl.message})")
        case Left(fl) =>
          throw Cluster.Refused(s"the stage '$name' failed: ${fl.kind}: ${fl.message}")
    go(attempts)


extension [A](flow: Flow[A])
  /** each chunk through `batcher`, `batch` rows at a time */
  def through[B](batcher: Batcher[A, B], batch: Int = Stage.Batch, attempts: Int = 3): Flow[B] =
    Stage.through(flow, batcher, batch, attempts)

  /** THE ONE MAP: `fn` of `module`, in whatever language the module's type
   * says — a `PyModule` runs in Python, an `RModule` in R, a `JvmModule`
   * here — through the `Engine` in scope for it (stage 3) */
  def mapIn[B](module: Any, fn: String, batch: Int = Stage.Batch, workers: Int = Stage.Workers)
              (using e: Engine[module.type], sa: okay.codec.Schema[A], sb: okay.codec.Schema[B]): Flow[B] =
    Stage.through(flow, e.batcher[A, B](module, fn, workers), batch, 3)

  /** a map whose far function also receives a MODEL — `fn(frame, model)`,
   * the one `Model.in` made, materialised once per interpreter (stage 4) */
  def mapModel[B](model: Model, fn: String, batch: Int = Stage.Batch)
                 (using sa: okay.codec.Schema[A], sb: okay.codec.Schema[B]): Flow[B] =
    Stage.through(flow, model.batcher[A, B](fn), batch, 3)

  /** a STATEFUL stage in whatever language the module's type says:
   * `open`/`step`/`finish` on the far side, the state kept for the
   * partition (stage 4) */
  def statefulIn[B](module: Any, open: String, step: String, finish: String,
                    batch: Int = Stage.Batch, workers: Int = Stage.Workers)
                   (using st: Stateful[module.type], sa: okay.codec.Schema[A], sb: okay.codec.Schema[B]): Flow[B] =
    Stateful.through(flow, st.streamer[A, B](module, open, step, finish, workers), batch)

  /** the map in Python: `fn` in `module` takes the frame (a dict of
   * lists, or the `pyarrow.Table` under `@okay.arrow`) and answers one of
   * `B`'s columns; the frame crosses as Arrow where the python has pyarrow */
  def mapPy[B](module: okay.foreign.PyModule, fn: String, python: String = "python3",
               batch: Int = Stage.Batch, workers: Int = Stage.Workers)
              (using okay.codec.Schema[A], okay.codec.Schema[B]): Flow[B] =
    Stage.through(flow, ForeignStage[okay.foreign.PyModule, A, B](Language.py(python), module, fn, workers), batch, 3)

  /** the map in R: `fn` in `module` takes a data.frame and answers one;
   * the frame crosses as Arrow where R has the `arrow` package */
  def mapR[B](module: okay.r.RModule, fn: String, rscript: String = "Rscript",
              batch: Int = Stage.Batch, workers: Int = Stage.Workers)
             (using okay.codec.Schema[A], okay.codec.Schema[B]): Flow[B] =
    Stage.through(flow, ForeignStage[okay.r.RModule, A, B](Language.r(rscript), module, fn, workers), batch, 3)

/**
 * The pools of a worker JVM, one per (language, interpreter, module): the
 * ONE pool with its routing (`okay.foreign.PyWorkers` over `okay.foreign.Pool`,
 * foreign-one-pool), shared by every stage, reduce, model, stateful stage,
 * handle and program that names the same module — Python's and R's alike.
 */
object Workers:
  def of(key: String, name: String, workers: Int, open: () => ForeignWorker): PyWorkers =
    Pools.get[PyWorkers](key)(PyWorkers.over(Pool[ForeignWorker](name, workers, open, _.alive, _.close())))(_.close())

  private[foreign] def dead(e: Throwable): Boolean =
    e.isInstanceOf[ForeignWorker.TimedOut] || Option(e.getMessage).exists(_.contains("DEAD"))

  /** one exchange on a borrowed worker; a death, or a worker that could not
   * be opened, is a transient condition for `Attempts` */
  def use[X](ws: PyWorkers, who: String)(f: ForeignWorker => Either[Condition, X]): Either[Condition, X] =
    try ws.use(f)
    catch
      case e: IllegalStateException if dead(e) => Left(Condition("WorkerDied", e.getMessage))
      case e: Exception => Left(Condition("WorkerUnavailable", s"$who could not be opened: ${e.getMessage}"))
