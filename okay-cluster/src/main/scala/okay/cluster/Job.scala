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
