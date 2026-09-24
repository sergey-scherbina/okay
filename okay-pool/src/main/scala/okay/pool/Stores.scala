package okay.pool

import okay.cluster.{Checkpoint, Lease}

/**
 * WHERE EVERY SUBMISSION'S JOURNAL COMES FROM (specs/cluster-pool.md,
 * stage 1). One factory, keyed by an arbitrary name — the run id for
 * a run's own epoch loop, `"$id.meta"` for the small record `Pool`
 * keeps beside it — so a build wires exactly one storage technology,
 * the same shape `okay.cluster.Jobs` already has for jobs: register
 * at start-up (`PoolConf.store` names the class), and a caller who
 * never sets it gets a safe, named default rather than a crash.
 */
object Stores:
  private var factory: (String => (Checkpoint, Lease)) | Null = null

  /** called by a registrar class's own static initialiser — the
   * `Jobs.register` shape, one factory instead of many jobs */
  def set(f: String => (Checkpoint, Lease)): Unit = synchronized { factory = f }

  def get: Option[String => (Checkpoint, Lease)] = synchronized(Option(factory))

  /**
   * EVERY PROCESS'S OWN MEMORY — works alone, and ONLY alone.
   * `Pool.main` refuses to start on this once a second peer is
   * configured (specs/cluster-pool.md, "This needs a shared store,
   * and says so"): "any member can answer a `GET`" is exactly the
   * property a per-process cell does not have.
   */
  val memory: String => (Checkpoint, Lease) = _ => (Checkpoint.Memory(), Lease.solitary)

  /** for a test that runs several `Stores.set` calls across "processes"
   * simulated as objects in one JVM */
  def clear(): Unit = synchronized { factory = null }
