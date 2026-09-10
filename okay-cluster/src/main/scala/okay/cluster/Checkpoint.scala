package okay.cluster

import okay.codec.Schema

/**
 * WHERE A COORDINATOR WRITES DOWN WHAT IT HAS FOLDED
 * (specs/dataflow.md, stage 8).
 *
 * Every stage from 6a to 6c ends with the same sentence: if the
 * COORDINATOR dies the run dies with it. The workers have been
 * recoverable since stage 5 — a partition is a recipe and a
 * replacement replays it — but the coordinator holds the folded state
 * and, until this, journalled nothing.
 *
 * WHAT MAKES THIS SMALL IS THE EPOCH LOOP BEING LOCK-STEP. Every
 * partition contributes exactly rounds 1..N before the coordinator
 * folds, so a checkpoint taken after absorbing round N is consistent
 * BY CONSTRUCTION: there is no alignment protocol, no barrier to
 * inject into the stream, nothing to reconcile between partitions.
 * Flink's checkpointing is an achievement because its operators run
 * asynchronously; this one is a `save` call because 6a chose the
 * other shape.
 *
 * AN INJECTABLE SEAM, NOT A DEPENDENCY. Two methods over bytes, so
 * okay-cluster's compile graph stays at okay-codec and the store is
 * the caller's: a file, okay-persist's compacted log, a row in a
 * table. `TestResume` binds it to okay-persist in TEST scope and
 * shows the assembly, which is the same arrangement okay-persist
 * already uses for TLS.
 */
trait Checkpoint:
  /** the coordinator has folded everything up to and including
   * `epoch`; `bytes` is what it holds */
  def save(epoch: Int, bytes: Array[Byte]): Unit

  /** the last epoch saved, if any */
  def latest: Option[(Int, Array[Byte])]

/**
 * WHO IS ALLOWED TO BE THE COORDINATOR (specs/dataflow.md, stage 10).
 *
 * Three methods and no dependency, exactly as `Checkpoint` is two
 * methods over bytes. okay-persist's `Election` answers all three —
 * `tryTakeover` returns the epoch that becomes the TERM, `leader`
 * says who holds it, `heartbeat` renews the lease — so the binding is
 * a handful of lines in test scope, and a caller with a different
 * store (a row with a lease column, a lock service) writes its own.
 *
 * THE TERM IS A FENCING TOKEN, and that is the whole reason `take`
 * answers a number rather than a boolean. Two coordinators over one
 * journal is worse than none: a paused leader that wakes believing it
 * still leads would commit over its successor's state, and the next
 * resume would read whichever landed last. The term is what lets the
 * journal refuse it.
 */
trait Lease:
  /** become the coordinator, if the seat is free — the TERM if taken */
  def take(): Option[Long]

  /** still the coordinator at this term? Called once per epoch,
   * before the commit, which is also where a lease is renewed */
  def held(term: Long): Boolean

  /** give the seat up; a lease that only expires may do nothing */
  def release(term: Long): Unit = ()

object Lease:
  /** the only candidate there is: leadership without an election, so
   * a run that has no second coordinator pays nothing for the seam */
  val solitary: Lease = new Lease:
    def take(): Option[Long] = Some(1L)
    def held(term: Long): Boolean = true

object Checkpoint:

  /** journals nothing and resumes nothing — the behaviour of every
   * stage before this one, kept as the default so a run that does not
   * ask for durability pays nothing for it */
  val none: Checkpoint = new Checkpoint:
    def save(epoch: Int, bytes: Array[Byte]): Unit = ()
    def latest: Option[(Int, Array[Byte])] = None

  /**
   * A checkpoint in this process's memory.
   *
   * It survives a coordinator, which is the thing being tested — a
   * new `Cluster.stream` over the same value resumes where the last
   * one stopped — and nothing else. Real durability is the caller's
   * store.
   */
  final class Memory extends Checkpoint:
    private var at: Option[(Int, Array[Byte])] = None
    private var saves: Long = 0L
    def save(epoch: Int, bytes: Array[Byte]): Unit = synchronized {
      at = Some((epoch, bytes)); saves += 1
    }
    def latest: Option[(Int, Array[Byte])] = synchronized(at)
    /** how many commits happened — so a test can assert the
     * coordinator journalled rather than infer it from the answer */
    def commits: Long = synchronized(saves)

  /**
   * A JOURNAL THAT ONLY THE LEADER MAY WRITE TO
   * (specs/dataflow.md, stage 10).
   *
   * The commit asks the lease first, and a coordinator that has lost
   * it throws `Deposed` instead of writing — so a predecessor that
   * wakes up mid-run stops at its next epoch rather than committing
   * over its successor's state. Nothing else about the run changes;
   * the exception leaves through `Cluster.stream` like any other.
   *
   * IT IS A CHECK, NOT A COMPARE-AND-SET, and the difference is one
   * commit wide: a leader deposed between the check and the write can
   * still land that write. Closing it needs a conditional write in
   * the STORE — "save this only if the term is still mine" — and the
   * seam already permits one, because `save` may throw. The wrapper
   * here is what can be built over a store that offers no such thing.
   */
  def fenced(term: Long, lease: Lease, under: Checkpoint): Checkpoint = new Checkpoint:
    def save(epoch: Int, bytes: Array[Byte]): Unit =
      if !lease.held(term) then throw Deposed(term, epoch)
      under.save(epoch, bytes)
    def latest: Option[(Int, Array[Byte])] = under.latest

  /**
   * THE RECORD A RESUME SHOULD BELIEVE, out of everything a journal
   * still holds (dataflow-durable).
   *
   * Later TERM wins; within a term, later EPOCH wins. A store that
   * keeps its history hands this its records and is immune to a
   * stale commit that got past the fence; a store that keeps only the
   * last write has nothing to choose from and is not.
   */
  def newest(records: Iterable[Folded]): Option[Folded] =
    if records.isEmpty then None
    else Some(records.maxBy(f => (f.term, f.epoch.toLong)))

  /** what a coordinator that has lost the seat is told, at the
   * moment it would have written */
  final case class Deposed(term: Long, epoch: Int)
    extends RuntimeException(
      s"this coordinator no longer holds the lease (term $term) and did not commit epoch $epoch")

/**
 * WHAT THE COORDINATOR HOLDS, AS A VALUE.
 *
 * The folded state is only half of it. The watermark is computed from
 * what each partition has been SEEN to produce, and the two counters
 * are part of the answer a `Run` reports — a resumed coordinator that
 * forgot them would answer a different `dropped` for the same stream,
 * which is exactly the kind of "nearly right" this repository counts
 * as wrong.
 *
 * `state` is bytes because its Schema is the sink's own (`Wire.state`)
 * and this record must be describable without naming it.
 */
final case class Folded(epoch: Int,
                        seen: Vector[Vector[Flows.Extent]],
                        drops: Long, merged: Long,
                        state: Array[Byte],
                        /**
                         * THE SESSION IDS THIS RUN IS USING, so a
                         * resumed coordinator inherits them rather
                         * than minting new ones.
                         *
                         * Two things fall out, and both matter. A
                         * worker that SURVIVED the coordinator still
                         * holds the session at the epoch it was last
                         * asked for, so the resumed run continues on
                         * it instead of replaying from the start —
                         * and a worker that did not is opened afresh
                         * under the same id, which is 6b's road
                         * exactly. Without this the old sessions
                         * would sit on every worker for ever, since
                         * the party that could close them is gone.
                         */
                        base: Long,
                        /**
                         * THE TERM THIS COMMIT WAS MADE UNDER, and
                         * why a record carries it (dataflow-durable).
                         *
                         * `Checkpoint.fenced` asks the lease before
                         * writing, which stops a ghost cheaply and at
                         * the right moment — and is a check before a
                         * write, so a leader deposed between the two
                         * can still land one stale commit. Closing
                         * THAT needs a compare-and-set no store here
                         * offers.
                         *
                         * It does not need one. A journal that keeps
                         * its history can defend itself on the READ
                         * side: a resume takes the record with the
                         * highest (term, epoch) rather than the last
                         * one written, and since terms only rise, a
                         * stale commit is shadowed for ever instead
                         * of being read back. `Checkpoint.newest`
                         * does that selection, and a store that keeps
                         * only the last write cannot — which is the
                         * honest difference between a log and a cell.
                         *
                         * Zero when nobody is fencing.
                         */
                        term: Long = 0L,
                        /**
                         * IS THE STREAM OVER? (dataflow-durable)
                         *
                         * Stage 8 said a coordinator that died
                         * between the last Close and the answer could
                         * resume, ask for one more epoch, be told
                         * everything was drained and re-answer the
                         * same value. It could not, and a test found
                         * it: the fresh sessions a resumed run opens
                         * replay the whole source, DISCARD the panes
                         * their catch-up closed, and hand over only
                         * what is still open at the end — so the
                         * tail panes were retired a second time out
                         * of one partition's half and overwritten
                         * with a partial value. 29 of 3 204 on the
                         * synthetic feed.
                         *
                         * A finished run says so here, and a resume
                         * that reads it answers from the state
                         * instead of asking anybody. Which is also
                         * the cheap thing to do.
                         */
                        done: Boolean = false)

object Folded:
  given Schema[Flows.Extent] = Resp.given_Schema_Extent
  given Schema[Folded] = Schema.derived
