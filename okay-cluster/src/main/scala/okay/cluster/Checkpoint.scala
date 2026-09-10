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
                        base: Long)

object Folded:
  given Schema[Flows.Extent] = Resp.given_Schema_Extent
  given Schema[Folded] = Schema.derived
