package okay.cluster

import okay.given
import okay.codec.Schema
import okay.docs.{Cond, Docs, PutResult, TopicDocs}
import okay.persist.{MemoryStore, Policy}

/**
 * A JOURNAL WHOSE COMMIT IS A COMPARE-AND-SET
 * (specs/dataflow.md, stage 10's last box).
 *
 * The box said the seam permits a conditional write and no store here
 * offers one. `okay-docs` does: `put(id, a, Cond.IfVersion(v))`
 * applies only if the document is still at `v`, and answers `Stale`
 * with what it holds now if it is not. That is the whole mechanism.
 *
 * WHY A DOCUMENT AND NOT A LOG. A log cannot do this by itself —
 * read-the-tail-then-append is two operations with the same gap the
 * fence has. A cell can, because "still at this version" is asked and
 * answered in one write. So the engine takes whichever defence its
 * store can offer: a log shadows a stale commit on the READ side
 * (`Checkpoint.newest`, highest (term, epoch) wins), a cell refuses
 * it on the write side, and `Checkpoint.fenced` picks by asking
 * whether the journal is `Fencing`.
 *
 * A `Stale` answer is read as "deposed" rather than retried, and
 * that is deliberate: this journal has ONE writer per term, so a
 * concurrent write means somebody else believes they are the
 * coordinator. Guessing again would be guessing about exactly the
 * thing the term exists to settle.
 */
final class DocsJournal(id: String = "coordinator") extends Fencing:
  import DocsJournal.*

  private val docs: Docs[Record] =
    TopicDocs[Record](MemoryStore().topic("journal", 1, Policy(compact = true)))

  private def now: Option[Docs.Versioned[Record]] = docs.get(id).runWith

  def save(epoch: Int, bytes: Array[Byte]): Unit =
    val _ = saveIfTerm(epoch, bytes, Long.MaxValue)

  def saveIfTerm(epoch: Int, bytes: Array[Byte], term: Long): Boolean =
    val current = now
    // A HIGHER TERM HAS WRITTEN: deposed, and no write is attempted.
    // (`Long.MaxValue` is `save`'s unconditional road, which no term
    // can be above.)
    if current.exists(_.value.term > term) then false
    else
      val cond = current.fold[Cond](Cond.IfAbsent)(v => Cond.IfVersion(v.version))
      docs.put(id, Record(if term == Long.MaxValue then 0L else term, epoch, bytes), cond).runWith match
        case PutResult.Applied(_) => true
        // somebody wrote between the read and the write — which is
        // the very gap this exists to close, and the answer is no
        case PutResult.Stale(_) => false

  def latest: Option[(Int, Array[Byte])] = now.map(v => (v.value.epoch, v.value.bytes))

  /** what the store holds, for a test to look at */
  def term: Option[Long] = now.map(_.value.term)

object DocsJournal:
  final case class Record(term: Long, epoch: Int, bytes: Array[Byte]) derives Schema
