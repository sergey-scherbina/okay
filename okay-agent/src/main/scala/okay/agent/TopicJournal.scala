package okay.agent

import okay.codec.Schema
import okay.persist.{Ack, Topic, Typed}

/**
 * `Durable.Journal` over a keyed topic (specs/persist.md, stage 1):
 * the doc-comment's promise — "a file or a table behind the same
 * three methods" — kept by the log itself.
 *
 * COMPLETE-AS-APPEND: intent and completion are separate records.
 * The in-place update `MemoryJournal.complete` performs cannot
 * survive an append-only disk, and should not — intent-first only
 * means anything if the intent physically precedes the answer. So
 * `append` writes an `Intent`, `complete` writes a `Complete`
 * referencing the intent's seq, and `all` folds the partition back
 * into entries: an intent with no completion is the crash window,
 * surfaced for the policy exactly as `MemoryJournal` surfaces it.
 *
 * One run = one key: the run's records land in one partition, in
 * order, and a topic serves many runs at once. Records travel
 * through the `Typed` envelope (version 1), so the journal format
 * can evolve by upcast like every journal-grade topic. A record of
 * this run that does not decode ends the fold there — the torn-tail
 * doctrine one level up: everything before the damage serves,
 * nothing after it is guessed at.
 */
final class TopicJournal(topic: Topic, run: String) extends Durable.Journal:
  import TopicJournal.*

  private val typed = Typed[Rec](topic, version = 1, upcasts = Map.empty)
  private val runKey = run.getBytes("UTF-8")
  private val partition = Topic.route(runKey, topic.partitions)

  def append(e: Durable.Entry): Unit =
    typed.append(partition, runKey, Rec.Intent(e.seq, e.op, e.fingerprint, e.key), Ack.Durable): Unit

  def complete(seq: Int, answer: String): Unit =
    typed.append(partition, runKey, Rec.Complete(seq, answer), Ack.Durable): Unit

  def all: Vector[Durable.Entry] =
    var entries = Vector.empty[Durable.Entry]
    var from = topic.begin(partition)
    var going = true
    while going do
      typed.read(partition, from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do
              d match
                case Typed.Decoded.Ok(off, _, key, rec) =>
                  if key.sameElements(runKey) then
                    rec match
                      case Rec.Intent(seq, op, fp, k) =>
                        entries :+= Durable.Entry(seq, op, fp, k, None)
                      case Rec.Complete(seq, answer) =>
                        entries = entries.map(e =>
                          if e.seq == seq then e.copy(answer = Some(answer)) else e)
                  from = off + 1
                case Typed.Decoded.Bad(off, _) =>
                  // ours or not is unknowable — the fold ends here
                  going = false
                  from = off
    entries

object TopicJournal:
  /** the journal's records; `derives` keeps the wire format in one
   * place, and the envelope version guards its evolution */
  enum Rec derives Schema:
    case Intent(seq: Int, op: String, fingerprint: String, key: String)
    case Complete(seq: Int, answer: String)
