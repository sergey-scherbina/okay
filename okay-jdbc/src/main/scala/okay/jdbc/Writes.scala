package okay.jdbc

import okay.{!, +, Async, Chunk, Produce, Stream, async}
import okay.given
import okay.codec.Schema
import okay.persist.{Ack, Topic, Typed}
import okay.sql.{Sql, SqlValue}

/**
 * The write bridge (specs/jdbc.md, "Writing correctly into a
 * database we do not own"): exactly-once OUTCOME into a database
 * whose structure is not ours to change. Nothing is created on
 * their side — their UNIQUE constraints are the idempotency
 * machinery; on ours, the intent is journaled in okay-persist
 * BEFORE the statement runs, so a crash between journal and commit
 * leaves a readable question instead of a silent maybe.
 *
 * Driver-agnostic: written against the `Sql` seam and a persist
 * `Topic` — it lives in okay-jdbc only until a second driver wants
 * it. One run = one key in the topic, the TopicJournal convention.
 */
final class Writes(db: Sql, topic: Topic, run: String):
  import Writes.*

  private val typed = Typed[Rec](topic, version = 1, upcasts = Map.empty)
  private val runKey = run.getBytes("UTF-8")
  private val partition = Topic.route(runKey, topic.partitions)
  private var seq = fold().map(_._1.seq).maxOption.map(_ + 1).getOrElse(0)

  /** intent first, then the statement, then the completion — the
   * order is the whole point: a crash between the second and third
   * steps is exactly what `recover` resolves */
  def write(sql: String, params: Vector[SqlValue], key: String): Long ! Async =
    val n = synchronized { val n = seq; seq += 1; n }
    async {
      typed.append(partition, runKey, Rec.Intent(n, sql, params, key), Ack.Durable)
    }.flatMap { _ =>
      db.update(sql, params).flatMap { count =>
        async {
          typed.append(partition, runKey, Rec.Done(n, count), Ack.Durable): Unit
          count
        }
      }
    }

  /** refold the run: every intent without a completion is the crash
   * window, resolved per key by the declared policy; answers are
   * DATA per entry — a batch recovery reports, the caller decides */
  def recover(policy: String => Policy): Vector[Recovered] ! Async =
    val open = fold().collect { case (i, None) => i }
    def resolve(rest: List[Rec.Intent], acc: Vector[Recovered]): Vector[Recovered] ! Async =
      rest match
        case Nil => okay.pure(acc)
        case i :: tail => policy(i.key) match
          case Policy.WithKey =>
            // the SAME statement, the SAME key: their constraint
            // answers "already happened" (MERGE / ON CONFLICT)
            db.update(i.sql, i.params).flatMap { n =>
              async { typed.append(partition, runKey, Rec.Done(i.seq, n), Ack.Durable) }
                .flatMap(_ => resolve(tail, acc :+ Recovered.Reapplied(i.key, n)))
            }
          case Policy.Reconcile(select) =>
            countRows(db.query(select, Vector(SqlValue.Text(i.key)))).flatMap { found =>
              if found > 0 then
                async { typed.append(partition, runKey, Rec.Done(i.seq, found), Ack.Durable) }
                  .flatMap(_ => resolve(tail, acc :+ Recovered.Settled(i.key, found)))
              else resolve(tail, acc :+ Recovered.Unresolved(i.key, "the far end has no row for this key"))
            }
          case Policy.Fail =>
            resolve(tail, acc :+ Recovered.Unresolved(i.key, "policy forbids repeating and asking"))
    resolve(open.toList, Vector.empty)

  /** the run's entries, oldest first: intent plus its completion
   * count when one arrived — the journal is readable, which is the
   * point of having one */
  def entries: Vector[(Rec.Intent, Option[Long])] = fold()

  private def fold(): Vector[(Rec.Intent, Option[Long])] =
    var intents = Vector.empty[(Rec.Intent, Option[Long])]
    var from = topic.begin(partition)
    var going = true
    while going do
      typed.read(partition, from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do d match
              case Typed.Decoded.Ok(off, _, key, rec) =>
                if key.sameElements(runKey) then rec match
                  case i: Rec.Intent => intents :+= (i, None)
                  case Rec.Done(s, n) =>
                    intents = intents.map(e => if e._1.seq == s then (e._1, Some(n)) else e)
                from = off + 1
              case Typed.Decoded.Bad(off, _) =>
                // the torn-tail doctrine one level up: nothing after
                // the damage is guessed at
                going = false
                from = off
    intents

  private def countRows(p: Chunk[Vector[SqlValue]] ! (Produce + Async)): Long ! Async =
    val S = summon[Stream[[X] =>> X ! (Produce + Async), Async]]
    S.uncons(p).flatMap {
      case None => okay.pure(0L)
      case Some((c, rest)) => countRows(rest).map(_ + c.length)
    }

object Writes:

  // Num travels as its decimal text on the journal (okay.sql's given)
  import okay.sql.decimalSchema
  given Schema[SqlValue] = Schema.derived

  /** the journal's records; intent and completion are SEPARATE —
   * complete-as-append, the specs/persist.md contract */
  enum Rec derives Schema:
    case Intent(seq: Int, sql: String, params: Vector[SqlValue], key: String)
    case Done(seq: Int, count: Long)

  /** the decision per unsettled intent — Durable.OnRepeat's shape,
   * bound to what a relational far end offers (specs/jdbc.md) */
  enum Policy:
    /** re-run the same statement with the SAME key; the statement's
     * contract is the idempotent form (MERGE / ON CONFLICT), which
     * is WithKey spelled in SQL */
    case WithKey
    /** do not re-run: `select` takes the key as its one parameter;
     * any row answers "it happened" and settles the journal */
    case Reconcile(select: String)
    /** neither repeat nor ask: report and leave it to a human */
    case Fail

  enum Recovered:
    case Reapplied(key: String, count: Long)
    case Settled(key: String, found: Long)
    case Unresolved(key: String, why: String)
