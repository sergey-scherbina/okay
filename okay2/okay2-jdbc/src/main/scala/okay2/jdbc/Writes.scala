package okay2.jdbc

import scala.annotation.tailrec
import okay2.{!, Writer, pure}
import okay2.async.Async
import okay2.codec.Schema
import okay2.persist.{Ack, Topic, Typed}
import okay2.sql.{Sql, SqlValue}
import okay2.stream.{Chunk, Source}

/**
 * The write bridge (okay-jdbc's Writes.scala, specs/jdbc.md, "Writing
 * correctly into a database we do not own"): exactly-once OUTCOME into a
 * database whose structure is not ours to change. Nothing is created on
 * their side — their UNIQUE constraints are the idempotency machinery; on
 * ours, the intent is journaled in okay2-persist BEFORE the statement
 * runs, so a crash between journal and commit leaves a readable question
 * instead of a silent maybe. One run = one key in the topic.
 */
final class Writes(db: Sql, topic: Topic, run: String) {
  import Writes._

  private val typed = Typed[Rec](topic, version = 1, upcasts = Map.empty)
  private val runKey = run.getBytes("UTF-8")
  private val partition = Topic.route(runKey, topic.partitions)
  private var seq = fold().map(_._1.seq).maxOption.map(_ + 1).getOrElse(0)

  /** intent first, then the statement, then the completion — a crash
   * between the second and third steps is exactly what `recover` resolves */
  def write(sql: String, params: Vector[SqlValue], key: String): Long ! Async = {
    val n = synchronized { val n = seq; seq += 1; n }
    Async {
      typed.append(partition, runKey, Rec.Intent(n, sql, params, key), Ack.Durable)
    }.flatMap { _ =>
      db.update(sql, params).flatMap { count =>
        Async {
          typed.append(partition, runKey, Rec.Done(n, count), Ack.Durable): Unit
          count
        }
      }
    }
  }

  /** refold the run: every intent without a completion is the crash
   * window, resolved per key by the declared policy; answers are DATA per
   * entry — a batch recovery reports, the caller decides */
  def recover(policy: String => Policy): Vector[Recovered] ! Async = {
    val open = fold().collect { case (i, None) => i }
    // an intent settled inside flatMap continues from there, a call that
    // cannot be a jump; `again` takes it, so the walk stays a loop
    def again(rest: List[Rec.Intent], acc: Vector[Recovered]): Vector[Recovered] ! Async = resolve(rest, acc)
    @tailrec def resolve(rest: List[Rec.Intent], acc: Vector[Recovered]): Vector[Recovered] ! Async =
      rest match {
        case Nil => pure[Async, Vector[Recovered]](acc)
        case i :: tail => policy(i.key) match {
          case Policy.WithKey =>
            // the SAME statement, the SAME key: their constraint answers
            // "already happened" (MERGE / ON CONFLICT)
            db.update(i.sql, i.params).flatMap { n =>
              Async { typed.append(partition, runKey, Rec.Done(i.seq, n), Ack.Durable) }
                .flatMap(_ => again(tail, acc :+ Recovered.Reapplied(i.key, n)))
            }
          case Policy.Reconcile(select) =>
            countRows(db.query(select, Vector(SqlValue.Text(i.key)))).flatMap { found =>
              if (found > 0)
                Async { typed.append(partition, runKey, Rec.Done(i.seq, found), Ack.Durable) }
                  .flatMap(_ => again(tail, acc :+ Recovered.Settled(i.key, found)))
              else again(tail, acc :+ Recovered.Unresolved(i.key, "the far end has no row for this key"))
            }
          case Policy.Fail =>
            resolve(tail, acc :+ Recovered.Unresolved(i.key, "policy forbids repeating and asking"))
        }
      }
    resolve(open.toList, Vector.empty)
  }

  /** the run's entries, oldest first: intent plus its completion count
   * when one arrived — the journal is readable */
  def entries: Vector[(Rec.Intent, Option[Long])] = fold()

  private def fold(): Vector[(Rec.Intent, Option[Long])] = {
    var intents = Vector.empty[(Rec.Intent, Option[Long])]
    var from = topic.begin(partition)
    var going = true
    while (going) {
      typed.read(partition, from, 256) match {
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else
            for (d <- rs if going) d match {
              case Typed.Decoded.Ok(off, _, key, rec) =>
                if (key.sameElements(runKey)) rec match {
                  case i: Rec.Intent => intents :+= ((i, None))
                  case Rec.Done(s, n) =>
                    intents = intents.map(e => if (e._1.seq == s) (e._1, Some(n)) else e)
                }
                from = off + 1
              case Typed.Decoded.Bad(off, _) =>
                // the torn-tail doctrine one level up: nothing after the
                // damage is guessed at
                going = false
                from = off
            }
      }
    }
    intents
  }

  private def countRows(p: Source[Chunk[Vector[SqlValue]]]): Long ! Async =
    Writer.loopWith[Chunk[Vector[SqlValue]], Long, Unit, Long, Async](p)(0L)((n, c) => n + c.length)((n, _) => n)
}

object Writes {

  /** the journal's records; intent and completion are SEPARATE —
   * complete-as-append, the specs/persist.md contract */
  sealed trait Rec

  object Rec {
    final case class Intent(seq: Int, sql: String, params: Vector[SqlValue], key: String) extends Rec
    final case class Done(seq: Int, count: Long) extends Rec
  }

  // Num travels as its decimal text and Uuid as its text on the journal
  // (okay2.sql's instances); Arr and Row hold values, so the value
  // schema is lazy and the derivation reaches it by name
  import okay2.sql.{decimalSchema, uuidSchema}
  implicit lazy val sqlValueSchema: Schema[SqlValue] = Schema.derived
  implicit lazy val recSchema: Schema[Rec] = Schema.derived

  /** the decision per unsettled intent — Durable.OnRepeat's shape, bound
   * to what a relational far end offers */
  sealed trait Policy

  object Policy {
    /** re-run the same statement with the SAME key (MERGE / ON CONFLICT) */
    case object WithKey extends Policy
    /** do not re-run: `select` takes the key as its one parameter; any
     * row answers "it happened" and settles the journal */
    final case class Reconcile(select: String) extends Policy
    /** neither repeat nor ask: report and leave it to a human */
    case object Fail extends Policy
  }

  sealed trait Recovered

  object Recovered {
    final case class Reapplied(key: String, count: Long) extends Recovered
    final case class Settled(key: String, found: Long) extends Recovered
    final case class Unresolved(key: String, why: String) extends Recovered
  }
}
