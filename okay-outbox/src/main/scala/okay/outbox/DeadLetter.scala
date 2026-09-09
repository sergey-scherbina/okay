package okay.outbox

import okay.*
import okay.codec.Schema
import okay.persist.{Ack, Offsets, Record, Store, Topic, Typed}

/**
 * Dead-lettering (specs/outbox.md): a record the handler cannot
 * process is retried a bounded number of times, then parked in a
 * dead-letter topic WITH its error, and the offset is committed past
 * it — a poison record never blocks its partition. `Dead` has a
 * Schema, so the dlq is readable by everything that reads the log,
 * and a replay is a read plus an append.
 */
object DeadLetter:

  final case class Dead(topic: String, part: Int, offset: Long,
                        key: Array[Byte], value: Array[Byte],
                        attempts: Int, error: String, at: Long) derives Schema

  /**
   * One pass from the group's committed offset (or the partition's
   * begin): every record handled in order; a failure retried up to
   * `attempts` times in all; then parked; the offset committed after
   * EVERY record, so a crash re-meets at most one. Answers the
   * records passed.
   */
  def consume(topic: Topic, part: Int, group: String, offsets: Offsets,
              dlq: Typed[Dead], attempts: Int = 3, max: Int = 256,
              clock: () => Long = () => System.currentTimeMillis)
             (handle: Record => Unit ! Async)(using Scheduler): Int ! Async =
    require(attempts >= 1, "a record is tried at least once")
    val from = offsets.committed(group, topic.name, part).getOrElse(topic.begin(part))
    val records = topic.read(part, from, max) match
      case Topic.Read.Records(rs) => rs
      case Topic.Read.TooEarly(begin) =>
        topic.read(part, begin, max) match
          case Topic.Read.Records(rs) => rs
          case Topic.Read.TooEarly(_) => Vector.empty

    def attempt(r: Record, n: Int): Unit ! Async =
      Async.attempt(handle(r)).flatMap {
        case Right(()) => pure(())
        case Left(_) if n < attempts => attempt(r, n + 1)
        case Left(e) =>
          okay.async {
            dlq.append(r.key, Dead(topic.name, part, r.offset, r.key, r.value, n,
              Option(e.getMessage).getOrElse(e.getClass.getName), clock()), Ack.Durable): Unit
          }
      }

    def go(rest: List[Record], n: Int): Int ! Async = rest match
      case Nil => pure(n)
      case r :: more =>
        attempt(r, 1)
          .flatMap(_ => okay.async(offsets.commit(group, topic.name, part, r.offset + 1)))
          .flatMap(_ => go(more, n + 1))

    go(records.toList, 0)

  /** dead records from `from` back onto their topics, key and value
    * as they were; answers the next dlq offset to replay from */
  def replay(dlq: Typed[Dead], store: Store, from: Long, max: Int = 256): Long ! Async =
    okay.async {
      dlq.read(0, from, max) match
        case Typed.Read.Records(rs) =>
          var next = from
          for d <- rs do
            d match
              case Typed.Decoded.Ok(offset, _, _, dead) =>
                store.topic(dead.topic).append(dead.part, dead.key, dead.value, Ack.Durable): Unit
                next = offset + 1
              case Typed.Decoded.Bad(offset, _) =>
                next = offset + 1   // a dlq record this Schema cannot read stays where it is
          next
        case Typed.Read.TooEarly(begin) => begin
    }
