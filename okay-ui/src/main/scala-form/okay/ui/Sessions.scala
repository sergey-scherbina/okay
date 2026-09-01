package okay.ui

import okay.*
import okay.given
import okay.codec.{Json, Schema}
import okay.persist.{Ack, Record, Topic}

import java.nio.charset.StandardCharsets.UTF_8

/**
 * Event-sourced UI sessions on the durable log (specs/ui.md, the low
 * level; okay-persist stage 0).
 *
 * The journal is the INBOUND LINE STREAM, verbatim — forged keys and
 * damaged JSON included — because `Wire.serve` is deterministic about
 * dropping them, and refolding the journal through the SAME pure
 * stage is the entire correctness argument. Filtering before
 * journaling would be a second implementation of the rule, and two
 * implementations of one rule is how they drift.
 *
 * One session = one key; `Topic.route` picks the partition, so
 * per-session order is the log's own per-key order. A snapshot lives
 * in the same partition under `key + "#snap"` with value
 * `{upTo, s}` — an OPTIMIZATION that bounds the refold, never the
 * truth (specs/persist.md inherits that doctrine and so does this).
 * Until persist-stage1's compaction, `latest` is a fold of the tail:
 * correct, not yet cheap.
 */
object Sessions {

  /** one session's address: its topic and its key */
  final case class Session(topic: Topic, key: String):
    private[Sessions] def keyBytes: Array[Byte] = key.getBytes(UTF_8)
    private[Sessions] def snapBytes: Array[Byte] = (key + "#snap").getBytes(UTF_8)
    /** the partition the key routes to — snapshots go to the SAME
     * one, so recovery is one read path */
    def partition: Int = Topic.route(keyBytes, topic.partitions)

  // ---------------------------------------------------------------- journal

  /** append one inbound line, intent-first: this happens BEFORE the
   * stage sees the line, so a crash between the two leaves the event
   * in history, not in limbo */
  def append(s: Session, line: String): Long =
    s.topic.append(s.partition, s.keyBytes, line.getBytes(UTF_8), Ack.Durable)

  /** every journaled line for this session from an offset, oldest
   * first — a stage-0 batch read loop (the streaming helper is
   * persist-stage1); other keys sharing the partition are filtered */
  def lines(s: Session, from: Long = 0L): Vector[(Long, String)] =
    records(s, from).collect {
      case r if java.util.Arrays.equals(r.key, s.keyBytes) =>
        (r.offset, String(r.value, UTF_8))
    }

  private def records(s: Session, from: Long): Vector[Record] =
    val stop = s.topic.end(s.partition)
    def go(at: Long, acc: Vector[Record]): Vector[Record] =
      if at >= stop then acc
      else s.topic.read(s.partition, at, 512) match
        case Topic.Read.Records(rs) if rs.nonEmpty => go(rs.last.offset + 1, acc ++ rs)
        case Topic.Read.Records(_) => acc
        // history compacted/retired from under us: begin where it begins
        case Topic.Read.TooEarly(begin) => go(begin, acc)
    go(from, Vector.empty)

  // ---------------------------------------------------------------- snapshots

  /** write a snapshot of S covering the journal up to `upTo` */
  def snapshot[S](s: Session, state: S, upTo: Long)(using sc: Schema[S]): Long =
    val value = Json.print(Json.JObj(Vector(
      "upTo" -> Json.JNum(upTo.toDouble),
      "s" -> Json.parse(Json.write(state)))))
    s.topic.append(s.partition, s.snapBytes, value.getBytes(UTF_8), Ack.Durable)

  /** the newest snapshot, if any — a fold of the tail until
   * persist-stage1's keep-latest compaction makes it a lookup */
  def latest[S](s: Session)(using sc: Schema[S]): Option[(S, Long)] =
    records(s, 0L).foldLeft(Option.empty[(S, Long)]) { (acc, r) =>
      if !java.util.Arrays.equals(r.key, s.snapBytes) then acc
      else
        val j = Json.parse(String(r.value, UTF_8))
        val upTo = Rpcish.num(j, "upTo")
        val decoded = Rpcish.field(j, "s").flatMap(v => Json.decode(sc)(v).toOption)
        (decoded, upTo) match
          case (Some(st), Some(n)) => Some((st, n))
          case _ => acc            // a damaged snapshot is data: skip it
    }

  // ---------------------------------------------------------------- recovery

  /**
   * The state a session reaches by replaying its journal through the
   * SAME pure stage a live run uses. From the newest snapshot when
   * one decodes, else from init; answers the state and the offset the
   * refold reached (where a continuation should journal from).
   */
  def recover[S](s: Session)(init: S)(view: S => Ui)(update: (S, Event) => S)
                (using Schema[S]): (S, Long) =
    val (start, from) = latest[S](s).fold((init, 0L))((st, n) => (st, n + 1))
    val tail = lines(s, from)
    // a journal spans CONNECTIONS: a journaled Closed ended one, not
    // the session's life — so the refold folds segment by segment,
    // each through the same pure stage, the next continuing from the
    // state the last one reached. (Found by the intent-first test: a
    // line appended after a Closed vanished from a naive refold.)
    def isClosed(line: String): Boolean =
      WireJson.eventOf(okay.codec.Json.parse(line)).contains(Event.Closed)
    def segments(rest: List[String]): List[List[String]] =
      if rest.isEmpty then Nil
      else
        val (seg, more) = rest.span(l => !isClosed(l))
        seg :: segments(more.drop(1))
    val replayed = segments(tail.map(_._2).toList).foldLeft(start) { (st, seg) =>
      if seg.isEmpty then st
      else !.run(Writer.run(through(Writer.of(seg))(Wire.serve(st)(view)(update))))._2
    }
    (replayed, tail.lastOption.fold(from)(_._1 + 1))

  // ---------------------------------------------------------------- serving

  /**
   * The whole story in one call: recover, then serve live with every
   * inbound line journaled BEFORE the stage sees it. The journaling
   * tee is an effectful stage between the transport and the pure
   * serve — `through` composes them, as everywhere.
   */
  def serve[S](sess: Session)(init: S)(view: S => Ui)(update: (S, Event) => S)
              (lines: Source[String], send: String => Unit ! Async)
              (using Schema[S]): S ! Async =
    type Row = Take % String + (Writer % String + Async)
    def tee: Unit ! Row =
      effect[Row, Option[String]](Take.Await()).flatMap {
        case None => pure(())
        case Some(line) =>
          effect[Row, Long](Async.Run(() => append(sess, line)))
            .flatMap(_ => effect[Row, Unit](Writer(line)))
            .flatMap(_ => tee)
      }

    val (recovered, _) = recover(sess)(init)(view)(update)
    val journaled: Source[String] = through[String, String, Async, Unit, Unit](lines)(tee)
    val served: S ! (Writer % String + Async) =
      through[String, String, Async, Unit, S](journaled)(
        !.widen[S, Take % String + Writer % String, Async](
          Wire.serve(recovered)(view)(update)))

    def drain(p: S ! (Writer % String + Async)): S ! Async =
      Writer.uncons[String, S, Async](p).flatMap {
        case Left(s) => pure(s)
        case Right((out, rest)) => send(out).flatMap(_ => drain(rest))
      }

    drain(served)

  /** tiny local json readers (the codec keeps objects as Vectors) */
  private object Rpcish:
    def field(j: Json, n: String): Option[Json] = j match
      case Json.JObj(fs) => fs.collectFirst { case (k, v) if k == n => v }
      case _ => None
    def num(j: Json, n: String): Option[Long] =
      field(j, n).collect { case Json.JNum(x) => x.toLong }
}
