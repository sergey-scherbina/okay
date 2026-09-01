package okay.obs

import okay.{!, Async}
import okay.codec.{Cbor, Json}
import okay.http.{Body, Http, Request}
import okay.persist.Topic

/**
 * The push glue: a CONSUMER of the trace topic, exactly like any
 * other consumer of any other topic — the offset is the resume
 * token, pacing belongs to the caller (a loop, a cron, a wakeup),
 * and shipping traces stops being a subsystem.
 */
object OtlpPush {

  /**
   * Read one batch from `from`, POST what arrived, answer the next
   * offset to resume from. Nothing new = no request. A collector
   * refusal is a Left naming the status — the batch is NOT consumed
   * (the offset does not advance), so retry re-ships it: at-least-
   * once, which is what trace ingestion expects.
   */
  def push(http: Http, topic: Topic, endpoint: String, service: String,
           from: Long, partition: Int = 0, max: Int = 512,
           headers: Seq[(String, String)] = Nil): Either[String, Long] ! Async =
    val (spans, next) = topic.read(partition, from, max) match
      case Topic.Read.TooEarly(begin) =>
        // truncated history: resume from what remains, ship that
        topic.read(partition, begin, max) match
          case Topic.Read.Records(rs) => (decode(rs), rs.lastOption.map(_.offset + 1).getOrElse(begin))
          case Topic.Read.TooEarly(b) => (Vector.empty, b)
      case Topic.Read.Records(rs) =>
        (decode(rs), rs.lastOption.map(_.offset + 1).getOrElse(from))
    if spans.isEmpty then okay.pure(Right(next))
    else
      http.send(Request.post(s"$endpoint/v1/traces",
        Body.Text(Json.print(Otlp.body(service, spans))),
        Seq("content-type" -> "application/json") ++ headers)).flatMap { r =>
        r.release.map { _ =>
          if r.ok then Right(next)
          else Left(s"the collector answered HTTP ${r.status}; the batch stays unconsumed at $from")
        }
      }

  /** damaged records are skipped — the trace topic is operational
   * data, and one bad frame must not stop the shipping of the rest */
  private def decode(rs: Vector[okay.persist.Record]): Vector[Span] =
    rs.flatMap(r => Cbor.read[Span](r.value).toOption)
}
