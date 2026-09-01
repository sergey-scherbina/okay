package okay.matching

import okay.codec.Json
import okay.codec.Json.*
import okay.persist.{Ack, Topic}

/**
 * The chat log on a persist topic (specs/match.md, stage 1): the
 * ONLY source of truth. Turns are appended keyed by profile (per-key
 * order for free), offsets become fact provenance, and any store —
 * memory, SQL, whatever comes next — is a projection rebuilt by
 * `replay`. Extraction being idempotent by (profile, attribute,
 * provenance), replaying over a store that already saw some of the
 * log is a no-op for what it saw and a catch-up for what it missed.
 */
final case class ChatTurn(profile: ProfileId, role: String, text: String)

final class ChatLog(topic: Topic) {

  /** provenance's `chat` field for this log */
  def chat: String = topic.name

  /** append one turn; the offset that comes back IS the provenance */
  def append(turn: ChatTurn, ack: Ack = Ack.Durable): Long =
    topic.append(turn.profile.uuid.getBytes("UTF-8"),
      Json.print(JObj(Vector(
        "profile" -> JStr(turn.profile.uuid),
        "role" -> JStr(turn.role),
        "text" -> JStr(turn.text)))).getBytes("UTF-8"), ack)

  private def turnOf(bytes: Array[Byte]): Option[ChatTurn] =
    Json.parse(new String(bytes, "UTF-8")) match
      case JObj(fs) =>
        def f(k: String) = fs.collectFirst { case (`k`, JStr(v)) => v }
        for p <- f("profile"); r <- f("role"); t <- f("text")
        yield ChatTurn(ProfileId(p), r, t)
      case _ => None

  /**
   * Walk every partition from its beginning, feeding each surviving
   * turn WITH its provenance to `extract` — which writes facts
   * through the same operations the live chat uses. Damage and
   * compacted holes are persist's to report; a turn that does not
   * parse is skipped, not thrown (the log may hold generations).
   */
  def replay(extract: (ChatTurn, Provenance) => Unit, batch: Int = 256): Long =
    var seen = 0L
    for part <- 0 until topic.partitions do
      var from = topic.begin(part)
      var going = true
      while going do
        topic.read(part, from, batch) match
          case Topic.Read.TooEarly(begin) => from = begin
          case Topic.Read.Records(rs) if rs.isEmpty => going = false
          case Topic.Read.Records(rs) =>
            rs.foreach { r =>
              turnOf(r.value).foreach { t =>
                extract(t, Provenance(chat, r.offset, t.text))
                seen += 1
              }
            }
            from = rs.last.offset + 1
    seen
}
