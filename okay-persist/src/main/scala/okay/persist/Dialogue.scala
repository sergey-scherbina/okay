package okay.persist

import okay.{!, +, Delim, pure}
import okay.codec.Schema

/**
 * A PAUSED PROGRAM WHOSE JOURNAL IS A TOPIC (durable-dialogue,
 * 2026-09-17): the glue between `Delim.replay` and the durable log.
 *
 * `Delim.resumable` stops a program in the middle and hands the rest
 * of it back as a value; that value is a closure and does not survive
 * a restart, so what is kept is the JOURNAL — the answers given so
 * far — and where the program stands is re-derived by running it
 * again and feeding those answers back without asking. Here the
 * journal is a partition of a topic, so "so far" survives the
 * process.
 *
 * THIS IS EVENT SOURCING, with one difference worth the whole class:
 * the events are the ANSWERS (the discipline is that everything the
 * outside world tells the program enters through `pause`, so the
 * answers are the only non-determinism there is), the aggregate's
 * state is where the program stands — and the fold that rebuilds it
 * is THE PROGRAM ITSELF. There is no `apply(state, event)` to write,
 * keep in step with the code, and get wrong.
 *
 * ORDER, as everywhere in this stack: the answer is appended DURABLY
 * BEFORE the program is advanced with it. A crash in that window
 * replays to the same place; the other order loses an answer the
 * outside world already acted on.
 *
 * One dialogue = one key = one partition, the convention `Saga`
 * follows. Damage is data: the fold stops at a record that does not
 * decode and names its offset, rather than feeding half a journal to
 * a program that would then be somewhere nobody chose.
 *
 * COST: `at` re-derives by replaying the whole journal, so a step is
 * O(answers so far). Dialogues are short by nature — a wizard, an
 * approval, a saga of calls — and the alternative is a cache that a
 * restart invalidates anyway. A snapshot road exists if one is ever
 * long enough to need it (`Snapshots`).
 */
final class Dialogue[Q, A, R, F[+_]](topic: Topic, val id: String)
                                    (body: Delim.Asking[Q, A, R, Delim + F] ?=> R ! (Delim + F))
                                    (using Schema[A]):

  private val typed = Typed[A](topic, version = 1, upcasts = Map.empty)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  /** what the log remembers, oldest first — and where it stopped
   * reading, if a record did not decode */
  def recovered: Dialogue.Recovered[A] =
    val out = Vector.newBuilder[A]
    var from = topic.begin(partition)
    var damage: Option[Dialogue.Damage] = None
    var going = true
    while going do
      typed.read(partition, from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do
              d match
                case Typed.Decoded.Ok(off, _, k, a) =>
                  if k.sameElements(key) then out += a
                  from = off + 1
                case Typed.Decoded.Bad(off, err) =>
                  damage = Some(Dialogue.Damage(off, err))
                  going = false
    Dialogue.Recovered(out.result().toList, damage)

  /** the answers given so far; a damaged log stops the journal where
   * the damage is, and `recovered` is how you see that it did */
  def journal: Delim.Journal[A] = recovered.answers

  /** where the program stands: itself, folded over its journal */
  def at: Delim.Dialogue[Q, A, R, F] ! F =
    Delim.replay[Q, A, R, F](body)(journal)

  /** answer the question it is asking. The answer is durable before
   * the program moves, so this is the only step that can be crossed
   * by a crash, and crossing it loses nothing. */
  def answer(a: A): Delim.Dialogue[Q, A, R, F] ! F =
    val _ = typed.append(partition, key, a, Ack.Durable)
    at

  /**
   * Run to the end, journaling every answer: `oracle` is what
   * actually talks to the outside world, and it is called once per
   * question — never for a question the journal already answered.
   */
  def run(oracle: Q => A ! F): R ! F =
    def step(p: Delim.Dialogue[Q, A, R, F]): R ! F = p match
      case Delim.Paused.Done(r) => pure(r)
      case Delim.Paused.Ask(q, _) => oracle(q).flatMap(a => answer(a).flatMap(step))
    at.flatMap(step)

object Dialogue:

  /** where the fold stopped, and why */
  final case class Damage(offset: Long, error: String)

  /** the journal, and the damage that ended it if any */
  final case class Recovered[A](answers: List[A], damage: Option[Damage]):
    def intact: Boolean = damage.isEmpty
