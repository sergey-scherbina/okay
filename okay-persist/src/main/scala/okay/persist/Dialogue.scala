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
 * COST, in two paths, because they are different problems
 * (dialogue-snapshots, 2026-09-17):
 *
 *   WARM — you are holding the program: `step(p, a)` journals the
 *   answer and advances it by one, no replay. A drive that answers n
 *   questions costs O(n). Measured: `run` over 40 answers reads ZERO
 *   records, a loop of the replaying `answer` reads 820 = 40·41/2.
 *
 *   COLD — you have only the log: `answer(a)` replays, so it is
 *   O(answers so far). Give the dialogue a `Snapshots` and an
 *   interval and it writes CHAPTERS — a journal prefix and the offset
 *   it ends at — so a start reads one chapter plus the tail.
 *
 * What no snapshot can remove: the program is run once over the
 * answers to find out where it stands. That is exactly what "the fold
 * is the program" costs, and it is cheap in the shape this is for —
 * straight-line code between pauses.
 */
final class Dialogue[Q, A, R, F[+_]](topic: Topic, val id: String,
                                    snapshots: Option[Snapshots] = None,
                                    snapshotEvery: Int = 0)
                                    (body: Delim.Asking[Q, A, R, Delim + F] ?=> R ! (Delim + F))
                                    (using Schema[A]):

  private val typed = Typed[A](topic, version = 1, upcasts = Map.empty)
  private val key = id.getBytes("UTF-8")
  private val partition = Topic.route(key, topic.partitions)

  /** what the log remembers, oldest first — and where it stopped
   * reading, if a record did not decode */
  def recovered: Dialogue.Recovered[A] =
    val out = Vector.newBuilder[A]
    // the newest chapter, if one was written: its answers are the
    // prefix and the log is read from just after where it ended. A
    // chapter that does not decode is damage like any other, and the
    // honest answer is to ignore it and read the log from the start —
    // the log is the truth, a snapshot is only a shortcut.
    val chapter: Option[Dialogue.Chapter[A]] = snapshots.flatMap: snaps =>
      snaps.latestValue[Dialogue.Chapter[A]](key).flatMap(_._2.toOption)
    chapter.foreach(c => out ++= c.answers)
    var from = chapter.map(_.upTo + 1).getOrElse(topic.begin(partition))
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

  /**
   * THE COLD PATH. Answer the question it is asking, knowing only the
   * log: journal the answer, then re-derive. The answer is durable
   * before the program moves, so this is the only step a crash can
   * cross, and crossing it loses nothing.
   *
   * O(answers so far), because it replays. Use it to take one step
   * from a standing start — a request that arrives at a process which
   * was not holding this dialogue. If you ARE holding it, `step` is
   * the same move without the replay.
   */
  def answer(a: A): Delim.Dialogue[Q, A, R, F] ! F =
    journalled(a)
    at

  /**
   * THE WARM PATH. Advance a dialogue you are already holding: the
   * answer is journalled and then the program in your hand takes ONE
   * step. No replay, so a drive that answers n questions costs O(n)
   * rather than O(n²) — which is the whole reason this exists beside
   * `answer`.
   *
   * The durable journal is the journal, so the in-memory one this
   * hands to `Delim.answer` is empty and its answer is dropped.
   */
  def step(p: Delim.Dialogue[Q, A, R, F], a: A): Delim.Dialogue[Q, A, R, F] ! F =
    journalled(a)
    Delim.answer(p, List.empty[A])(a).map(_._1)

  /** write the journal so far as a chapter, so a cold start reads one
   * record and a tail instead of everything. Explicit, because a
   * snapshot is an optimisation a consumer opts into — the same
   * doctrine `Snapshots` states. */
  def snapshot(): Unit = snapshots.foreach: snaps =>
    val r = recovered
    if r.intact then
      val _ = snaps.putValue(key, Dialogue.Chapter(topic.end(partition) - 1, r.answers))

  private def journalled(a: A): Unit =
    val _ = typed.append(partition, key, a, Ack.Durable)
    written += 1
    if snapshotEvery > 0 && written % snapshotEvery == 0 then snapshot()

  private var written = 0

  /**
   * Run to the end, journaling every answer: `oracle` is what
   * actually talks to the outside world, and it is called once per
   * question — never for a question the journal already answered.
   */
  def run(oracle: Q => A ! F): R ! F =
    def go(p: Delim.Dialogue[Q, A, R, F]): R ! F = p match
      case Delim.Paused.Done(r) => pure(r)
      // the WARM path: the program is in hand, so no step replays
      case Delim.Paused.Ask(q, _) => oracle(q).flatMap(a => step(p, a).flatMap(go))
    at.flatMap(go)

object Dialogue:

  /** where the fold stopped, and why */
  final case class Damage(offset: Long, error: String)

  /**
   * A JOURNAL PREFIX, WRITTEN DOWN. A continuation cannot be
   * snapshotted — it is a closure — so what a chapter holds is the
   * answers up to `upTo` and nothing derived from them. A cold start
   * then reads one record plus whatever arrived after it.
   *
   * What it does NOT buy: the program is still run once, over the
   * whole answer list, to find out where it stands. That is exactly
   * what "the fold is the program" costs, and it is cheap in the
   * shape this is for — straight-line code between pauses.
   */
  final case class Chapter[A](upTo: Long, answers: List[A])

  object Chapter:
    given [A](using Schema[A]): Schema[Chapter[A]] = Schema.derived

  /** the journal, and the damage that ended it if any */
  final case class Recovered[A](answers: List[A], damage: Option[Damage]):
    def intact: Boolean = damage.isEmpty
