package okay.agent

import okay.Handler
import okay.codec.Json

/**
 * The third mode of a journal, beside `Durable.tools` (record, and
 * recover from a crash) and `Durable.replaying` (answer everything
 * from the journal, touch nothing): run the journal again against
 * TODAY'S world, with the tools actually executing, and compare.
 *
 * That is the mode that turns a journal into a test. `replaying`
 * proves the program is deterministic given the same answers;
 * rerunning proves the WORLD still gives those answers. A journal
 * recorded in June and rerun in September either still matches, or
 * names the step where reality moved.
 *
 * Two treatments of a divergence, and the difference is only whether
 * the run stops:
 *
 *   - `Loud` throws at the first divergence, naming the step, the
 *     call, what was recorded and what the world said now. This is
 *     the default and what CI should use: a journal that no longer
 *     reproduces is a finding, not a nuisance.
 *   - `Quiet` accepts the new answer, keeps going LIVE from there,
 *     and branches a new [[Version]]: the entries before the
 *     divergence shared with the parent, a fresh live tail after it.
 *
 * `Quiet` never means silent. The divergence is recorded on the new
 * version and reported in the [[Outcome]] — snapshot testing already
 * taught the industry that an auto-accept nobody reads is worse than
 * no test. Quiet moves the run forward; it does not hide anything.
 *
 * **Why a divergence branches instead of patching.** Once step k
 * answers differently, every entry after k is unusable: the model
 * that consumed answer X asked its next question because of X, and
 * the run that saw Y will ask something else. So a new version is
 * never a patch over the old one. It is the shared prefix up to k
 * plus whatever the live run does after it, which is why versions
 * form a TREE that branches at divergence points, and why the
 * interesting diff is always "diverged at k: recorded X, got Y".
 *
 * **What a version must carry to be worth comparing.** [[Provenance]]
 * (the code revision, the model, the tool set) — otherwise a diff
 * between two versions says behaviour changed and connects it to
 * nothing. The journal sees calls, not the world underneath them, so
 * the provenance header is the only place the world gets pinned at
 * all, and it is pinned by whatever the caller can honestly state.
 *
 * **The model half needs nothing new.** A rerun fixes the model with
 * `Handlers.scripted` over the recorded replies and lets the TOOLS
 * run live, which is the combination that tests your code against
 * fixed model behaviour. Of the four record/replay combinations only
 * two earn their keep: this one, and `Durable.replaying` (both
 * sides from the journal) for reproducing an incident. A live model
 * over journalled tools is nondeterministic anyway, and both live is
 * just recording.
 */
object Rerun {

  import Durable.{Entry, Journal}

  /** What produced a version: enough to make a diff between two of
   * them mean something. Every field is the caller's own claim —
   * this layer cannot verify a revision, only carry it. */
  final case class Provenance(revision: String = "",
                              model: String = "",
                              tools: String = "",
                              note: String = "")

  /** Where a rerun stopped agreeing with its journal, and how. */
  final case class Divergence(seq: Int, call: String, kind: Divergence.Kind,
                              recorded: String, got: String):
    def describe: String =
      s"step $seq ($call): ${kind.words} — recorded ${quote(recorded)}, got ${quote(got)}"
    private def quote(s: String) = if s.length <= 80 then s"'$s'" else s"'${s.take(77)}...'"

  object Divergence:
    /** Which side moved. Both branch the same way; they are told
     * apart because they mean different things to a reader: the
     * world changed under the same question, or the program is
     * asking a different question than it did. */
    enum Kind(val words: String):
      case Answer extends Kind("the world answered differently")
      case Call extends Kind("the program asked something else")

  /** One version of a journal: its entries, what produced them, and
   * which version it branched from at which step. A root version has
   * no parent and no branch point. */
  final case class Version(id: String,
                           entries: Vector[Entry],
                           provenance: Provenance,
                           parent: Option[String] = None,
                           branchedAt: Option[Int] = None,
                           divergence: Option[Divergence] = None):
    /** the journal interface, so a version replays like any journal */
    def journal: Journal = Version.journalOf(entries)

  object Version:
    /** A content-derived id, so the same entries under the same
     * parent are the same version no matter who built it. Not a
     * security boundary; it only has to tell versions apart. */
    def idOf(entries: Vector[Entry], parent: Option[String], p: Provenance): String =
      val text = parent.getOrElse("") + "" + p.toString + "" +
        entries.map(e => s"${e.seq}|${e.fingerprint}|${e.answer.getOrElse("")}").mkString("")
      f"v${math.abs(text.hashCode)}%08x"

    def root(entries: Vector[Entry], p: Provenance = Provenance()): Version =
      Version(idOf(entries, None, p), entries, p)

    /** a read-only Journal over fixed entries — a version IS a
     * journal to everything that only reads one */
    private def journalOf(es: Vector[Entry]): Journal = new Journal:
      def append(e: Entry): Unit = ()
      def complete(seq: Int, answer: String): Unit = ()
      def all: Vector[Entry] = es

  /** Where versions live. Memory here; a directory or a table behind
   * the same three methods. */
  trait Versions:
    def put(v: Version): Unit
    def get(id: String): Option[Version]
    def all: Vector[Version]

    /** the chain from `id` back to its root, newest first — the
     * "how did this journal get here" a reader actually wants */
    def lineage(id: String): Vector[Version] =
      def walk(at: Option[String], acc: Vector[Version]): Vector[Version] = at match
        case None => acc
        case Some(i) => get(i) match
          case None => acc
          case Some(v) => walk(v.parent, acc :+ v)
      walk(Some(id), Vector.empty)

  final class MemoryVersions extends Versions:
    private var byId = Map.empty[String, Version]
    private var order = Vector.empty[String]
    def put(v: Version): Unit =
      if !byId.contains(v.id) then order = order :+ v.id
      byId = byId.updated(v.id, v)
    def get(id: String): Option[Version] = byId.get(id)
    def all: Vector[Version] = order.flatMap(byId.get)

  /** how a divergence is treated: stop, or branch and carry on */
  enum OnDiverge:
    case Loud, Quiet

  /** a rerun under `Loud` met a divergence */
  final class Diverged(val at: Divergence)
    extends RuntimeException(s"the journal no longer reproduces: ${at.describe}")

  /** What a rerun leaves behind: the version that describes THIS
   * run, and the divergence if there was one. `version eq base` (by
   * id) exactly when nothing diverged. */
  final case class Outcome(version: Version, divergence: Option[Divergence]):
    def reproduced: Boolean = divergence.isEmpty

  /**
   * The rerun handler, and the handle to read its outcome.
   *
   * Before a divergence, every call is checked against the journal
   * and then EXECUTED anyway: the point is to learn whether the world
   * still answers the same, which cannot be learned without asking
   * it. After a divergence under `Quiet`, the journal is left behind
   * entirely and the rest of the run is live — because from there on
   * it is a different run, and pretending otherwise is the one thing
   * this design refuses to do.
   */
  def live(base: Version, inner: Handler[Tool],
           mode: OnDiverge = OnDiverge.Loud,
           provenance: Provenance = Provenance(),
           versions: Versions = MemoryVersions())
  : (Run, Handler[Tool]) =
    val run = new Run(base, mode, provenance, versions)
    (run, run.handler(inner))

  /** the live state of one rerun */
  final class Run private[agent] (base: Version, mode: OnDiverge,
                                  provenance: Provenance, versions: Versions):
    private var seq = 0
    private val recorded = base.entries
    private var kept = Vector.empty[Entry]
    private var found: Option[Divergence] = None

    /** the divergence so far, if any */
    def divergence: Option[Divergence] = found

    /**
     * The version this run produced, stored in `versions`.
     *
     * No divergence: the base, unchanged and unduplicated — a rerun
     * that reproduced has nothing new to say, and a store full of
     * identical versions would say it badly.
     */
    def outcome: Outcome = found match
      case None => Outcome(base, None)
      case Some(d) =>
        val v = Version(
          id = Version.idOf(kept, Some(base.id), provenance),
          entries = kept,
          provenance = provenance,
          parent = Some(base.id),
          branchedAt = Some(d.seq),
          divergence = Some(d))
        versions.put(v)
        Outcome(v, Some(d))

    private[agent] def handler(inner: Handler[Tool]): Handler[Tool] = new Handler[Tool]:
      def handle[A](e: Tool[A]): A = e match
        case Tool.Call(c) =>
          val n = seq
          seq += 1
          val fp = fingerprintOf(c)

          // Past a divergence the journal is behind us: this is a
          // different run and it runs live, recording as it goes.
          if found.isDefined then return record(n, c, fp, inner.handle(Tool.Call(c)))

          recorded.find(_.seq == n) match
            // beyond the journal: the program does more than it did.
            // Not a divergence of an ANSWER — there is nothing to
            // compare — so it is the same call/answer decision as any
            // other step past the end: live, and recorded.
            case None =>
              val answer = inner.handle(Tool.Call(c))
              diverge(Divergence(n, c.name, Divergence.Kind.Call, "(no entry)", answer))
              record(n, c, fp, answer)

            case Some(entry) if entry.fingerprint != fp =>
              // the program asks something else than it did; run the
              // new call, then branch on it
              val answer = inner.handle(Tool.Call(c))
              diverge(Divergence(n, c.name, Divergence.Kind.Call, entry.fingerprint, fp))
              record(n, c, fp, answer)

            case Some(entry) =>
              val answer = inner.handle(Tool.Call(c))
              val was = entry.answer.getOrElse("(no answer recorded)")
              if answer != was then
                diverge(Divergence(n, c.name, Divergence.Kind.Answer, was, answer))
              record(n, c, fp, answer)

    /** Loud stops here; Quiet notes it and lets the run continue. */
    private def diverge(d: Divergence): Unit =
      mode match
        case OnDiverge.Loud => throw Diverged(d)
        case OnDiverge.Quiet => if found.isEmpty then found = Some(d)

    private def record(n: Int, c: ToolCall, fp: String, answer: String): String =
      kept = kept :+ Entry(n, c.name, fp, Durable.keyFor(n, c), Some(answer))
      answer

  /** the same fingerprint `Durable` journals by, so a version written
   * by one is read by the other */
  private def fingerprintOf(c: ToolCall): String =
    s"${c.name}(${Json.print(c.args)})"
}
