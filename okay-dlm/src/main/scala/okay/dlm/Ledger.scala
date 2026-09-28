package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/**
 * EVERY CHANGE TO WHAT THE MODEL KNOWS, as it was decided — append-only,
 * replayable, and the audit is a read of it (specs/dlm-learning.md).
 *
 * There is no second system: the model at any moment is the fold of
 * these entries, `Ledger.replay` IS that fold, and a service that
 * already journals every turn writes these into the journal it has.
 * A REFUSAL IS AN ENTRY TOO — an audit that only shows what was
 * accepted cannot see an attack.
 */
object Ledger:

  enum Entry:
    /** a lesson taken: whose words, to which class, by whom */
    case Learned(who: String, earlier: String, intent: String, offset: Long, at: Long, by: String)
    /** a lesson withdrawn */
    case Forgotten(who: String, earlier: String, at: Long, by: String)
    /** a pair that became everyone's: enough holders, or a teacher */
    case Shared(earlier: String, intent: String, holders: Int, at: Long, by: String)
    /** a table rebuilt: the hash before and after, and the corpus it came from */
    case Rebuilt(artifact: String, encoder: String, before: Option[String], after: String, corpus: String, at: Long, by: String)
    /** a door that said no, and why */
    case Refused(who: String, what: String, why: String, at: Long, by: String)
    /** a table let go from the shelf, and the policy that let it go */
    case Pruned(artifact: String, hash: String, policy: String, at: Long, by: String)

    def at: Long
    def by: String

  /** where entries go: a service's journal, or ours in memory */
  trait Sink:
    def append(e: Entry): Unit

  /** OURS: a vector, for a suite and for a `Governed` that keeps its own history */
  final class Recorded extends Sink:
    @volatile private var all = Vector.empty[Entry]
    def append(e: Entry): Unit = synchronized { all :+= e }
    def entries: Vector[Entry] = all

  val silent: Sink = _ => ()

  /** A LEDGER ON DISK: one JSON object per line, appended — what a build
   * step writes beside the tables it builds, and what a boot reads back */
  final class File(path: java.nio.file.Path) extends Sink:
    import java.nio.file.{Files, StandardOpenOption}
    def append(e: Entry): Unit = synchronized {
      Files.createDirectories(path.toAbsolutePath.getParent)
      Files.writeString(path, line(e) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND): Unit
    }
    def entries: Vector[Entry] =
      if !Files.exists(path) then Vector.empty else parse(Files.readString(path))

  /** one entry as one line */
  def line(e: Entry): String = Json.print(encode(e))

  /** lines back to entries; a line this reader does not know is skipped,
   * because a newer writer may know more kinds than it */
  def parse(text: String): Vector[Entry] =
    text.linesIterator.map(_.trim).filter(_.nonEmpty)
      .flatMap(l => scala.util.Try(Json.parse(l)).toOption.flatMap(decode)).toVector

  /** several sinks as one */
  def tee(sinks: Sink*): Sink = e => sinks.foreach(_.append(e))

  /**
   * THE FOLD IS THE REPLAY. `Learned` and `Forgotten` are the memory's
   * own events; the rest are audit and change nothing the router
   * reads. A ledger folded with learning off is the empty memory —
   * which is exactly the claim that the model does not drift while
   * nobody is teaching it.
   */
  def replay(entries: Iterable[Entry], rules: Memory.Rules = Memory.defaults,
             teaching: Teaching = Teaching.ours): Memory =
    if !teaching.enabled(Teaching.Channel.Lesson) then Memory.empty
    else Memory.of(entries.flatMap {
      case Entry.Learned(who, earlier, intent, offset, at, _) =>
        Some(Memory.Event.Taught(who, earlier, intent, offset, at))
      case Entry.Forgotten(who, earlier, _, _) if teaching.enabled(Teaching.Channel.Withdrawal) =>
        Some(Memory.Event.Withdrawn(who, earlier))
      case _ => None
    }, rules.copy(teacher = w => rules.teacher(w) || teaching.teacher(w)))

  // ---- the wire: a service journals these as JSON ------------------------

  def encode(e: Entry): Json = e match
    case Entry.Learned(who, earlier, intent, offset, at, by) => JObj(Vector(
      "entry" -> JStr("learned"), "who" -> JStr(who), "earlier" -> JStr(earlier), "intent" -> JStr(intent),
      "offset" -> JNum(offset.toDouble), "at" -> JNum(at.toDouble), "by" -> JStr(by)))
    case Entry.Forgotten(who, earlier, at, by) => JObj(Vector(
      "entry" -> JStr("forgotten"), "who" -> JStr(who), "earlier" -> JStr(earlier),
      "at" -> JNum(at.toDouble), "by" -> JStr(by)))
    case Entry.Shared(earlier, intent, holders, at, by) => JObj(Vector(
      "entry" -> JStr("shared"), "earlier" -> JStr(earlier), "intent" -> JStr(intent),
      "holders" -> JNum(holders.toDouble), "at" -> JNum(at.toDouble), "by" -> JStr(by)))
    case Entry.Rebuilt(artifact, encoder, before, after, corpus, at, by) => JObj(Vector(
      "entry" -> JStr("rebuilt"), "artifact" -> JStr(artifact), "encoder" -> JStr(encoder)) ++
      before.map(b => "before" -> JStr(b)) ++ Vector(
      "after" -> JStr(after), "corpus" -> JStr(corpus), "at" -> JNum(at.toDouble), "by" -> JStr(by)))
    case Entry.Refused(who, what, why, at, by) => JObj(Vector(
      "entry" -> JStr("refused"), "who" -> JStr(who), "what" -> JStr(what), "why" -> JStr(why),
      "at" -> JNum(at.toDouble), "by" -> JStr(by)))
    case Entry.Pruned(artifact, hash, policy, at, by) => JObj(Vector(
      "entry" -> JStr("pruned"), "artifact" -> JStr(artifact), "hash" -> JStr(hash), "policy" -> JStr(policy),
      "at" -> JNum(at.toDouble), "by" -> JStr(by)))

  def decode(j: Json): Option[Entry] = j match
    case JObj(fs) =>
      def s(k: String) = fs.collectFirst { case (`k`, JStr(v)) => v }
      def n(k: String) = fs.collectFirst { case (`k`, JNum(v)) => v.toLong }
      s("entry").flatMap {
        case "learned" => for who <- s("who"); e <- s("earlier"); i <- s("intent"); o <- n("offset"); at <- n("at"); by <- s("by")
          yield Entry.Learned(who, e, i, o, at, by)
        case "forgotten" => for who <- s("who"); e <- s("earlier"); at <- n("at"); by <- s("by") yield Entry.Forgotten(who, e, at, by)
        case "shared" => for e <- s("earlier"); i <- s("intent"); h <- n("holders"); at <- n("at"); by <- s("by")
          yield Entry.Shared(e, i, h.toInt, at, by)
        case "rebuilt" => for a <- s("artifact"); enc <- s("encoder"); after <- s("after"); c <- s("corpus"); at <- n("at"); by <- s("by")
          yield Entry.Rebuilt(a, enc, s("before"), after, c, at, by)
        case "refused" => for who <- s("who"); w <- s("what"); why <- s("why"); at <- n("at"); by <- s("by")
          yield Entry.Refused(who, w, why, at, by)
        case "pruned" => for a <- s("artifact"); h <- s("hash"); p <- s("policy"); at <- n("at"); by <- s("by")
          yield Entry.Pruned(a, h, p, at, by)
        case _ => None
      }
    case _ => None
