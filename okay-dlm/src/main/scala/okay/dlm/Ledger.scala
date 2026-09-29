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
    /**
     * A PERSON'S OWN WORDS TAKEN OUT (dlm-erasure, §10). The content is
     * gone from the ledger; this is what stays in its place — the
     * subject (a digest by default, since the identifier is the
     * person's data too), how many entries went, why, and who did it.
     *
     * An `Erased` entry is never itself erased: it is the evidence that
     * a request was honoured, and a ledger that can lose that cannot
     * show it ever happened.
     */
    case Erased(subject: String, entries: Int, why: String, at: Long, by: String)

    def at: Long
    def by: String

  /** where entries go: a service's journal, or ours in memory */
  trait Sink:
    def append(e: Entry): Unit

  /**
   * A LEDGER THAT CAN TAKE A PERSON'S WORDS BACK OUT (§10).
   *
   * Append-only is the audit's claim, and a person's right to have their
   * data removed is the law's; they reconcile in one place and only one —
   * the CONTENT goes, the FACT stays (`Entry.Erased`). A sink that cannot
   * do it (a broadcast, somebody else's topic) simply is not one of
   * these, and `Governed.erase` says so rather than pretending.
   */
  trait Erasable extends Sink:
    /** every entry this ledger holds, oldest first */
    def entries: Vector[Entry]
    /** remove every entry that is this person's; answers how many went */
    def erase(who: String): Int

  /** OURS: a vector, for a suite and for a `Governed` that keeps its own history */
  final class Recorded extends Erasable:
    @volatile private var all = Vector.empty[Entry]
    def append(e: Entry): Unit = synchronized { all :+= e }
    def entries: Vector[Entry] = all
    def erase(who: String): Int = synchronized {
      val (stays, gone) = Ledger.erase(all, who)
      all = stays; gone
    }

  val silent: Sink = _ => ()

  /** A LEDGER ON DISK: one JSON object per line, appended — what a build
   * step writes beside the tables it builds, and what a boot reads back */
  final class File(path: java.nio.file.Path) extends Erasable:
    import java.nio.file.{Files, StandardCopyOption, StandardOpenOption}
    def append(e: Entry): Unit = synchronized {
      Files.createDirectories(path.toAbsolutePath.getParent)
      Files.writeString(path, line(e) + "\n", StandardOpenOption.CREATE, StandardOpenOption.APPEND): Unit
    }
    def entries: Vector[Entry] =
      if !Files.exists(path) then Vector.empty else parse(Files.readString(path))
    /**
     * REWRITTEN WITHOUT THAT PERSON, and by a move, not in place: a
     * process that dies halfway through an erasure must leave either
     * the old file or the new one, never half of either. The lines a
     * reader did not understand are NOT carried over — this reader
     * cannot tell whose they are, and a line whose subject is unknown
     * is exactly what an erasure may not leave behind.
     */
    def erase(who: String): Int = synchronized {
      if !Files.exists(path) then 0
      else
        val (stays, gone) = Ledger.erase(entries, who)
        if gone == 0 then 0
        else
          val tmp = path.resolveSibling(path.getFileName.toString + ".erasing")
          Files.writeString(tmp, stays.map(line).mkString("", "\n", if stays.isEmpty then "" else "\n"))
          Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING): Unit
          gone
    }

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

  /**
   * WHAT STAYS WHEN A PERSON IS ERASED, and how many entries went (§10).
   *
   * An entry is that person's when it NAMES them: their lessons, their
   * withdrawals, the refusals they were given — and a `Shared` entry
   * they themselves shared, whose `earlier` is their sentence. An entry
   * that only acts on tables (`Rebuilt`, `Pruned`) is nobody's words.
   *
   * `Erased` entries always stay, whoever they name: they are the record
   * that erasures happened, including this one.
   */
  def erase(entries: Iterable[Entry], who: String): (Vector[Entry], Int) =
    val all = entries.toVector
    val stays = all.filter {
      case Entry.Erased(_, _, _, _, _) => true
      case Entry.Learned(w, _, _, _, _, _) => w != who
      case Entry.Forgotten(w, _, _, _) => w != who
      case Entry.Refused(w, _, _, _, b) => w != who && b != who
      case Entry.Shared(_, _, _, _, b) => b != who
      case _ => true
    }
    (stays, all.length - stays.length)

  /**
   * A SUBJECT THAT IS NOT THE PERSON. The identifier is the person's
   * data as much as their sentence is, so the record of an erasure names
   * them by a digest of it — enough to count erasures and to answer «was
   * my request honoured», not enough to be the identifier again.
   *
   * SHA-256, the first sixteen bytes, as `Exemplars.hash` does it.
   */
  def digest(who: String): String =
    "sha256:" + java.security.MessageDigest.getInstance("SHA-256")
      .digest(who.getBytes("UTF-8")).take(16).map(b => f"${b & 0xff}%02x").mkString

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
    case Entry.Erased(subject, entries, why, at, by) => JObj(Vector(
      "entry" -> JStr("erased"), "subject" -> JStr(subject), "entries" -> JNum(entries.toDouble),
      "why" -> JStr(why), "at" -> JNum(at.toDouble), "by" -> JStr(by)))

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
        case "erased" => for sub <- s("subject"); e <- n("entries"); why <- s("why"); at <- n("at"); by <- s("by")
          yield Entry.Erased(sub, e.toInt, why, at, by)
        case _ => None
      }
    case _ => None
