package okay.ui

import okay.*
import okay.codec.{Json, JsonOptic}
import okay.codec.JsonOptic.given

/**
 * A JSON document edited in place — the product that fired the
 * zipper's trigger (specs/zipper.md). The state is a `Zipper[Json]`
 * plus the two drafts the inputs hold; the view is the document as
 * an outline with the focus marked, a row of moves, a row of edits
 * and done/cancel; the step is a pure function of the two, which is
 * what lets a test drive it through `Nav.update` with no host.
 *
 * BUTTONS ONLY, no letter keys: no host delivers `Event.Key` today
 * (`Frame.interpretChar` drops a letter that no `Input` is focused
 * on), so a `j`/`k` binding would be code no host can reach. On the
 * terminal Tab walks the buttons and Enter presses.
 *
 * INLINE editing, not a dialog: `Nav.To` after a `Push`ed prompt
 * replaces the prompt's frame only, leaving the previous editor
 * frame under the new one; an `Input` in the view keeps one frame.
 *
 * Structural edits go through the PARENT — `JsonOptic.removeChild`
 * and `insertChild` under a `modify` at `up` — because a plate is
 * arity-preserving and an object's key cannot be invented by it.
 */
object JsonEditor:

  /** the cursor, the value draft (the focused scalar's text), the key draft */
  final case class Ed(z: Zipper[Json], value: String, key: String)

  /** an editor over `json`; `done(Some(edited))` on done, `done(None)` on cancel */
  def apply(json: Json)(done: Option[Json] => Nav): Screen =
    Nav.screen(Ed(Zipper(json), draftOf(json), ""))(view)((s, e) => step(s, e, done))

  def view(s: Ed): Ui =
    import Ui.*
    val doc = Column(outline(s.z.root, Nil, s.z.path, 0), key = "$doc")
    val moves = Row(Vector(Button("into", "$into"), Button("out", "$out"),
                           Button("prev", "$prev"), Button("next", "$next")))
    val edits = Row(Vector(Input(s.value, "$value", "value"), Button("set", "$set"),
                           Input(s.key, "$key", "key"), Button("add", "$add"),
                           Button("delete", "$delete", Role.Danger)))
    val end = Row(Vector(Button("done", "$done", Role.Primary), Button("cancel", "$cancel")))
    Column(Vector(doc, moves, edits, end), key = "$editor")

  /** one line per node, indented by depth; the focused line is marked
   * and emphasised, and it is the only one that is */
  private def outline(j: Json, path: List[Int], focus: List[Int], depth: Int, label: String = ""): Vector[Ui] =
    val here = path == focus
    val text = "  " * depth + (if here then "> " else "  ") + label + summary(j)
    val line = Ui.Text(text, if here then Style(bold = true, tone = Tone.Emphasis) else Style.none)
    val kids = j match
      case Json.JArr(vs) => vs.zipWithIndex.flatMap((v, i) => outline(v, path :+ i, focus, depth + 1, s"$i: "))
      case Json.JObj(fs) => fs.zipWithIndex.flatMap { case ((k, v), i) => outline(v, path :+ i, focus, depth + 1, s"$k: ") }
      case _ => Vector.empty
    line +: kids

  private def summary(j: Json): String = j match
    case Json.JArr(vs) => s"[${vs.length}]"
    case Json.JObj(fs) => s"{${fs.length}}"
    case leaf => Json.print(leaf)

  private def step(s: Ed, e: Event, done: Option[Json] => Nav): Nav | Ed =
    import Event.*
    // a move that does not exist leaves everything as it was
    def moved(z: Option[Zipper[Json]]): Ed = z.fold(s)(z2 => s.copy(z = z2, value = draftOf(z2.focus)))
    def container(j: Json): Boolean = j match
      case _: Json.JArr | _: Json.JObj => true
      case _ => false
    e match
      case Pressed("$into") => moved(s.z.first)
      case Pressed("$out") => moved(s.z.up)
      case Pressed("$prev") => moved(s.z.left)
      case Pressed("$next") => moved(s.z.right)
      case Edited("$value", v) => s.copy(value = v)
      case Edited("$key", k) => s.copy(key = k)
      case Pressed("$set") => s.copy(z = s.z.set(parse(s.value)))
      case Pressed("$delete") => s.z.index match
        case Some(i) => moved(s.z.up.map(_.modify(JsonOptic.removeChild(_, i))))
        case None => s                                  // the root has no parent to delete it from
      case Pressed("$add") => s.z.index match
        // after the focus, among its siblings
        case Some(i) => moved(s.z.up.flatMap(_.modify(JsonOptic.insertChild(_, i + 1, s.key, Json.JNull)).down(i + 1)))
        // at the root: inside it, last — when it is a container at all
        case None if container(s.z.focus) =>
          val n = JsonOptic.plate.children(s.z.focus).length
          moved(s.z.modify(JsonOptic.insertChild(_, n, s.key, Json.JNull)).down(n))
        case None => s
      case Pressed("$done") => done(Some(s.z.root))
      case Pressed("$cancel") | Closed => done(None)
      case _ => s

  /** what the value input shows for a focus: a scalar's text, nothing for a container */
  private def draftOf(j: Json): String = j match
    case Json.JArr(_) | Json.JObj(_) => ""
    case leaf => Json.print(leaf)

  /** the draft as Json: JSON syntax if it parses, the text itself otherwise */
  private def parse(s: String): Json = Json.parse(s) match
    case Json.JErr(_) => Json.JStr(s)
    case j => j
