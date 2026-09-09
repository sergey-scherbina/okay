package okay.ui

import okay.codec.Json

/**
 * The wire shapes of the view, hand-mapped like every dialect in
 * this repository (the MCP and provider mappings set the precedent):
 * one place knows the field names, and nothing else does. Derivation
 * waits on the codec growing Vector, recursion and defaults
 * (backlog: codec-vector); the wire does not.
 *
 * Total in the reading direction: a damaged or unknown value is
 * None, never a throw — a wire that crashes on one bad line loses
 * every good one after it.
 */
object WireJson {

  import Json.*

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)
  private def field(j: Json, n: String): Option[Json] = j match
    case JObj(fs) => fs.collectFirst { case (k, v) if k == n => v }
    case _ => None
  private def str(j: Json, n: String): Option[String] =
    field(j, n).collect { case JStr(s) => s }
  private def num(j: Json, n: String): Option[Int] =
    field(j, n).collect { case JNum(x) => x.toInt }
  private def bool(j: Json, n: String): Option[Boolean] =
    field(j, n).collect { case JBool(b) => b }

  // ---------------------------------------------------------------- Ui

  def uiJson(u: Ui): Json = u match
    case Ui.Text(s, st) =>
      val base = Vector("t" -> JStr("text"), "s" -> JStr(s))
      JObj(base ++ (if st.bold then Vector("bold" -> JBool(true)) else Vector.empty)
        ++ (if st.dim then Vector("dim" -> JBool(true)) else Vector.empty)
        ++ (if st.tone != Tone.Plain then Vector("tone" -> JStr(st.tone.toString.toLowerCase)) else Vector.empty)
        ++ (if st.size != Size.Normal then Vector("size" -> JStr(st.size.toString.toLowerCase)) else Vector.empty))
    case Ui.Row(cs, k) => tagged("row", k, "c" -> JArr(cs.map(uiJson)))
    case Ui.Column(cs, k) => tagged("col", k, "c" -> JArr(cs.map(uiJson)))
    case Ui.Box(cs, dir, w, gap, pad, k) => tagged("box", k,
      "dir" -> JStr(if dir == Dir.Horizontal then "h" else "v"),
      "w" -> JArr(w.map(n => JNum(n.toDouble))), "gap" -> JNum(gap.toDouble),
      "pad" -> JNum(pad.toDouble), "c" -> JArr(cs.map(uiJson)))
    case Ui.Scroll(c, k) => tagged("scroll", k, "c" -> JArr(Vector(uiJson(c))))
    case Ui.Image(src, alt) => tagged("image", "", "src" -> JStr(src), "alt" -> JStr(alt))
    case Ui.Button(l, k, role) => tagged("button", k, "label" -> JStr(l),
      "role" -> JStr(role.toString.toLowerCase))
    case Ui.Input(v, k, l, kind, live) => tagged("input", k, "value" -> JStr(v), "label" -> JStr(l),
      "kind" -> JStr(kind.toString.toLowerCase), "live" -> JBool(live))
    case Ui.Form(fs, submit, k) => tagged("form", k, "submit" -> JStr(submit), "c" -> JArr(fs.map(uiJson)))
    case Ui.Items(is, k) => tagged("items", k, "c" -> JArr(is.map(uiJson)))
    case Ui.Table(h, rows, k) => tagged("table", k, "header" -> JArr(h.map(JStr(_))),
      "rows" -> JArr(rows.map(r => JArr(r.map(uiJson)))))
    case Ui.Tabs(ls, i, pages, k) => tagged("tabs", k, "labels" -> JArr(ls.map(JStr(_))),
      "i" -> JNum(i.toDouble), "c" -> JArr(pages.map(uiJson)))
    case Ui.Modal(t, body, k) => tagged("modal", k, "title" -> JStr(t), "c" -> JArr(Vector(uiJson(body))))
    case Ui.Check(on, k, l) => tagged("check", k, "on" -> JBool(on), "label" -> JStr(l))
    case Ui.Select(os, i, k) => tagged("select", k,
      "options" -> JArr(os.map(JStr(_))), "i" -> JNum(i.toDouble))

  private def tagged(t: String, key: String, fs: (String, Json)*): Json =
    JObj((("t" -> JStr(t)) +: (if key.isEmpty then Vector.empty[(String, Json)]
      else Vector("k" -> JStr(key)))) ++ fs.toVector)

  def uiOf(j: Json): Option[Ui] =
    def kids: Option[Vector[Ui]] = field(j, "c") match
      case Some(JArr(vs)) =>
        val ks = vs.map(uiOf)
        if ks.forall(_.isDefined) then Some(ks.flatten) else None
      case _ => None
    val key = str(j, "k").getOrElse("")
    str(j, "t").flatMap {
      case "text" => str(j, "s").map(s => Ui.Text(s,
        Style(bold = bool(j, "bold").getOrElse(false),
          dim = bool(j, "dim").getOrElse(false),
          tone = str(j, "tone").flatMap(t => Tone.values.find(_.toString.toLowerCase == t)).getOrElse(Tone.Plain),
          size = str(j, "size").flatMap(t => Size.values.find(_.toString.toLowerCase == t)).getOrElse(Size.Normal))))
      case "row" => kids.map(Ui.Row(_, key))
      case "col" => kids.map(Ui.Column(_, key))
      case "box" => kids.map { cs =>
        val w = field(j, "w") match
          case Some(JArr(vs)) => vs.collect { case JNum(n) => n.toInt }
          case _ => Vector.empty
        Ui.Box(cs, if str(j, "dir").contains("h") then Dir.Horizontal else Dir.Vertical,
          w, num(j, "gap").getOrElse(0), num(j, "pad").getOrElse(0), key) }
      case "scroll" => kids.flatMap(_.headOption).map(Ui.Scroll(_, key))
      case "image" => for src <- str(j, "src"); alt <- str(j, "alt") yield Ui.Image(src, alt)
      case "button" => str(j, "label").map(Ui.Button(_, key,
        str(j, "role").flatMap(r => Role.values.find(_.toString.toLowerCase == r)).getOrElse(Role.Plain)))
      case "input" => str(j, "value").map(Ui.Input(_, key, str(j, "label").getOrElse(""),
        str(j, "kind").flatMap(r => InputKind.values.find(_.toString.toLowerCase == r)).getOrElse(InputKind.Text),
        bool(j, "live").getOrElse(false)))
      case "form" => for fs <- kids; submit <- str(j, "submit") yield Ui.Form(fs, submit, key)
      case "items" => kids.map(Ui.Items(_, key))
      case "table" => (field(j, "header"), field(j, "rows")) match
        case (Some(JArr(hs)), Some(JArr(rs))) =>
          val rows = rs.map {
            case JArr(cells) => val cs = cells.map(uiOf); if cs.forall(_.isDefined) then Some(cs.flatten) else None
            case _ => None }
          if rows.forall(_.isDefined) then Some(Ui.Table(hs.collect { case JStr(s) => s }, rows.flatten, key))
          else None
        case _ => None
      case "tabs" => for pages <- kids; i <- num(j, "i") yield
        Ui.Tabs(field(j, "labels") match
          case Some(JArr(ls)) => ls.collect { case JStr(s) => s }
          case _ => Vector.empty, i, pages, key)
      case "modal" => for body <- kids.flatMap(_.headOption); t <- str(j, "title") yield Ui.Modal(t, body, key)
      case "check" => bool(j, "on").map(Ui.Check(_, key, str(j, "label").getOrElse("")))
      case "select" => field(j, "options") match
        case Some(JArr(os)) =>
          Some(Ui.Select(os.collect { case JStr(s) => s }, num(j, "i").getOrElse(0), key))
        case _ => None
      case _ => None
    }

  // ---------------------------------------------------------------- Event

  def eventJson(e: Event): Json = e match
    case Event.Pressed(k) => obj("e" -> JStr("press"), "k" -> JStr(k))
    case Event.Edited(k, v) => obj("e" -> JStr("edit"), "k" -> JStr(k), "v" -> JStr(v))
    case Event.Toggled(k, on) => obj("e" -> JStr("toggle"), "k" -> JStr(k), "on" -> JBool(on))
    case Event.Chosen(k, i) => obj("e" -> JStr("choose"), "k" -> JStr(k), "i" -> JNum(i.toDouble))
    case Event.Key(c) => obj("e" -> JStr("key"), "c" -> JStr(c.toString))
    case Event.Resized(w, h) => obj("e" -> JStr("resize"), "w" -> JNum(w.toDouble), "h" -> JNum(h.toDouble))
    case Event.Closed => obj("e" -> JStr("closed"))

  def eventOf(j: Json): Option[Event] = str(j, "e").flatMap {
    case "press" => str(j, "k").map(Event.Pressed(_))
    case "edit" => for k <- str(j, "k"); v <- str(j, "v") yield Event.Edited(k, v)
    case "toggle" => for k <- str(j, "k"); on <- bool(j, "on") yield Event.Toggled(k, on)
    case "choose" => for k <- str(j, "k"); i <- num(j, "i") yield Event.Chosen(k, i)
    case "key" => str(j, "c").flatMap(_.headOption).map(Event.Key(_))
    case "resize" => for w <- num(j, "w"); h <- num(j, "h") yield Event.Resized(w, h)
    case "closed" => Some(Event.Closed)
    case _ => None
  }

  // ---------------------------------------------------------------- Patch

  private def path(p: List[Int]): Json = JArr(p.map(i => JNum(i.toDouble)).toVector)
  private def pathOf(j: Json): Option[List[Int]] = field(j, "at") match
    case Some(JArr(vs)) => Some(vs.collect { case JNum(n) => n.toInt }.toList)
    case _ => None

  def patchJson(p: Patch): Json = p match
    case Patch.Replace(at, ui) => obj("p" -> JStr("replace"), "at" -> path(at), "ui" -> uiJson(ui))
    case Patch.SetText(at, s) => obj("p" -> JStr("text"), "at" -> path(at), "s" -> JStr(s))
    case Patch.SetValue(at, s) => obj("p" -> JStr("value"), "at" -> path(at), "s" -> JStr(s))
    case Patch.SetChecked(at, on) => obj("p" -> JStr("checked"), "at" -> path(at), "on" -> JBool(on))
    case Patch.SetSelected(at, i) => obj("p" -> JStr("selected"), "at" -> path(at), "i" -> JNum(i.toDouble))
    // the keyed-diff trio — added by the exhaustivity warning this
    // sweep surfaced: the previous task extended Patch and missed the
    // wire, which would have MatchErrored a server-driven reorder
    case Patch.Remove(at, i) => obj("p" -> JStr("remove"), "at" -> path(at), "i" -> JNum(i.toDouble))
    case Patch.Reorder(at, order) => obj("p" -> JStr("reorder"), "at" -> path(at),
      "order" -> JArr(order.map(n => JNum(n.toDouble))))
    case Patch.Insert(at, i, ui) => obj("p" -> JStr("insert"), "at" -> path(at),
      "i" -> JNum(i.toDouble), "ui" -> uiJson(ui))

  def patchOf(j: Json): Option[Patch] = str(j, "p").flatMap {
    case "replace" => for at <- pathOf(j); u <- field(j, "ui").flatMap(uiOf) yield Patch.Replace(at, u)
    case "text" => for at <- pathOf(j); s <- str(j, "s") yield Patch.SetText(at, s)
    case "value" => for at <- pathOf(j); s <- str(j, "s") yield Patch.SetValue(at, s)
    case "checked" => for at <- pathOf(j); on <- bool(j, "on") yield Patch.SetChecked(at, on)
    case "selected" => for at <- pathOf(j); i <- num(j, "i") yield Patch.SetSelected(at, i)
    case "remove" => for at <- pathOf(j); i <- num(j, "i") yield Patch.Remove(at, i)
    case "reorder" => for
        at <- pathOf(j)
        order <- field(j, "order") match
          case Some(JArr(vs)) => Some(vs.collect { case JNum(n) => n.toInt })
          case _ => None
      yield Patch.Reorder(at, order)
    case "insert" => for at <- pathOf(j); i <- num(j, "i"); u <- field(j, "ui").flatMap(uiOf)
      yield Patch.Insert(at, i, u)
    case _ => None
  }
}
