package okay.ui


/**
 * The React-shaped rendering, PURE: a Ui tree becomes the element
 * tree a `createElement` host expects — type, props, children — as a
 * VALUE, so the mapping is asserted on the JVM and the js glue is the
 * five lines it should be. Works for anything with React's shape
 * (Preact included), which is the point of targeting the shape rather
 * than the library.
 */
final case class Elem(tag: String,
                      props: Vector[(String, String)],
                      children: Vector[Elem] = Vector.empty,
                      text: Option[String] = None)

object React {

  import Ui.*

  /**
   * THE SEMANTIC NODES THIS HOST DRAWS ITSELF — the same set `live.js`
   * puts in its hello, and the reason it is a named value rather than
   * a literal at the one call site below.
   *
   * A host's catch-all asks `Ui.lower` for the meaning of a node it
   * does not claim, and `Ui.lower` RECURSES: whatever vocabulary is
   * handed to it reaches the children too. Passing `Set.empty` there
   * therefore does not say "lower this node", it says "lower this
   * whole subtree as if I drew nothing" — and a `Link` in a `Table`
   * cell came out as `Text("label — href")` one node away from where
   * the same link was an anchor (react-host-vocab, 2026-09-18, found
   * from okay-watch's analyst page).
   *
   * It also made the two roads disagree on ONE tree: `Wire.serve`
   * lowers per the client's hello, so the socket sent the anchor while
   * `Html.render` sent text. A page served both ways is one page only
   * if this set and `live.js`'s hello are the same set.
   *
   * `Frame` and `Swing` claim no semantic node, so `Set.empty` IS
   * their vocabulary and is correct there; the defect was never that
   * the set was empty, it was that a constant stood where a host's own
   * vocabulary belongs.
   *
   * WHY `table` IS IN THIS SET AND THE OTHER THREE ARE NOT
   * (ui-browser-vocab, specs/ui-product.md): a browser is the one host
   * whose medium HAS these elements, but claiming a node costs the
   * patch consumers whatever its element structure inserts between a
   * patch PATH and the child that path names.
   *
   * `Table` inserts nothing, because no path ever descends into one:
   * `Ui.diff` has no `Table` case, so a changed table is a `Replace`
   * at its own path and a consumer only ever BUILDS a table or swaps
   * one whole. The price is that same sentence read the other way — a
   * changed cell now replaces the table where the lowering gave a
   * narrow `SetText` — and it is a price, stated in the spec with the
   * trigger for revisiting it.
   *
   * `Items` would insert an `<li>`: every patch consumer would have to
   * unwrap it on the way down and wrap on Insert/Reorder/Remove, in
   * Scala and again in hand-written JavaScript, to gain `<ul>` over
   * `<div>`. `Tabs` would oblige every claiming client to switch tabs
   * itself. `Disclosure`'s `<details>` toggles natively and tells
   * nobody, so on the scriptless road the server's `open` and the
   * browser's would disagree from the first click, where the lowered
   * button is a POST that keeps them in step. `Modal`'s `<dialog>`
   * needs a script to open at all.
   */
  val Vocabulary: Set[String] = Set(Vocab.link, Vocab.table)

  /** the tree, in createElement's terms; keys ride as data-key, which
   * is also how the glue knows which Event a DOM event means */
  def elem(ui: Ui): Elem = ui match
    case Text(s, style) =>
      // ONE TABLE (Classes), because the browser's own renderer spells
      // the same six tokens and two spellings of one table is a page
      // that looks right one way and wrong the other
      val cls = Classes.of(style)
      Elem("span", if cls.isEmpty then Vector.empty else Vector("className" -> cls.mkString(" ")),
        text = Some(s))
    case Row(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-row")), children.map(elem))
    case Column(children, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-col")), children.map(elem))
    case Box(children, dir, weights, gap, pad, key) =>
      // flexbox forgets the layout problem: weights are flex-grow on
      // the children, gap and pad are the box's own style
      val cls = if dir == Dir.Horizontal then "okay-box okay-h" else "okay-box okay-v"
      val style = (if gap > 0 then Vector(s"gap:${gap}ch") else Vector.empty) ++
        (if pad > 0 then Vector(s"padding:${pad}ch") else Vector.empty)
      // the weights ALSO ride on the box as data-w, so a patch consumer
      // replacing or inserting one child can give it its flex without
      // holding the tree (TestDom found a Replace losing it)
      val weighted = weights.length == children.length
      val props = Vector("className" -> cls) ++
        (if style.nonEmpty then Vector("style" -> style.mkString(";")) else Vector.empty) ++
        (if weighted then Vector("data-w" -> weights.mkString(" ")) else Vector.empty)
      val kids = children.zipWithIndex.map { (c, i) =>
        val e = elem(c)
        if weighted then styled(e, s"flex:${weights(i)}") else e
      }
      Elem("div", keyed(key, props), kids)
    case Scroll(child, key) =>
      Elem("div", keyed(key, Vector("className" -> "okay-scroll", "style" -> "overflow:auto")), Vector(elem(child)))
    case Image(src, alt) => Elem("img", Vector("src" -> src, "alt" -> alt))
    case Button(label, key, role) =>
      val props = if role == Role.Plain then Vector.empty
        else Vector("className" -> ("okay-" + role.toString.toLowerCase))
      Elem("button", keyed(key, props), text = Some(label))
    case Input(value, key, label, kind, live) =>
      val lv = if live then Vector("data-live" -> "1") else Vector.empty
      val input = kind match
        case InputKind.Text => Elem("input", keyed(key, lv :+ ("value" -> value)))
        case InputKind.Secret => Elem("input", keyed(key, lv ++ Vector("type" -> "password", "value" -> value)))
        case InputKind.Number => Elem("input", keyed(key, lv ++ Vector("type" -> "number", "value" -> value)))
        case InputKind.Multiline => Elem("textarea", keyed(key, lv :+ ("value" -> value)))
      if label.isEmpty then input
      else Elem("label", Vector.empty, Vector(Elem("span", Vector.empty, text = Some(label)), input))
    case Check(on, key, label) =>
      val box = Elem("input", keyed(key, Vector("type" -> "checkbox", "checked" -> on.toString)))
      if label.isEmpty then box
      else Elem("label", Vector.empty, Vector(box, Elem("span", Vector.empty, text = Some(label))))
    case Select(options, selected, key) =>
      Elem("select", keyed(key, Vector("value" -> options.lift(selected).getOrElse(""))),
        options.map(o => Elem("option", Vector("value" -> o), text = Some(o))))
    // a Form is a marked column: data-form names it, so a patch
    // consumer (live.js) knows which inputs fold locally and which
    // button submits them
    case Form(fields, submit, key) =>
      Elem("div", Vector("data-form" -> key, "className" -> "okay-form"),
        fields.map(elem) :+ elem(Button(submit, key, Role.Primary)))
    // a Link is a semantic node this host claims: an anchor is what a
    // browser has and nothing else does (ui-link)
    case Link(label, href) => Elem("a", Vector("href" -> href), text = Some(label))
    // and a TABLE is the other one (ui-browser-vocab): rows of boxes
    // read as a table and are not one — nothing assistive can see the
    // header, and a column's width had to be an inline `flex` on every
    // cell. `<col>` says the width ONCE, in the element whose job it
    // is, and `weights` is what it says: a SHARE, turned into a
    // percentage here (integer division; browsers normalise a column
    // set that does not total 100, and no pixel crosses the wire).
    // Empty weights write no colgroup, which is the even split the
    // lowering always gave.
    case Table(header, rows, key, weights) =>
      val total = weights.sum
      val cols =
        if weights.length != header.length || header.isEmpty || total <= 0 then Vector.empty
        else Vector(Elem("colgroup", Vector.empty,
          weights.map(w => Elem("col", Vector("style" -> s"width:${w * 100 / total}%")))))
      val head =
        if header.isEmpty then Vector.empty
        else Vector(Elem("thead", Vector.empty, Vector(Elem("tr", Vector.empty,
          header.map(h => Elem("th", Vector("scope" -> "col"), text = Some(h)))))))
      val body = Elem("tbody", Vector.empty, rows.map(r =>
        Elem("tr", Vector.empty, r.map(c => Elem("td", Vector.empty, Vector(elem(c)))))))
      Elem("table", keyed(key, Vector("className" -> "okay-table")), (cols ++ head) :+ body)
    // the React host claims no other semantic node: it draws the
    // lowering, which is the node's meaning — asked for with THIS
    // host's vocabulary, so a node it does claim survives inside one
    // it does not (see `Vocabulary`). The recursion terminates because
    // every name in that set is matched above, so a lowered node is
    // never handed back to this case
    case semantic => elem(Ui.lower(semantic, Vocabulary))

  /** a style declaration appended to an element's own */
  private def styled(e: Elem, decl: String): Elem =
    e.props.indexWhere(_._1 == "style") match
      case -1 => e.copy(props = e.props :+ ("style" -> decl))
      case i => e.copy(props = e.props.updated(i, "style" -> (e.props(i)._2 + ";" + decl)))

  private def keyed(key: String, props: Vector[(String, String)]): Vector[(String, String)] =
    if key.isEmpty then props else ("data-key" -> key) +: props

  /** the DOM event a rendered node reports, back as OUR event — the
   * other half of the glue, pure as well */
  def event(ui: Ui, key: String, kind: String, value: String): Option[Event] =
    Ui.focusable(ui).collectFirst {
      case Button(_, k, _) if k == key && kind == "click" => Event.Pressed(k)
      case Input(_, k, _, _, _) if k == key && kind == "input" => Event.Edited(k, value)
      case Check(on, k, _) if k == key && kind == "change" => Event.Toggled(k, !on)
      case Select(o, _, k) if k == key && kind == "change" =>
        Event.Chosen(k, math.max(o.indexOf(value), 0))
    }
}
