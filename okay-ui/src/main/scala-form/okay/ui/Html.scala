package okay.ui

/**
 * The HTML host (specs/ui-html.md): a `Ui` tree rendered as HTML, and
 * a browser's POST read back as the events an `update` folds. It is
 * the oldest client there is — no script, no socket, no build step.
 *
 * NOT a `Host`, deliberately. A scriptless page holds no connection,
 * so there is no `events: Source[Event]` to give: the loop is HTTP
 * itself, this object is its two halves, and saying that in the type
 * is honest. The terminal, Swing and GTK are hosts WITH a loop; this
 * is a host without.
 *
 * Strings only, so it rides wherever the tree does. What belongs to a
 * container rather than to HTML — the request, the session, the mount
 * — stays in okay-script (`mountPlain`, `Live.post`).
 */
object Html:

  /** the hidden field a mounted form carries, naming the mount */
  val MountField = "__okay_plain"

  /** what a keyed button posts under */
  val PressField = "__press"

  /** the tree as HTML -- the SAME structure `React.elem` builds and
   * the browser's patch consumer navigates, so a path into one is a
   * path into the other. A page is complete without any script: what
   * the browser shows first is this. */
  def render(ui: Ui): String = render(ui, named = false)

  /** `named`: every input, check, select and textarea also carries
   * `name=` (its key) and a keyed button posts as `__press=<key>` --
   * what makes the rendered tree a form a browser can POST back */
  def render(ui: Ui, named: Boolean): String =
    val sb = new StringBuilder
    write(React.elem(ui), sb, named)
    sb.toString

  /** the tree as one `<form method="post">`: `render(named = true)`
   * plus the hidden field naming this mount. This IS the client. */
  def form(id: String, ui: Ui, action: String): String =
    val safe = escape(id)
    s"""<form method="post" action="${escape(action)}" id="okay-live-$safe" class="okay-plain">""" +
      s"""<input type="hidden" name="$MountField" value="$safe">""" +
      render(ui, named = true) + "</form>"

  /**
   * The way in: the fields a browser posted, read against the tree
   * they were rendered from. A post is a DIFF -- an Input, Check or
   * Select whose posted value differs from the shown one is an
   * `Edited`/`Toggled`/`Chosen`, an unchanged one is nothing (an
   * unposted checkbox is `false`, as HTML has it) -- and then the
   * press: `__press=<key>` is a `Pressed`, or, when `key` is a
   * `Form`'s own, that form's edits travel inside ONE `Submitted`,
   * the hybrid rule read backwards. Every event is checked with
   * `Wire.permitted` against `shown`: the capability rule of the
   * socket, on the other road.
   */
  def events(shown: Ui, fields: Map[String, String]): Vector[Event] =
    val edits: Vector[Event] = Ui.focusable(shown).flatMap {
      case Ui.Input(v, k, _, _, _) => fields.get(k).filter(_ != v).map(Event.Edited(k, _))
      case Ui.Check(on, k, _) => Option.when(fields.contains(k) != on)(Event.Toggled(k, !on))
      case Ui.Select(os, i, k) => fields.get(k).map(os.indexOf).filter(j => j >= 0 && j != i).map(Event.Chosen(k, _))
      case _ => None
    }
    val forms = Ui.forms(shown)
    val press = fields.get(PressField)
    val submitted = press.flatMap(forms.get)
    // the edits of the form being submitted go INSIDE its Submitted;
    // every other edit goes on its own, before the press
    val own = submitted.fold(edits)(fs => edits.filterNot(e => keyed(e).exists(fs)))
    val last = press.map { k =>
      submitted.fold(Event.Pressed(k))(fs => Event.Submitted(k, edits.filter(e => keyed(e).exists(fs))))
    }
    (own ++ last).filter(Wire.permitted(shown, _))

  /** `events`, folded: the step one request makes */
  def step[S](view: S => Ui, update: (S, Event) => S)(s: S, fields: Map[String, String]): S =
    events(view(s), fields).foldLeft(s)(update)

  private def keyed(e: Event): Option[String] = e match
    case Event.Edited(k, _) => Some(k)
    case Event.Toggled(k, _) => Some(k)
    case Event.Chosen(k, _) => Some(k)
    case _ => None

  private def write(e: Elem, sb: StringBuilder, named: Boolean): Unit =
    sb ++= "<" ++= e.tag: Unit
    val key = e.props.collectFirst { case ("data-key", k) => k }
    for (k, v) <- e.props do
      k match
        case "className" => attr(sb, "class", v)
        case "checked" => if v == "true" then sb ++= " checked": Unit
        case _ => attr(sb, k, v)
    if named then
      key.foreach { k =>
        e.tag match
          case "input" | "select" | "textarea" => attr(sb, "name", k)
          case "button" =>
            attr(sb, "name", PressField)
            attr(sb, "value", k)
          case _ => ()
      }
      if e.tag == "input" && e.props.exists(_ == ("type", "checkbox")) then attr(sb, "value", "on")
    sb ++= ">": Unit
    // the void elements this renderer can emit: no text, no
    // children, no closing tag (`<col>` joined them with the browser
    // vocabulary — `</col>` is not HTML)
    if e.tag != "input" && e.tag != "img" && e.tag != "col" then
      // a textarea's value is its content, not an attribute
      if e.tag == "textarea" then e.props.collectFirst { case ("value", v) => v }.foreach(v => sb ++= escape(v): Unit)
      e.text.foreach(t => sb ++= escape(t): Unit)
      e.children.foreach(write(_, sb, named))
      sb ++= "</" ++= e.tag ++= ">": Unit

  private def attr(sb: StringBuilder, k: String, v: String): Unit =
    sb ++= " " ++= k ++= "=\"" ++= escape(v) ++= "\"": Unit

  def escape(s: String): String =
    val sb = new StringBuilder
    s.foreach {
      case '&' => sb ++= "&amp;": Unit
      case '<' => sb ++= "&lt;": Unit
      case '>' => sb ++= "&gt;": Unit
      case '"' => sb ++= "&quot;": Unit
      case c => sb += c: Unit
    }
    sb.toString
