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

  /**
   * THE LEVEL-L STYLESHEET, beside the tree whose classes it names.
   *
   * `React.elem` writes `okay-row`, `okay-tone-danger`, `okay-table`
   * and a dozen more, and until ui-html-css the only file that knew
   * those names was okay-script's phone stylesheet — so a product
   * serving `Html.render` itself started from unstyled HTML and wrote
   * the token map again by hand (okay-watch did, twice, including
   * fifteen `nth-child` selectors for what `Kind` now says). The
   * client of this tree lives beside the tree (`LiveJs`, ui-html
   * stage 2); so does its style.
   *
   * THEMING IS SIX CUSTOM PROPERTIES, not a fork of these rules: a
   * product sets `--okay-fg`, `--okay-muted`, `--okay-accent`,
   * `--okay-danger`, `--okay-line` and `--okay-base` on `:root` and
   * changes nothing else. What is NOT here is layout the tree did not
   * say: this file styles what a node MEANS and leaves where things
   * sit to `Box`'s own weights, gap and pad.
   *
   * A CELL WRAPS; IT NEVER ELLIPSIZES. The rule is okay-watch's, from
   * a page whose readers must CHECK what it says: `DE000000000…` can
   * be checked against nothing, copied nowhere and compared with
   * nothing, while an IBAN over two lines can be all three.
   * `anywhere`, because an IBAN and a transaction hash have no spaces
   * to break at; the header row keeps the ordinary word rules, or
   * "weight" comes out as "weigh / t".
   */
  val css: String =
    """:root {
      |  color-scheme: light dark;
      |  --okay-fg: #111; --okay-muted: #777; --okay-accent: #2563eb;
      |  --okay-danger: #dc2626; --okay-line: #ddd; --okay-base: 16px;
      |  --okay-gap: 8px;
      |  --okay-mono: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace;
      |}
      |body { color: var(--okay-fg); font-size: var(--okay-base); }
      |/* the tree's own containers: a Box says the rest itself */
      |.okay-col, .okay-box.okay-v, .okay-form { display: flex; flex-direction: column; gap: var(--okay-gap); }
      |.okay-row, .okay-box.okay-h { display: flex; flex-direction: row; gap: var(--okay-gap); align-items: baseline; }
      |.okay-scroll { overflow: auto; }
      |img { max-width: 100%; height: auto; }
      |/* what a text IS (ui-text-intent), and how it reads */
      |.okay-bold, .okay-tone-emphasis { font-weight: 600; }
      |.okay-dim, .okay-tone-muted { color: var(--okay-muted); }
      |.okay-tone-danger { color: var(--okay-danger); }
      |.okay-size-small { font-size: 0.85em; }
      |.okay-size-large { font-size: 1.4em; }
      |.okay-kind-ident { font-family: var(--okay-mono); }
      |.okay-kind-number { font-variant-numeric: tabular-nums; }
      |.okay-align-end { text-align: end; margin-inline-start: auto; }
      |/* a button's role, as a token and not a colour the page picked */
      |button.okay-primary { background: var(--okay-accent); color: #fff; border-color: var(--okay-accent); }
      |button.okay-danger { background: var(--okay-danger); color: #fff; border-color: var(--okay-danger); }
      |button.okay-active { font-weight: 600; text-decoration: underline; }
      |/* the one semantic node a browser draws itself (ui-browser-vocab) */
      |.okay-table { width: 100%; border-collapse: collapse; table-layout: fixed; }
      |.okay-table th { text-align: start; font-weight: 600; border-bottom: 1px solid var(--okay-line); padding: 6px 8px 6px 0; overflow-wrap: normal; }
      |.okay-table td { border-bottom: 1px solid color-mix(in srgb, var(--okay-line) 50%, transparent); padding: 8px 8px 8px 0; vertical-align: baseline; overflow-wrap: anywhere; }
      |""".stripMargin

  /** the class a mounted form carries, and the prefix of its id: what
   * the live client and `Enhance` know it by */
  val PlainClass = "okay-plain"
  val LivePrefix = "okay-live-"

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
    s"""<form method="post" action="${escape(action)}" id="$LivePrefix$safe" class="$PlainClass">""" +
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
