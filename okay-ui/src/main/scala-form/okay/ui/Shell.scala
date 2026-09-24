package okay.ui

/**
 * AN APPLICATION'S FRAME for the HTML host (specs/ui-app.md): its name,
 * a sidebar of grouped places with the reader's own marked, and the
 * page beside it — for a plain page and a live one alike, because it
 * frames the page's HTML and never looks inside it.
 *
 * NOT a `Ui` node: a frame around a page is the container's business,
 * as `Html.form`'s mount is, and a new case would have to be drawn by
 * the terminal, Swing and Telegram hosts, which have no page to frame.
 */
final case class Shell(brand: String, groups: Vector[Shell.Group], footer: Vector[Shell.Item] = Vector.empty)

object Shell:
  /** a place: its words, where it is, and a glyph before it (may be empty) */
  final case class Item(label: String, href: String, icon: String = "")
  /** places under a heading; an empty title is a group with none */
  final case class Group(title: String, items: Vector[Item])

  /** the body, framed: the sidebar and `main.okay-main` beside it */
  def html(shell: Shell, here: String, body: String): String =
    val sb = StringBuilder()
    def put(parts: String*): Unit = parts.foreach(sb ++= _)
    put("""<div class="okay-app"><nav class="okay-side"><div class="okay-brand"><span class="okay-mark"></span>""",
      Html.escape(shell.brand), "</div>")
    shell.groups.foreach { g =>
      if g.title.nonEmpty then put("""<div class="okay-group">""", Html.escape(g.title), "</div>")
      g.items.foreach(i => put(item(i, here)))
    }
    put("""<div class="okay-grow"></div>""")
    shell.footer.foreach(i => put(item(i, here)))
    put("""</nav><main class="okay-main">""", body, "</main></div>")
    sb.toString

  /** the reader is HERE: the place itself, or a page under it; `/` only at `/` */
  def current(href: String, here: String): Boolean =
    val at = here.takeWhile(c => c != '?' && c != '#')
    at == href || (href != "/" && at.startsWith(href.stripSuffix("/") + "/"))

  private def item(i: Item, here: String): String =
    s"""<a class="${if current(i.href, here) then "okay-place okay-here" else "okay-place"}" href="${Html.escape(i.href)}">""" +
      s"""<span class="okay-icon">${Html.escape(i.icon)}</span>${Html.escape(i.label)}</a>"""

  /** the frame's own rules; the colours are the six custom properties `Html.css` sets */
  val css: String =
    """.okay-app { display: flex; min-height: 100vh; }
      |body:has(.okay-app) { margin: 0; }
      |.okay-side { width: 13.5rem; flex: none; display: flex; flex-direction: column; gap: 2px; padding: 14px 10px;
      |  box-sizing: border-box; position: sticky; top: 0; height: 100vh; -webkit-user-select: none; user-select: none;
      |  cursor: default; background: color-mix(in srgb, var(--okay-fg) 5%, transparent);
      |  border-right: 1px solid var(--okay-line); }
      |.okay-brand { font-weight: 700; padding: 3px 10px 14px; display: flex; align-items: center; gap: 8px; }
      |.okay-mark { width: 13px; height: 13px; border-radius: 3px; background: var(--okay-accent); }
      |.okay-place { display: flex; align-items: center; gap: 9px; padding: 7px 10px; border-radius: 6px;
      |  color: var(--okay-fg); text-decoration: none; font-size: 0.9rem; }
      |.okay-place:hover { background: color-mix(in srgb, var(--okay-fg) 7%, transparent); }
      |.okay-place.okay-here { background: var(--okay-accent); color: #fff; }
      |.okay-icon { width: 1.1rem; text-align: center; opacity: 0.8; }
      |.okay-group { font-size: 0.7rem; text-transform: uppercase; letter-spacing: 0.06em; color: var(--okay-muted);
      |  padding: 14px 10px 4px; }
      |.okay-grow { flex: 1; }
      |.okay-main { flex: 1; min-width: 0; padding: 22px 32px 48px; box-sizing: border-box; }
      |.okay-main a { text-decoration: none; }
      |.okay-main a:hover { text-decoration: underline; }
      |""".stripMargin
