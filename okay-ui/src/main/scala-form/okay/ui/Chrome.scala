package okay.ui

/**
 * WHAT A PAGE IS DRAWN WITH, chosen per request (specs/app-host.md):
 * the application's places (`Shell`), whether this is the app's own
 * window — the sidebar, the ways (`Enhance`), a page that keeps itself
 * fresh by fetching — or a browser, where the site's own strip goes
 * above the page; and the notices an application shows above every
 * page (an update to take, a lease held on another computer).
 *
 * The word is the UI's for the frame around a page's content, kept
 * on purpose: okay-ui's `Frame` is the terminal's.
 *
 * ONE CORE, TWO FACES (okay-watch's specs/app-window.md): the same page
 * is drawn under either, and which one is the REQUEST's to say —
 * a `Chrome` is what `Route.provided` / `Router.provided` install for a
 * handler, never a global and never a thread's state.
 */
final case class Chrome(shell: Shell, app: Boolean, notices: Vector[String] = Vector.empty)

object Chrome:
  /** the body under its frame: the sidebar with the notices above the
   * page when `app`; the site's `strip` (the product's own — a site's
   * navigation is content), the notices and the page otherwise */
  def html(c: Chrome, here: String, body: String, strip: String = ""): String =
    val above = c.notices.mkString
    if c.app then Shell.html(c.shell, here, above + body) else strip + above + body

  /**
   * A WHOLE DOCUMENT: the doctype, the charset, the viewport (without
   * it a phone lays the page out at 980px and scales it down), the
   * title, the product's own `head`, a `refresh` of N seconds that
   * FETCHES in the app (`okay-refresh`, specs/ui-app.md — a reload
   * would jump) and reloads on the site, the app's face and ways when
   * `app`, the product's trailing `script` (its live client).
   */
  def document(c: Chrome, here: String, body: String, title: String, head: String = "",
               refresh: Int = 0, script: String = "", strip: String = ""): String =
    val meta =
      if refresh <= 0 then ""
      else if c.app then s"""<meta name="okay-refresh" content="$refresh">"""
      else s"""<meta http-equiv="refresh" content="$refresh">"""
    val face = if c.app then s"<style>$css${Shell.css}${Enhance.css}</style>" else ""
    val ways = if c.app then s"<script>${Enhance.script}</script>" else ""
    s"""<!doctype html><html><head><meta charset="utf-8">""" +
      s"""<meta name="viewport" content="width=device-width, initial-scale=1">""" +
      s"""<title>${Html.escape(title)}</title>$meta$head$face</head>""" +
      s"""<body>${html(c, here, body, strip)}$ways$script</body></html>"""

  /**
   * THE APP'S FACE: the system's font, crisper and a little larger than
   * a website (okay-watch's operator, 2026-09-24: «шрифты немного
   * крупнее четче»; «еще крупнее и еще четче» — 18px, black on light),
   * as custom properties a product may set; the sidebar at the page's
   * size; the engine's own subpixel smoothing (`antialiased` draws
   * thinner); the engine draws the system font in two weights, so bold
   * is 700, never 600.
   */
  val css: String =
    ":root{--okay-base:18px;--okay-muted:#3a3f46;--okay-fg:#000}" +
      "@media (prefers-color-scheme: dark){:root{--okay-muted:#c4c9d1;--okay-fg:#fff}}" +
      "body{font-family:-apple-system,BlinkMacSystemFont,\"Segoe UI\",system-ui,sans-serif;" +
      "-webkit-font-smoothing:subpixel-antialiased;text-rendering:optimizeLegibility}" +
      ".okay-main input,.okay-main textarea,.okay-main select,.okay-main button{font-size:1rem}" +
      ".okay-main h1,.okay-main h2{font-weight:700}" +
      ".okay-bold,.okay-tone-emphasis,button.okay-primary{font-weight:700}" +
      ".okay-side{width:15.5rem;padding:18px 12px;gap:4px}.okay-brand{font-size:1.15rem;padding:4px 12px 18px}" +
      ".okay-mark{width:15px;height:15px}.okay-place{font-size:1.05rem;font-weight:500;padding:9px 12px;gap:11px}" +
      ".okay-icon{width:1.3rem;font-size:1.1rem}.okay-group{font-size:.78rem;font-weight:700;padding:18px 12px 6px}" +
      ".okay-main select{background-color:Field;color:FieldText}"
