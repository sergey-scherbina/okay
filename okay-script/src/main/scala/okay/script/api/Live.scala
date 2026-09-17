package okay.script.api

import okay.*
import okay.ui.{Event, Html, Ui, Wire}
import okay.TDict

/** A server-driven okay-ui app a page declares -- `Wire.serve`'s
 * three arguments, held as a value so the page can `mount` it and
 * the container can run a session for every WebSocket that connects.
 * See specs/okay-script.md "Live pages".
 *
 * Declare it once, at object level (a ```scala declare block), so the
 * app -- like a JSP declaration -- is built per compile, not per
 * request; mount it in prose with `${mount("counter", counter)}`.
 *
 * `push` (script-live-push) is the SERVER's own events — a clock, a
 * shared feed — merged into every session's input beside the
 * browser's, exactly `Ui.run`'s `external`; a fresh instance of the
 * source runs per session. A pushed event obeys the same capability
 * rule as a browser's (`Wire.permitted`): a `Pressed` must name a key
 * the shown tree has, which is how okay-ui's own timer test ticks.
 */
final class Live[S](val init: S, val view: S => Ui, val update: (S, Event) => S,
                    val push: Source[Event] = pure(()),
                    /** with a Schema, the state is also a SESSION ATTRIBUTE
                     * (script-live-durable): read before the in-memory copy,
                     * written on Closed -- so a restart, a second node sharing
                     * the sessions topic, and the TTL sweep all apply to it */
                    val schema: Option[okay.codec.Schema[S]] = None):
  /** the tree a fresh session shows first -- also the SSR content */
  def first: Ui = view(init)

  /** the state each session KEY (the container's session cookie)
   * last reached, so a socket that reconnects goes on rather than
   * starting over (script-live-resume). In memory, this process
   * only: a restart starts over, and a key is never evicted while
   * the process lives -- stated, human-scale, like `Hub`. Held
   * inside the app so the type stays `S` and nothing is cast. */
  private val resumed = TDict.empty[String, S]

  /** one session: event lines in, tree/patch lines out (okay-ui's
   * pure `Wire.serve`); with a `key`, it starts from the state that
   * key last reached and, on Closed, remembers the state it reached
   * -- its first frame, the full tree, puts a reconnecting browser
   * right. Without a key the final state is discarded. With a
   * Schema and a bound session (`attrs`), the durable copy under
   * `okay.live.<name>` is read first and written last. */
  def session(key: Option[String], attrs: Option[Session] = None, name: String = ""): Stage[String, String, Unit] =
    Wire.serve(load(key, attrs, name))(view)(update).map(store(key, attrs, name, _))

  /** the state a key holds: the durable copy first, then the
   * in-memory one, then `init` -- the socket road and the plain road
   * read the same two places */
  private def load(key: Option[String], attrs: Option[Session], name: String): S =
    val stored = for sc <- schema; a <- attrs; v <- a.get(s"okay.live.$name"); s <- Live.decode(sc, v) yield s
    stored.orElse(key.flatMap(resumed.get)).getOrElse(init)

  private def store(key: Option[String], attrs: Option[Session], name: String, s: S): Unit =
    key.foreach(k => resumed.put(k, s))
    for sc <- schema; a <- attrs do a.set(s"okay.live.$name", Live.encode(sc, s))

  /** the plain road's session (script-live-plain): the state `key`
   * holds, `Live.step`ped through `fields` when they name this
   * mount (`Live.PlainField` = `id`), and kept where `session` keeps
   * it -- a GET, or another mount's POST, steps nothing */
  def post(key: Option[String], id: String, fields: Map[String, String],
           attrs: Option[Session] = None, name: String = ""): S =
    val from = load(key, attrs, name)
    if !fields.get(Live.PlainField).contains(id) then from
    else
      val next = Live.step(this, from, fields)
      store(key, attrs, name, next)
      next

  /** `post`, rendered: the plain mount's HTML for this request --
   * held here so the state stays `S` (a `Live[?]` cannot hand its
   * state back to its own `view` from outside) */
  def plainHtml(key: Option[String], id: String, fields: Map[String, String],
                attrs: Option[Session], action: String): String =
    Live.plain(id, view(post(key, id, fields, attrs, id)), action)

  /** a session with no key: from `init`, remembering nothing */
  def session: Stage[String, String, Unit] = session(None)

  /** the state a key last reached, if any -- for a test, an admin page */
  def resumedFor(key: String): Option[S] = resumed.get(key)

object Live:
  def apply[S](init: S)(view: S => Ui)(update: (S, Event) => S, push: Source[Event] = pure(())): Live[S] =
    new Live(init, view, update, push)

  /** an app whose state survives the process: kept in the session
   * (script-live-durable), CBOR in base64, under `okay.live.<id>` */
  def durable[S](init: S)(view: S => Ui)(update: (S, Event) => S, push: Source[Event] = pure(()))
                (using sc: okay.codec.Schema[S]): Live[S] =
    new Live(init, view, update, push, Some(sc))

  /** a `Live.form`'s state: the partial value in the codec's own
   * shape, the errors under their fields, and the last submit's
   * message (okay-script-forms) */
  final case class FormState(value: okay.codec.Json, errors: Vector[(String, String)], message: Option[String])

  /** the key of a `Live.form`'s submit button -- `$` keeps it clear
   * of any field name */
  val SubmitKey = "$submit"

  /** a typed form as a Live app: okay-ui's `Form.ofWith[A]` plus a
   * submit button; `Form.edit` on every field event; on submit the
   * per-field errors, the decode, the cross-field `checks`, and --
   * only then -- `submit(a)` with the typed value, whose answer is
   * shown and the form cleared. The page never sees a `Json`. */
  def form[A](submit: A => String, checks: okay.ui.Form.Check[A]*)(using okay.codec.Schema[A]): Live[FormState] =
    formWith[A]("Submit")(submit, checks*)

  def formWith[A](label: String)(submit: A => String, checks: okay.ui.Form.Check[A]*)
                 (using okay.codec.Schema[A]): Live[FormState] =
    import okay.ui.Form
    val empty = Forms.defaults[A]
    // a Form node (ui-hybrid): the browser folds the fields itself and
    // sends them ONCE as Submitted when the button is pressed
    def view(st: FormState): Ui =
      val fields = Form.ofWith[A](st.errors)(st.value) match
        case c: Ui.Column => c.children
        case other => Vector(other)
      Ui.Column(Vector(Ui.Form(fields, label, SubmitKey))
        ++ st.errors.collect { case ("", m) => Ui.Text(s"! $m", okay.ui.Style(bold = true)) }
        ++ st.message.map(m => Ui.Text(m)).toVector)
    def submitNow(st: FormState): FormState =
      val errs = Form.errors[A](st.value)
      if errs.nonEmpty then st.copy(errors = errs, message = None)
      else Form.decode[A](st.value) match
        case Left(m) => st.copy(errors = Vector("" -> m), message = None)
        case Right(a) =>
          val failures = checks.flatMap(_(a)).toVector
          if failures.nonEmpty then st.copy(errors = failures, message = None)
          else FormState(empty, Vector.empty, Some(submit(a)))
    def update(st: FormState, e: Event): FormState = e match
      case Event.Pressed(SubmitKey) => submitNow(st)
      case Event.Submitted(SubmitKey, _) => submitNow(st.copy(value = Form.submitted[A](st.value, e)))
      case other => st.copy(value = Form.edit[A](st.value, other), message = None)
    Live(FormState(empty, Vector.empty, None))(view)(update)

  /** the hidden field a plain mount's form carries, naming the mount
   * -- `okay.ui.Html.MountField`, kept under its old name so no page
   * changes (specs/ui-html.md) */
  val PlainField: String = Html.MountField

  /** the plain road's step (script-live-plain): `Html.step` over this
   * app's own view and update -- the rule itself moved to okay-ui
   * with the rest of the HTML host (specs/ui-html.md) */
  def step[S](app: Live[S], s: S, fields: Map[String, String]): S =
    Html.step(app.view, app.update)(s, fields)

  /** the tree as one `<form method="post">` -- `Html.form` */
  def plain(id: String, ui: Ui, action: String): String = Html.form(id, ui, action)

  private[api] def encode[S](sc: okay.codec.Schema[S], s: S): String =
    java.util.Base64.getEncoder.encodeToString(okay.codec.Codecs.cbor(sc).encode(s))

  /** damage is `None`: a torn attribute is a session that starts over */
  private[api] def decode[S](sc: okay.codec.Schema[S], v: String): Option[S] =
    scala.util.Try(java.util.Base64.getDecoder.decode(v)).toOption
      .flatMap(b => okay.codec.Codecs.cbor(sc).decode(b).toOption)

  /** where the container serves the patch consumer */
  val JsPath = "/__okay/live.js"

  /** the tree as HTML, server-side -- `okay.ui.Html.render`. A page
   * is complete without any script: what the browser shows first is
   * this. */
  def html(ui: Ui): String = Html.render(ui)

  /** `named`: every field carries `name=` and a keyed button posts as
   * `__press=<key>` -- the plain `<form method="post">` road */
  def html(ui: Ui, named: Boolean): String = Html.render(ui, named)

  def escape(s: String): String = Html.escape(s)

/** Mounts a Live app here: its first tree as HTML (so the page is
 * whole without JavaScript), and the script that opens this page's
 * own WebSocket (`?__live=<id>`) to receive patches and send events.
 * Outside a `Site` (a bare `render`) the HTML still renders; there is
 * just no container to answer the socket. */
/** the head of an INSTALLABLE page (specs/frontend.md "Mobile"): the
 * viewport, the level-L stylesheet, the manifest "Add to Home Screen"
 * reads, and the service worker that opens the page offline. Put it
 * where the head goes; `start` is the page's own path (the default is
 * the page being rendered). */
def installable(name: String, start: String = ""): String =
  val at = if start.nonEmpty then start else Web.current.path
  okay.script.Mobile.head(name, at)

def mount(id: String, app: Live[?]): String =
  Container.liveRegistrar.foreach(register => register(id, app))
  // a Live page is stateful, so mounting one opens the session: the
  // cookie it sets is the key a reconnecting socket resumes by
  // (script-live-resume); outside a Site the session is the detached one
  Session.current.set("okay.live", "1")
  val safe = Live.escape(id)
  s"""<div id="okay-live-$safe" data-okay-live="$safe">${Live.html(app.first)}</div>""" +
    s"""<script src="${Live.JsPath}"></script><script>okayLive("$safe")</script>"""

/** Mounts a Live app on the plain road (script-live-plain): the
 * tree as one `<form method="post">` and no script -- every press is
 * a POST to `action` (this page, by default), folded by `Live.step`
 * into the state the session holds, and the state reached is what
 * renders. The session is opened as `mount` opens it, so the cookie
 * is the key -- the same key, and the same state, the socket road
 * resumes by. */
def mountPlain(id: String, app: Live[?], action: String = Web.current.path): String =
  Session.current.set("okay.live", "1")
  val web = Web.current
  val key = Option(Session.current.id).filter(_.nonEmpty)
  val fields = if web.method == "POST" then web.form else Map.empty[String, String]
  app.plainHtml(key, id, fields, Some(Session.current), action)
