# okay-ui HTML host — the plain road's pure half, where a product can reach it

## Overview

script-live-plain (specs/okay-script.md, 2026-09-17) built the plain
road of a Live app: the tree as one `<form method="post">`, a POST
diffed against the shown tree into the events the socket would have
sent, no script. Three of its four parts are PURE — `Live.html`
(tree → HTML), `Live.plain` (the form around it), `Live.step` (fields
→ events → state) — and all three live in `okay.script.api.Live`,
whose module depends on `okayStaging` (the compiler), `okayDeploy`,
`okayAcme`, `okayTls` and `okayJetty`. Only the fourth part,
`mountPlain`, needs any of that: it reads `Web.current` and opens
`Session.current`, which are the container's.

The consumer that found this is okay-watch (`../okay-watch`,
specs/ui.md there): a product that rides okay as a submodule, has its
own Jetty router and its own door (`Analysts`, bearer keys over TLS),
and wants the analyst page as a `Ui` value drawn without a script. It
cannot take okay-script for three functions of strings, and it should
not have to: an HTML renderer of a tree is a HOST of okay-ui, exactly
as the terminal, Swing and GTK are, and the module that defines the
tree is where its hosts live.

So this spec moves the pure half into okay-ui as `okay.ui.Html`, and
REVISES a decision written the same day in specs/okay-script.md:
"the step lives in okay-script, not okay-ui, because it reads HTML
field names, and okay-ui has no notion of a name attribute". That
reasoning proves too much — the terminal host has a notion of ANSI
and the Swing host of `JComponent`, and neither belongs to the tree.
A host knows its medium; the tree does not. What stays in okay-script
is what belongs to the container: the request, the session, the
mount.

## Interface

```scala
package okay.ui

/** The HTML host. Stateless on purpose: a browser without a script
 * holds no connection, so this host has no `events: Source[Event]`
 * and is not a `Host` — it is the two halves of one request,
 * `render` on the way out and `events` on the way in, and the loop
 * is HTTP itself. Cross-platform (strings only): a Native or JS
 * server serves it as well as the JVM. */
object Html:
  /** the tree as HTML — the structure `React.elem` builds and the
   * browser's patch consumer walks, so a path into one is a path into
   * the other; escaped; complete without any script */
  def render(ui: Ui): String
  /** `named`: every input, check, select AND textarea carries `name=`
   * (its key), a keyed button posts as `__press=<key>` */
  def render(ui: Ui, named: Boolean): String
  /** the tree as one `<form method="post" action=...>`: `render(named =
   * true)` plus a hidden field naming this mount */
  def form(id: String, ui: Ui, action: String): String
  val MountField = "__okay_plain"
  val PressField = "__press"
  /** the way in: a POST diffed against the tree it was rendered from.
   * An Input/Check/Select whose posted value differs from the shown
   * one is Edited/Toggled/Chosen (an unposted checkbox is false);
   * `__press` is a Pressed, or, naming a Form's own key, that form's
   * edits inside ONE Submitted; every event passes `Wire.permitted`
   * against `shown` — the same capability rule as the socket's */
  def events(shown: Ui, fields: Map[String, String]): Vector[Event]
  /** `events`, folded: the step a request makes */
  def step[S](view: S => Ui, update: (S, Event) => S)(s: S, fields: Map[String, String]): S
  def escape(s: String): String
```

okay-script keeps its names as one-line delegates so no page changes:
`Live.html` → `Html.render`, `Live.plain` → `Html.form`, `Live.step`
→ `Html.step(app.view, app.update)`, `Live.PlainField` → `Html.MountField`,
`Live.escape` → `Html.escape`. `mountPlain`, `Live.post` and the
`load`/`store` pair stay where they are: they are the container's.

## Behavior

Stage 1 — the move (claim: ui-html-host, after this spec):
- [x] `Html.render` renders every shape to the HTML `React.elem`
      implies, escaped — TestLive's render test, moved to okay-ui's
      `TestHtml` verbatim (the JVM test tree; the object is in the
      shared source directory)
- [x] `Html.events` on a shown tree: unchanged fields are nothing, a
      changed Input/Check/Select is one event, an unposted checkbox
      is `Toggled(false)` only when shown on, a Select posts by option
      text, a value that is no option is nothing; `__press` of a shown
      key is `Pressed`, of an unshown key nothing; a `Form`'s own key
      folds that form's edits into ONE `Submitted` and leaves the
      others plain; a `+` inside the form is a `Pressed` after plain
      edits — TestLivePlain's step tests, moved, with the recorder app
      replaced by asserting on `events` directly (no `Live` here)
- [x] `Html.form` is one form with the hidden mount field, every field
      named, the textarea too, and no `<script`
- [x] okay-script's `Live.html`/`plain`/`step` delegate: TestLive,
      TestForms, TestLivePlain and TestLiveResume pass UNCHANGED —
      the delegates are the proof that no page notices
- [x] `Html` compiles on all three platforms (it is in the shared
      directory, `sbt okayUiJS/compile okayUiNative/compile`), and
      `okayUiJVM`'s dependency classpath carries no okay-script — the
      property okay-watch depends on, asserted by `show
      okayUiJVM/dependencyClasspath | grep -c okay-script` = 0 in the
      landing notes

Stage 2 — the browser's half of the live road, for the same consumer
(claim: ui-live-js, only when okay-watch's specs/ui.md reaches its
stage 3):
- [ ] `LiveJs.source` (226 lines of plain JavaScript, okay-script)
      moves beside `Html` as `okay.ui.LiveJs` — it is the browser's
      client of `Protocol`, and a product with its own WebSocket route
      (okay-jetty's) needs the client without the container; okay-script
      serves it from there at the same path

## Decisions

- **A host, not a `Host`.** `Html` has no event source because a
  scriptless page has no connection: the loop is the request cycle.
  Forcing it under `Host` would mean a `Source` that never yields and
  a `render` that returns Unit while the caller needs the string. The
  terminal, Swing and GTK are hosts with a loop; this one is a host
  without, and saying so in the type is honest.
- **Shared directory, not `scala-jvm`.** Nothing in it is
  platform-specific — StringBuilder and Map — and a Native server
  (okay-script's own image is JVM, but okay-http has a Native leg)
  loses nothing by having it.
- **Delegates, not a deprecation cycle.** okay-script's names are used
  by pages in prose (`Live.html` in docs and in the example store);
  five one-line delegates cost nothing and keep every page compiling.
  If a later lane wants them gone, it is a grep.
- **The revised decision is recorded, not erased.** specs/okay-script.md's
  "the step lives in okay-script" gets a one-line pointer here rather
  than a rewrite: the history of why it moved is worth more than a
  clean page.
- **Why now.** okay-watch is the first product to want the analyst
  page as a tree drawn by a plain browser, and it is a submodule
  consumer 201 commits behind (2026-09-17): its bump lands once, and
  it should land against a tree that already has `Html`, so the
  product never depends on okay-script even transitionally.

## Results

Stage 1 LANDED 2026-09-17. `okay.ui.Html` is 130 lines;
okay-script's `Live.html`/`plain`/`step`/`escape`/`PlainField` are
five delegates, and TestLive, TestLivePlain, TestForms and
TestLiveResume pass UNCHANGED (18 tests) — which is the whole proof
that no page notices. `TestHtml` in okay-ui, 7 tests. `okayUiJVM`'s
dependency classpath carries okay, okay-codec, okay-lex, okay-parse
and okay-persist, and nothing of okay-script: measured, not assumed.

Two things the move taught:

- **`scala-form` IS the cross-platform directory.** The spec said
  "the shared directory, not `scala-jvm`", meaning `src/main/scala`.
  But `Html.events` needs `Wire.permitted` — the capability rule must
  not be defined twice — and `Wire` lives in `src/main/scala-form`,
  which build.sbt adds to the JVM, JS AND Native source sets alike.
  So `Html` sits beside `Wire` there, and the property the spec
  wanted (it compiles on all three) holds as written:
  `okayUiJS/compile` and `okayUiNative/compile` are green.
- **An EMPTY post is not the identity, and the mount field is what
  makes a GET safe.** `Html.events(shown, Map.empty)` on a tree with a
  `Check` shown ON answers `Toggled(false)`: HTML's own rule is that
  an unposted checkbox is unchecked, and an empty map is
  indistinguishable from a form posted with everything cleared. What
  protects a GET is therefore `Live.post`'s guard — it steps only when
  the post carries THIS mount's id — never the emptiness of the map.
  A consumer rendering the tree itself (okay-watch's `/ui`, its
  specs/ui.md) must keep that guard or a refresh will untick boxes.
