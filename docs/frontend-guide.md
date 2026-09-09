# Frontends in okay — one application, any client

This is the guide. The decisions and their measurements are in
`specs/frontend.md` and `specs/ui.md`; the wire contract a client in
another language implements is `docs/protocol/frontend.md`.

## The idea in one paragraph

A frontend's logic belongs to the application, not to the technology
that draws it. In okay a screen is a VALUE — a tree of widgets with
keys — an application is a pure fold of events into state, and
drawing is a HOST's job. The same application runs on a terminal,
under React, on the raw DOM, in a Swing or GTK window, and over the
wire to a browser or a native phone client that never changes when
the server does. What crosses the wire is a small protocol with one
definition, rendered into a document and a conformance script, so a
client written in Kotlin or Swift needs nothing of okay.

## 1. An application

```scala
import okay.ui.*

def view(n: Int): Ui = Ui.Column(Vector(
  Ui.Text(s"count: $n", Style(tone = Tone.Emphasis)),
  Ui.Row(Vector(Ui.Button("-", "dec"), Ui.Button("+", "inc", Role.Primary)))))

def update(n: Int, e: Event): Int = e match
  case Event.Pressed("inc") => n + 1
  case Event.Pressed("dec") => n - 1
  case _ => n
```

- `view` is `S => Ui`, pure. `update` is `(S, Event) => S`, pure.
- Widgets carry KEYS; events name keys. There are no closures in the
  tree — that is what makes it a value with equality, a diff, and a
  wire shape.
- The tree the user sees is the CAPABILITY LIST: an event naming a
  key that is not on it is dropped before `update` sees it.

## 2. The tree: two levels

**Level L** — the closed layout vocabulary every host and every
client draws:

| Node | What it is |
|---|---|
| `Text(s, style)` | text with TOKENS: `Style(bold, dim, tone, size)`; a host maps `Tone.Danger` to its own red |
| `Row`, `Column` | children beside / below each other |
| `Box(children, dir, weights, gap, pad, key)` | the general container: weights divide the main axis, gap and pad in character units |
| `Image(src, alt)` | a picture, or its alt where a host has no loader |
| `Button(label, key, role)` | `Role.Plain / Primary / Danger / Active` |
| `Input(value, key, label, kind, live)` | `InputKind.Text / Secret / Multiline / Number`; `live` is explained in §5 |
| `Check(on, key, label)`, `Select(options, selected, key)` | |
| `Scroll(child, key)` | |
| `Form(fields, submit, key)` | fields and a submit button keyed like the form — the hybrid rule of §5 lives on it |

**Level S** — the open semantic vocabulary: `Items`, `Table`, `Tabs`,
`Modal`, `Disclosure`. Each is DEFINED by its lowering to level L
(`Ui.lower`). A client that claims a semantic node draws it natively
(a real table); one that does not receives the lowering, and the
server cannot tell the difference — `Ui.keys(node) ==
Ui.keys(Ui.lower(node))` is a tested law. Write semantics; drop to
`Box` where you want a non-standard look.

Pixels never enter the tree. Style is tokens, layout is weights and
character units: a terminal stays a terminal, a phone looks like a
phone.

## 3. Running it in-process

`Ui.run(init)(view)(update)(host)` is the loop: host events and any
external sources are MERGED (a ticking clock, a server push — there
is no `Cmd` type; subscriptions are `merge`). Hosts:

| Host | Where | Notes |
|---|---|---|
| `Terminal.host()` | JVM, Native | ANSI, raw mode by `stty`; `Frame.render` is the pure half, testable as lines |
| `ReactJs.host(react, root)` | Scala.js | `React.elem` is pure and JVM-tested; five lines of glue |
| `Ui.diffing(Dom.backend(document, root))` | Scala.js | raw DOM, zero dependencies, driven by the core diff |
| `Swing.host(panel)`, `Swing.window(title)(app)` | JVM | zero dependencies, headless-tested |
| `okay.ui.gtk.Gtk.host(box)`, `Gtk.window(title)(app)` | Scala Native | GTK 4; present only where `pkg-config --exists gtk4` answers (`brew install gtk4 pkg-config`) |
| the test host | tests | renders to a value, feeds scripted events — the whole loop with no screen |

A `Host` takes the whole tree; a `Backend` takes patches, and
`Ui.diffing` turns one into the other with the core diff (keyed
children move rather than rebuild; the law "diff then patch equals
the next tree" is tested on every node and at every backend).

Scenarios — a wizard, a dialog — are PROGRAMS, not folds:
`Dialog.show(ui)` awaits one event; `Form.ask[A]` renders a form from
a `Schema[A]` and answers the typed value; `Nav` is a stack of
screens. See `specs/ui.md`.

## 4. Over the wire: a server-driven frontend

```scala
Wire.serve(init)(view)(update): Stage[String, String, S]
```

is a PURE stage — lines in, lines out — that runs over channels, a
WebSocket, or stdio. The conversation (`okay.ui.Protocol`, one
derived definition, JSON lines or CBOR bytes):

```
client → server   Hello {vocab, version}   first, once
server → client   Tree {ui}                the full tree, then
server → client   Patch {patch}            narrow patches
client → server   Event {event}            what the user did
either            Close
```

`Hello.vocab` names the semantic nodes the client draws; everything
else is lowered by the server before it is sent. `Wire.client(host,
vocab)` is the Scala client over any host. Sessions are event-sourced
(`Sessions`: journal, refold, snapshot), so a reconnect resumes.

The contract for a client in another language is RENDERED from the
schemas — `docs/protocol/frontend.md` — with a shape language on one
screen, and `docs/protocol/conformance.jsonl` is the script a client
must reproduce. Both files fail a test when they drift from the code;
`OKAY_RENDER=1 sbt okayUiJVM/testOnly okay.ui.TestProtocol`
regenerates them.

## 5. The hybrid rule: no round trip per keystroke

A `Form`'s fields fold ON THE CLIENT: typing sends nothing, and the
form's button sends ONE `Submitted(key, edits)` carrying every field
as the `Edited`/`Toggled`/`Chosen` a live form would have sent. The
server folds them through the same `Form.edit` a live edit takes
(`Form.submitted`), so a submitted form cannot decode differently
from a typed one, and the client needs no schema.

- an `Input` with `live = true` sends `Edited` per change
  (search-as-you-type); so does any input outside a `Form`;
- a client that CLAIMS `tabs` or `disclosure` switches them locally;
- the server stays the truth: its `SetValue` lands on the client's
  field and wins;
- a `Submitted` that names a form not shown, a field not that form's,
  or an "edit" that is not one never reaches `update`.

## 6. Live pages: a frontend with no build step

In okay-script (`docs/okay-script-guide.md`), a page declares an app
and mounts it:

```markdown
```scala declare
import okay.ui.*
import okay.script.api.*
val counter = Live(0)(view)(update)
```
<!doctype html><html><head>${installable("Counter")}</head><body>
${mount("counter", counter)}
</body></html>
```

`mount` renders the first tree as HTML (the page is whole without
JavaScript) and opens the page's own WebSocket; `live.js` (~160 lines,
dependency-free, served by the container) is a level-L client of the
protocol. `Live.form[A]` is a typed form as a Live app.

`installable(name)` (§7) is the mobile web: viewport, a mobile-first
stylesheet for level L, a web manifest, a service worker.

## 7. Mobile

**Web.** `installable("Name")` in a page's head gives: a viewport,
`/__okay/app.css` (flex rows and columns, 44px tap targets, 16px
inputs so iOS does not zoom, tokens as classes), a manifest ("Add to
Home Screen" opens the page standalone with the page as its start
URL), and `/__okay/sw.js`, a service worker keeping the SHELL —
network first, cache on failure — so the page opens offline as it was
last seen and the socket reconnects when it can. Proved through a
real headless browser in an iPhone emulation (`TestMobileWeb`, in
okay-demo-e2e-browser, `sbt integrationTest`).

**Native.** A native client is a thin client of the protocol in the
platform's own language; it draws level L, claims nothing, and never
changes when the application does:

- `okay-compose/` — Kotlin, Compose Desktop today (Android is the
  same composables once an SDK is on the build machine), the JDK's
  WebSocket, no okay dependency. `./gradlew :protocol:test` replays
  the conformance script; `./gradlew :app:run --args "<ws url>"`
  opens a window; `:app:smoke` is a headless check against a running
  server.
- iOS: a Swift package over the same document is the road (SwiftUI
  is a thin client like Compose; Cocoa from Scala Native is
  `objc_msgSend` all the way and is not worth it).

Writing your own client: read `docs/protocol/frontend.md` (the
shapes fit on one screen), apply patches by path, hold the tree,
implement the hybrid rule of §5, and replay
`docs/protocol/conformance.jsonl` — every `in` applied, every `tree`
held, every `out` produced. That is the whole conformance.

## 8. Where things are

| | |
|---|---|
| `okay-ui` | the tree, the diff, the loop, Form/Dialog/Nav, Protocol, Wire, Sessions, the terminal/React/DOM/Swing hosts |
| `okay-ui-gtk` | the GTK 4 host (Scala Native) |
| `okay-script` | Live pages, `live.js`, `installable` and the mobile files |
| `okay-compose` | the Kotlin/Compose client |
| `docs/protocol/` | the rendered contract and the conformance script |
| `specs/frontend.md`, `specs/ui.md`, `specs/ui-toolkit.md` | the decisions, with their measurements |
