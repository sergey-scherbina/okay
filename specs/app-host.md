# app-host — one code, online and on the desktop: what the platform owes an application

## Overview

okay-watch's operator, 2026-09-28, after its desktop app lost its
sidebar (okay-watch bug app-frame-lost): *«окей должен предоставить
все необходимое для создания подобных приложений как окей ватч где
один и тот же код работает как в онлайн так и на десктопе причем
достаточно нативно … в ватче только бизнес логика — а в окей все
связанное с платформой и инфраструктурой»*.

What the product had hand-rolled, read out of its code in the order
the bug found it:

1. **A capability from the request.** What a page is drawn with
   depends on the request (who is asking, from where, the app or the
   site). The product first kept it in process-wide vars (two
   services in one JVM drew each other's banner), then in a
   `ThreadLocal` set around MATCHING the request — which okay-ops's
   `Red.route` and `Lifecycle.route` never saw, because each builds
   the route it wraps inside its own effect, when the answer RUNS.
   okay ships the capability form twice, each for one value
   (`Secure.granted`: `Principal ?=> route`; `Traced.route`:
   `Tracer ?=> Route`) and a product writes a third. One general form
   is owed.
2. **The frame chosen per request.** ui-app gave the HTML host an
   application's frame (`Shell`) and ways (`Enhance`). What it did not
   give is the CHOICE — the same page framed by the sidebar in the
   app's window and by the site's strip in a browser — nor the notices
   an application shows above every page (an update to take, a lease
   held elsewhere), nor the document around it (viewport, a refresh
   that fetches in the app and reloads on the site, the live script).
   The product had all of it in strings.
3. **The route-wrapper law, unstated.** `Traced.route` owns the run
   of the route it wraps; `Red.route` and `Lifecycle.route` build the
   route late. Nothing said so, and it is exactly what anything
   thread-scoped around matching (okay-platform's own `Scoped`) falls
   through.
4. **The app's own window.** JavaFX's WebView over the local service:
   no address bar, the menus, a Save dialog for every download, the
   system browser for every outside link, print, About, the size and
   place remembered, a second start to the front, the data directory
   where the system keeps an application's — 577 lines of product code
   with the product's names in them, and nothing of it about
   blockchains.

## Interface

### okay-http — the capability from the request
- `Route.provided[E](env: Request => E)(route: E ?=> Route): Route` —
  a PartialFunction route written against `using E` serves under it;
  `env` is read from the request, for definedness and for the answer.
  The shape of `Traced.route`, for any value.
- `Router.provided[E, A, X](env: Request => E)(h: E ?=> (A, Request) => X): (A, Request) => X`
  — one handler of a `Router`, whose table is built once and must not
  be rebuilt per request: the capability is computed from the request
  inside the handler and CAPTURED by what it builds, so it holds
  however late a wrapper builds the answer and on whichever thread.

### okay-ui — the frame chosen per request (`scala-form`, beside `Shell`)
- `Chrome(shell: Shell, app: Boolean, notices: Vector[String])` — what
  a page is drawn with: the application's places, whether this is the
  app's own window (the sidebar) or a browser (the site's strip), and
  the lines shown above every page.
- `Chrome.html(c, here, body, strip)` — the body under its frame:
  `Shell.html` with the notices above the page when `app`; the site's
  `strip` (the product's own, it is a site's business), the notices and
  the page otherwise.
- `Chrome.document(c, here, body, title, head, refresh, script, strip)`
  — a whole document: doctype, charset, viewport, the title, the
  product's `head`, a `refresh` of N seconds that FETCHES in the app
  (`okay-refresh`, ui-app) and reloads on the site (`http-equiv`), the
  app's own style (`Chrome.css` + `Shell.css` + `Enhance.css`) and
  `Enhance.script` when `app`, the product's trailing `script`.
- `Chrome.css` — the app's face: the system font, the base size and the
  muted tone as custom properties (`--okay-base`, `--okay-fg`,
  `--okay-muted`) a product may set, the sidebar at a page's size.

### okay-ops — the law
- Scaladoc on `Red.route` and `Lifecycle.route`: the wrapped route is
  built when the answer runs, not when the request is matched; nothing
  set on the thread around matching reaches it; a value the answer
  needs is a capability captured by the handler (`Route.provided`).
  No behaviour changes.

### okay-desktop — the app's own window (new module, JVM, JavaFX provided)
- `App(name, icon, base, start, version, about, menus, picks, saves, busy, quit)`
  — what a product says about itself and nothing else: its name (the
  title, the user agent's suffix), its icon, where its service answers
  and the first page, its version line, the About box's words, its own
  menus and items, the links on its pages that open a file dialog and
  where the file is posted, which downloads are saved through a dialog,
  the path that says whether quitting should ask.
- `App.Menu`, `App.Item`, `App.Act` (`Go`, `External`, `Js`, `Save`,
  `Print`, `Post`, `Pick`, `Run`) — a menu as data; the window draws the
  product's menus between its own File items and Edit / View / Help.
- `App.Pick(link, title, filter, post, failed)` — a link on a page that
  the window answers with an Open dialog and a POST of the file.
- `Window.open(app, state)`, `Window.focus()`, `Window.systemAbout(app)`,
  `Window.external(url)` — the window until it closes; the second
  double-click; About in the system's application menu; the system
  browser.
- `WindowState(x, y, w, h)` in `<state>/window.txt`, with a floor.
- `Desktop.dataDir(name)`, `Desktop.windowed`, `Desktop.free(port)`,
  `Desktop.freePort(preferred)`, `Desktop.launch(app, data, serve, …)`
  — where the system keeps an application's data; JavaFX and a screen;
  the launch — one copy per data folder (`Instance`), the service in the
  window's process or on a free port (specs/app-in-process.md).
  small "running" window and the browser) once it answers.
- `App.script(app)` — the bridge the window installs on every page:
  clicks on `a[download]` and on the `saves` paths go to a Save
  dialog, a form sent `as=csv` too, a `pick` link to its dialog.

### Since app-in-process (2026-09-29)

The window reaches its service without a port (specs/app-in-process.md):
the `app://` scheme and the bridge in okay-desktop, `Transport` for
everything the window asks, one copy by a lock and a Unix-domain socket
in the data folder; `App.base` is filled in by the launch.

## Behavior
- [ ] `Route.provided`: two requests get two values; definedness is the
      inner route's; the value is the request's inside a wrapper that
      builds the route late (`okay.async(...).flatMap(_ => routes(r))`)
- [ ] `Router.provided`: the handler's capability is the request's,
      captured — read after the handler returned, on another thread
- [ ] `Chrome.html`: the notices above the page under both frames; the
      sidebar only when `app`; the strip only when not
- [ ] `Chrome.document`: `okay-refresh` when `app`, `http-equiv` when
      not, none at 0; the app's style and script only when `app`; every
      class `Chrome.css` writes is one the frame or the ways use
- [ ] `Red.route`/`Lifecycle.route` build late: a counter in the wrapped
      route's construction moves when the answer runs, not before
- [ ] `WindowState`: round trip; a window under the floor comes back as
      the default
- [ ] `Desktop.dataDir`: the three systems' places, from the name
- [ ] `App.script`: every pick's link and the `saves` pattern are in it;
      a product with none has a script that saves downloads only
- [ ] okay-desktop compiles against JavaFX (provided); nothing in it
      loads a JavaFX class on a headless test

## Decisions
- `Chrome`, the UI word for the frame around a page's content, and not
  `Frame`: okay-ui's `Frame` is the terminal's, and the operator, asked,
  kept the jargon (*«я не возражаю против жаргонного хрома, тем более что
  понятие фрейм перегружено смыслами»*).
- The site's strip is the product's: a site's navigation is content
  (who it sells to, sign out or not), the app's sidebar is the frame.
- `Router.provided` is the handler form and not a router rebuilt per
  request: a `Router` is a trie built once (router-trie); `Traced.route`
  may rebuild a PartialFunction per request because a PartialFunction
  is a closure, a trie is not.
- Red and Lifecycle keep building late: `Attempt` around the build is
  what counts a throwing route as `exception`, and a strict build would
  let it escape the meter. The law is stated instead, and the
  capability form makes it moot.
- okay-desktop is JVM-only and JavaFX is `Provided`: the installed
  app's Java carries the modules (jlink), a server never has them, and
  the jar stays one file for every system — okay-watch's rule, now the
  module's.
