## app-host - one code, online and on the desktop: what the platform owes an application

okay-watch's operator, 2026-09-28, after the desktop app lost its
sidebar to a thread-local set around matching a request (okay-watch bug
app-frame-lost): «окей должен предоставить все необходимое … где один и
тот же код работает как в онлайн так и на десктопе … в ватче только
бизнес логика — а в окей все связанное с платформой». specs/app-host.md.

- okay-http: `Route.provided[E](env: Request => E)(route: E ?=> Route)`
  — a capability read from the request, ambient in a PartialFunction
  route, the shape `Traced.route` and `Secure.granted` each had for one
  value; `Router.provided(env)(h)` for one handler of a table, whose
  trie is built once — the value is computed inside the handler and
  captured, so it holds however late a wrapper builds the answer and
  on whichever thread. `TestProvided` (3): two requests, two values;
  the value inside a wrapper that builds late, run on another thread.
- okay-ui (`scala-form`): `Chrome(shell, app, notices)` — the frame
  chosen per request: `Chrome.html` frames the page by okay-ui's
  `Shell` with the notices above it in the app, by the product's own
  strip on the site; `Chrome.document` is the whole document (viewport,
  a refresh that fetches in the app and reloads on the site, the app's
  face `Chrome.css`, `Enhance` when the app). `TestChrome` (3).
- okay-ops: the route-wrapper law stated on `Red.route` and
  `Lifecycle.route` — the wrapped route is built when the answer runs,
  so nothing thread-scoped around matching reaches it; a test in
  `TestLifecycleRed` holds it. No behaviour changed.
- okay-desktop, a new JVM module (JavaFX provided, `jdkFloor(21)`): the
  app's own window over a product's local service — `App` is what the
  product says about itself (name, icon, base, the first page, its
  version line, About's words, its menus as data, the links a page
  answers with an Open dialog, which downloads go through a Save
  dialog, the page to print apart, the question before quitting);
  `Window` is the JavaFX window (outside links to the system browser,
  Save dialogs, print, About, the size and place remembered, a tour for
  screenshots); `Desktop` the launch (the data directory per system,
  one copy running, the window or the small "running" window and the
  browser). `TestDesktop` (4) on the pure parts; nothing in the tests
  loads a JavaFX class.
- What okay-watch keeps is its `App` value and its service.
