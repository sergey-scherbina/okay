# okay-desktop

The app's own window ([specs/app-host.md](../../specs/app-host.md)): an
installed product's pages inside the system's web engine, with its own
menus, a Save dialog for every download, the system browser for every
outside link, and its size and place remembered — the same for every
product. What is the PRODUCT's is one value it hands over, `App`.

| | |
|---|---|
| `App` | what a product says about itself and nothing else: its name (the title, the user agent's suffix), its icon, where its service answers on this computer and the first page, its version line, the About box's words, its menus, the links that open a file dialog, which downloads go through a Save dialog, and whether quitting should ask |
| `App.Menu` / `App.Item` / `App.Act` | a menu as data (`Go`, `External`, `Js`, `Save`, `Print`, `Post`, `Pick`, `Run`); the window draws the product's menus between its own File items and Edit / View / Help |
| `App.Pick` | a link on a page the window answers with an Open dialog and a POST of the file |
| `App.script` | the bridge installed on every page: `a[download]` and the `saves` paths go to a Save dialog, a form sent `as=csv` too, a `pick` link to its dialog |
| `Window` | `open`, `focus` (the second double-click), `systemAbout` (About in the system's application menu), `external` (the system browser) |
| `WindowState` | the window's size and place in `<state>/window.txt`, with a floor |
| `Desktop` | `dataDir` (where the system keeps an application's data), `running` (one copy), `windowed` (JavaFX and a screen), `front`, and `launch` — the service on its own thread, then the window, or without one the small "running" window and the browser |

**Depends on:** nothing of okay's — the service it shows is the
product's, reached over HTTP on this computer. JavaFX is `Provided`:
the installed app's Java carries it, a server never loads `Window`,
and that is why the window is asked for by name (`Desktop.windowed`)
and nothing else in the module mentions JavaFX. JVM only, JDK 21 for
the launch's platform and virtual threads.

okay-watch's own window and launch reduce to a description in terms of
this module; any product that ships as a desktop app does the same.

## Further

| | |
|---|---|
| [`specs/app-host.md`](../../specs/app-host.md) | one code online and on the desktop: the route-wrapper law, the chrome chosen per request, and this module |
| [`okay-ui`](okay-ui.md) | the pages the window shows, and `Chrome`, the frame chosen per request |
