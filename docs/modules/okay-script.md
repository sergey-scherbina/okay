# okay-script

Markdown files as Scala source (specs/okay-script.md). A `.md` file
with fenced ` ```scala ` blocks is a literate program: the blocks are
extracted, concatenated in document order, and compiled by the REAL
Scala 3 compiler in-process (`dotty.tools.dotc`), then run. No new
language and no interpreter — markup metadata extraction, minimal
preprocessing, meta-compilation.

| | |
|---|---|
| `ScalaScript.run(markdown)` | the whole surface: compile the file's blocks together and run them; a `Result` carries diagnostics as DATA |
| `Segment` | `Text` / `Code(s, startLine)` / `Interp(expr, startLine)` — the tokenized document; the start lines are what make a compile error point at the ORIGINAL `.md` line, not the synthesized source |
| `Meta` | front-matter and document metadata (`Meta.parse`, `Meta.current`), auto-injected around a run |
| `Page` | render mode: a `.md` file compiled once and re-invoked per request, recompiled when the file changes (hot-reload) |
| `Site` | the container — "a new JSP": a directory of pages served over okay-http/okay-jetty (`Jetty.serve(port)(site.routes)()`); `/a/b` → `a/b.md`, `index.md`, `[param].md`, static files; sessions (`Sessions.memory`, or `Sessions.persisted` over okay-persist so a restart keeps them); error page |
| `okay.script.api` | what a page sees: `Web.current` (method, path, query, headers, form, cookies, params), `Response.current` (status, headers, redirect, cookies), `Session.current`, `include`/`forward`, `Error.current`. Shared with the host classloader, servlet-API style |
| `Live` / `mount` | okay-ui as the front-end layer: a page declares `Live(init)(view)(update)` and mounts it; the container serves the SSR, `/__okay/live.js` and the page's own WebSocket session (`Jetty.serve(port)(site.routes)(site.ws)`) |
| `secure:` front-matter | declarative page security, web.xml's constraint: a scope (or `any`) the caller must carry, checked by the deployment's `verify`; a login page (`login.md`) gets the redirect, an API client the 401/403 ladder; `Principal.current`, `login(token)`/`logout()` |
| `Forms` / `Live.form` | typed forms from a `Schema` (okay-ui's `Form`): `Forms.html[A]`/`Forms.read[A]` is the plain `<form method=post>` road, `Live.form[A](submit)` the live one -- either way the page gets an `A`, never a `Json` |
| `Application` / `signIn` | JSP's application scope: attributes shared by every page of a Site (`Application.current`, typed through a Schema's JSON, `persisted` over okay-persist); `signIn(subject, scopes)` mints through the Site's `issue`, the pair of its `verify` |
| `Site.serve(port)` / `okay.script.Serve` | the one line to run a Site over Jetty (routes, sockets, pushes), and the stock entry point: `sbt "okayScript/runMain okay.script.Serve pages 8080"`; `OKAY_DATA=<dir>` makes sessions and the application scope persistent, `OKAY_TLS_CERT`+`OKAY_TLS_KEY` serve HTTPS |
| `languages` / `Lang` / `t` | pages in several languages: `page.<lang>.md` variants chosen by `?lang=`, the OKAYLANG cookie or `Accept-Language`; `t(key, args*)` from `i18n/<lang>.yaml` with fallback to the first language |
| `cache:` front-matter / `Caching` | conditional requests: static files always carry an ETag and Last-Modified and answer 304; a page opts in with `cache: <seconds>` and goes `private` automatically when it is `secure:`, sets a cookie, or rides a session |
| `Site.warm` / `Site.stats` / `opsRoutes` | compile the whole directory at boot (a broken page is named then, not by the first visitor), counters and gauges, and opt-in `/healthz` `/stats` `/metrics` in JSON and Prometheus text |
| ` ```scala declare ` | an object-level block (JSP `<%! %>`): a `val` built once per compile, a `def` every request can call |
| `Classpath` / `Deps` | the ambient classpath a script compiles against (`Classpath.api` for a page importing the API), plus `using dep` coordinate resolution |

One file is one compilation unit: a later block sees what an earlier
block defined, the way one Scala source or a REPL session would. A
run is `ok` iff the source compiles with zero errors AND the program
runs without throwing — a smoke test, not an output checker
(mdoc-style output comparison is filed to BACKLOG, not built).

Library API only: no CLI, no sbt-test integration, no automatic walk
of `specs/*.md` in the default gate. Its tests FORK, because the
compiler reads the test JVM's own classpath (okay-script-scalac-
classpath). okay-jetty is a main dependency since `Site.serve`.
