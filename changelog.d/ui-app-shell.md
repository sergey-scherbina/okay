## ui-app-shell — an application's frame and ways for the HTML host (2026-09-24)

okay-watch's desktop app must look and behave like an app, and that
belongs in okay-ui rather than in a product's patches (specs/ui-app.md).

- `Shell(brand, groups, footer)` + `Shell.html(shell, here, body)`: a
  sidebar of grouped places with the reader's own marked, the page's
  HTML beside it in `main.okay-main` — plain and live pages alike; not a
  `Ui` node (a frame is the container's business, as `Html.form`'s
  mount is).
- `Enhance` — a typed `Js` program (no escape hatch) and its css: every
  `select` opens a list under its field at its width in the page's font
  (the `select` left in place for the live road's patches); a form in
  the frame is sent by `fetch` with a spinner beside its button and only
  the content swapped, scroll kept; `meta okay-refresh` fetches instead
  of reloading; a live-road press spins until the next patch.
- `Html.PlainClass` / `Html.LivePrefix`: the mounted form's class and id
  prefix, named once for the live client and `Enhance`.
- TestAppShell: the frame's marks and escaping, `/` current only at `/`,
  every written class has a rule, zero raws, `node --check` parses it.
