# ui-app — an application's frame and ways, for the HTML host

## Overview
okay-watch's operator, 2026-09-24, of its desktop app (the HTML host
inside the app's own window): it must look and behave like an app —
«чтобы она была действительно десктопная — смотрелась как десктопная и
вела себя как десктопная». The first cut was the product's own patches:
a sidebar in its strings, a script its window injected. Asked whether
that was okay-ui (*«через использование okay-ui?»*), the answer was
no, and the decision was to move it here, where every product and both
roads — the plain one and the live one — get it.

What an app has that a page does not, and what this spec adds to the
HTML host, nothing else touched:

- a FRAME: its name, a sidebar of grouped places, where the reader is,
  the page beside it;
- a DROPDOWN that opens under its field, at its width, in the page's
  font — a web engine's own list is a small grey menu (JavaFX's WebKit
  draws it at 11px, 120px wide, measured);
- a PRESS that does not reload: sent in the background, a spinner
  beside its button, only the content replaced, the scroll where it was;
- a page that keeps itself FRESH by fetching, not by a meta refresh
  that repaints and jumps.

## Interface
- `Shell(brand, groups, footer)` with `Shell.Item(label, href, icon)`
  and `Shell.Group(title, items)`; `Shell.html(shell, here, body)`
  frames a page's body (plain HTML or `Html.render`/live output
  alike); `Shell.css`. The item whose `href` is `here`, or a prefix of
  it at a `/`, is marked current; `"/"` is current only at `"/"`.
- `Enhance.script` (a `Js` program, printed) and `Enhance.css`:
  - every `select` opens OUR list — the `select` itself is untouched
    (a live page may patch it), the choice sets its value and fires
    `input` and `change` as the engine's would; Esc or a click outside
    closes it;
  - a form inside `main.okay-main` that is not `[data-hard]` and not
    a mount the live client drives (`form.okay-plain` while `okayLive`
    is on the page — a PLAIN mount, `Html.form` alone, is sent like any
    form: ui-app-plain-press) is sent by `fetch`; its button gets `okay-busy`
    and a spinner (at least 450 ms, so it is seen); the answer's
    `main.okay-main` replaces this one's (and its sidebar, which may
    say something new); the scroll is kept when the page is the same
    one, reset when it moved; an answer with no frame replaces the
    document;
  - `<meta name="okay-refresh" content="N">` anywhere in the page
    fetches the page every N seconds and swaps it the same way;
  - on the live road a pressed button gets the spinner until the
    mount's next patch (or 10 s).

## Behavior
- [x] the frame marks the current place, groups with and without a
      title, escapes every label, and carries the body verbatim
- [x] `"/"` is current only at `"/"`; a prefix is current at `/x/…`
- [x] every class the frame and the script write has a rule in their
      css (the guard TestHtmlCss keeps for the tree)
- [x] the script is a program a JavaScript engine parses
- [x] the script's hooks are the ones the frame writes
      (`main.okay-main`, `data-hard`, `okay-refresh`, `okay-busy`)

## Decisions
- NOT a `Ui` node. A frame around a page is a container's business, as
  `Html.form`'s mount is; a new `Ui` case would have to be drawn by the
  terminal, Swing and Telegram hosts, none of which has a page to
  frame.
- The picker leaves the `select` in place rather than replacing it: a
  live page patches its tree by path, and an element swapped for a
  widget is a path that no longer exists.
