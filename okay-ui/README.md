# okay-ui — the toolkit that is not a toolkit

The view is a VALUE, the loop is a fold over merged event sources, and
the renderer is a seam — so one application runs on a terminal, under
React, or on a test host, unchanged. Elm's architecture with the two
things Elm fakes made real: effects are the effect row, and `Cmd` is
not needed because subscriptions are `merge`.

## The pieces

| | |
|---|---|
| `Ui` | the tree — data only, keys instead of closures, so it diffs, compares and (later) crosses a wire |
| `Event` | what the user did, naming keys. UNTRUSTED by doctrine: the shown tree is the capability list |
| `Ui.run(init)(view)(update)(host, external)` | the loop: a pure update, the world merged in as sources |
| `Ui.diff` / `Patch` / `Host.diffing` | retained trees and narrow patches; a patch consumer becomes a Host |
| `Host` | the seam, React-shaped: hand over the whole tree — two functions, like `Link` |
| `Frame` / `Terminal` | the terminal's pure half (frames are `Vector[String]`, keys interpret against the tree, tested with no tty) and the thin impure edge |

## The shape of an application

```scala
Ui.run(init)(view)(update)(host, external)
```

Four arguments and no framework: `init` is a value, `view` is
`State => Ui`, `update` is `(State, Event) => State` in the effect row
you chose, and `external` is every other source of events merged in.
Swap `host` for `Terminal.host()`, a React host or a test host and the
other three do not change — which is how the same application is
driven by a test with no terminal at all.

## Further

| | |
|---|---|
| [`docs/modules/okay-ui.md`](../docs/modules/okay-ui.md) | the pieces and the reasoning |
| [`specs/ui.md`](../specs/ui.md) | the design and its decisions |
| [`okay-ui-gtk/`](../okay-ui-gtk), [`okay-compose/`](../okay-compose) | other hosts behind the same seam |
