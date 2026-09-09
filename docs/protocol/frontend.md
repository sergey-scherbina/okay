# The frontend protocol, version 1

This file is RENDERED by `okay.ui.Protocol.document` from the derived
schemas of the tree, the events, the patches and the message envelope
(okay-ui's `TestProtocol` fails when it drifts; regenerate with
`OKAY_RENDER=1 sbt okayUiJVM/testOnly okay.ui.TestProtocol`). It is
the contract a thin client implements — in Kotlin, Swift or anything
else — with no dependency on okay: read `docs/protocol/conformance.jsonl`
and reproduce it (specs/frontend.md).

## The conversation

Lines of JSON (one message per line) or CBOR items, the same shapes:

```
client → server   Hello {vocab, version}   first, once
server → client   Tree {ui}                the full tree, then
server → client   Patch {patch}            narrow patches
client → server   Event {event}            what the user did
either            Close
```

- A sum is an object with ONE key, the case name: `{"Text": {...}}`.
  A product is its fields by name, every field present. An
  enumeration is a string from its list (`"h"`/`"v"` for a direction).
  `[T]` is a JSON array of T, `T?` a field that may be `null` or
  absent, `int` a JSON number without a fraction.
- `Hello.vocab` lists the SEMANTIC nodes the client draws itself
  (`items`, `table`, `tabs`, `modal`, `disclosure`). Every other node
  is LOWERED by the server to the layout level before it is sent, so
  a client that claims nothing receives only: `Text`, `Row`, `Column`,
  `Box`, `Image`, `Button`, `Input`, `Check`, `Select`, `Scroll`,
  `Form`.
- THE HYBRID RULE. A `Form` is fields plus a submit button whose key
  is the form's key. The client keeps the fields' values itself and
  sends nothing while the user types; when the button is pressed it
  sends ONE `Submitted {key, edits}` — an `Edited` per input, a
  `Toggled` per check, a `Chosen` per select, all of them — instead
  of `Pressed`. An `Input` with `live: true` also sends `Edited` as
  it changes (search-as-you-type). Inputs outside any `Form` send
  `Edited`/`Toggled`/`Chosen` as they happen. A client that claims
  `tabs` or `disclosure` switches them itself and sends nothing; a
  client that does not receives their lowering (buttons) and sends
  `Pressed`. The server stays the truth: a `SetValue` it sends lands
  on the client's field and wins.
- A server that receives an `Event` before any `Hello` serves the
  client as if it had claimed nothing.
- Patch paths index children in order: `Row`/`Column`/`Box` children,
  `Scroll`'s child at 0, `Form` fields, `Items` items, `Modal`'s body
  at 1. `Reorder` gives the survivors' old indices in new order;
  removals come first (descending), then one reorder, then insertions
  (ascending), then content patches.
- Events name KEYS; the server drops an event whose key is not on the
  tree it last sent — the shown tree is the capability list.
- A `Box` with `weights` divides its main axis by weight; `gap` and
  `pad` are in character units; style is TOKENS a host maps to its
  own idiom.

## The shapes

Rendered from the derived schemas; a name on the right is defined
below, a sum lists its cases with `|`.

```
Msg = Hello {vocab: [string], version: int}
  | Tree {ui: Ui}
  | Patch {patch: Patch}
  | Event {event: Event}
  | Close {}

Ui = Text {s: string, style: Style}
  | Row {children: [Ui], key: string}
  | Column {children: [Ui], key: string}
  | Box {children: [Ui], dir: "h" | "v", weights: [int], gap: int, pad: int, key: string}
  | Image {src: string, alt: string}
  | Button {label: string, key: string, role: "plain" | "primary" | "danger" | "active"}
  | Input {value: string, key: string, label: string, kind: "text" | "secret" | "multiline" | "number", live: bool}
  | Check {on: bool, key: string, label: string}
  | Select {options: [string], selected: int, key: string}
  | Scroll {child: Ui, key: string}
  | Form {fields: [Ui], submit: string, key: string}
  | Items {items: [Ui], key: string}
  | Table {header: [string], rows: [[Ui]], key: string}
  | Tabs {labels: [string], selected: int, pages: [Ui], key: string}
  | Modal {title: string, body: Ui, key: string}
  | Disclosure {title: string, open: bool, body: Ui, key: string}

Style = {bold: bool, dim: bool, tone: "plain" | "emphasis" | "muted" | "danger", size: "small" | "normal" | "large"}

Patch = Replace {path: [int], ui: Ui}
  | SetText {path: [int], s: string}
  | SetValue {path: [int], s: string}
  | SetChecked {path: [int], on: bool}
  | SetSelected {path: [int], index: int}
  | Remove {path: [int], index: int}
  | Reorder {path: [int], order: [int]}
  | Insert {path: [int], index: int, ui: Ui}

Event = Pressed {key: string}
  | Edited {key: string, value: string}
  | Toggled {key: string, on: bool}
  | Chosen {key: string, index: int}
  | Key {ch: string}
  | Resized {w: int, h: int}
  | Closed {}
  | Submitted {key: string, edits: [Event]}
```
