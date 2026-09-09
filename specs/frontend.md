# Frontend: one application, any client, the protocol between them

## Overview

The operator's direction (2026-09-09), in its own terms: okay must
support the frontend IDEALLY. A frontend's logic is defined by the
structure, logic and design of the system it shows, not by the
technology that draws it. And the frontend should be DYNAMIC the way
the web is: the browser does not know in advance what it will show,
it draws what the server sends — so why not a native client the same
way, one that never changes while every piece of logic changes on the
server, over a protocol and a model that depend on neither side's
platform nor language.

The assessment: specs/ui.md already built most of the foundation, on
purpose. The view is a VALUE with keys instead of closures; the
renderer is a seam (`Host`/`Backend`); `Wire.serve` is a pure stage
that sends the full tree once and narrow patches after, and treats
the shown tree as the capability list; `Form` renders from the same
`Schema` that decodes; okay-script's Live pages already run this over
a WebSocket with reconnect and durable sessions. A dynamic native
client is `Wire.client` with a native `Host` — nothing in the design
prevents it. What is missing is what this spec adds:

1. a VOCABULARY rich enough to draw a real application, at two
   levels, both data;
2. the PROTOCOL as a first-class artifact: derived codecs (JSON and
   CBOR from one `Schema`), a handshake, a written contract a client
   in Swift or Kotlin can implement without Scala;
3. the HYBRID rule: which events stay on the client, which cross,
   stated in the tree as data — no round trip per keystroke;
4. native thin clients over that contract, and every Scala target
   (JVM, JS, Native) as a host out of the box.

The operator's three answers fix the shape: thin clients in the
platform's own language count, and Scala hosts still work with no
extra step; two vocabulary levels, both first-class; hybrid from the
start.

## Two levels, one tree

The browser's own design: `<select>` is semantic and drawn natively,
a custom element is a tree of `div`s, both live in one document.

- **Level L, layout — small and CLOSED.** The minimum a conforming
  client must draw: `Box` (direction, weights, gap, padding), `Text`
  (style TOKENS, not pixels), `Image`, `Input` (kinds: text, secret,
  multiline, number), `Button`, `Check`, `Select`, `Scroll`. A thin
  client that draws these draws everything.
- **Level S, semantic — OPEN.** `Form`, `List`, `Table`, `Tabs`,
  `Modal`, `Menu`, `Progress`, and what applications need next. A
  semantic node is DEFINED by its lowering: `lower: S => L`, a pure
  function in the core. The lowering is not a fallback bolted on; it
  is what the node means.
- **The client declares what it draws.** Its first line is
  `hello {vocab: [...], version}`. The server lowers every node the
  client did not claim BEFORE sending, so the client always receives
  a tree it understands whole, and no lowering logic lives on any
  client. A rich client claims `Table` and draws a native table; a
  thin one gets `Box` of `Box`.
- **The law that keeps the two levels one.** The keys of a semantic
  node equal the keys of its lowering; therefore the capability list
  is the same, the events are the same, and `update` cannot tell
  whether the client drew `Table` natively or through its lowering.
  Diff commutes with lowering: `diff(lower(a), lower(b))` applied to
  `lower(a)` yields `lower(b)`.

Where a programmer sees both: write semantics; drop to `Box` where a
non-standard look is wanted. Style is TOKENS (`emphasis`, `muted`,
`danger`, sizes small/normal/large): a host maps tokens to its own
idiom, so a native client looks native and a terminal stays a
terminal. Pixels never cross the wire.

## The hybrid wire

The rule is data in the tree, not code on the client:

- **Input is local by default.** An `Input` folds its edits into the
  client's own store, keyed by the input's key. The server learns the
  value when the enclosing `Form` submits: ONE event,
  `Submitted(formKey, Json)`, decoded by the same `Schema` that drew
  the form (ui-toolkit's drift law, now the wire's rule). An `Input`
  outside a `Form`, or one marked `live`, sends `Edited` as today —
  search-as-you-type is a choice the tree makes visible.
- **A closed set of local behaviours.** `Local.Toggle(key)` shows or
  hides a keyed subtree, `Local.Tab(key, n)` selects a tab, focus and
  scroll are the host's own. Enumerable, small, all data. What is not
  in the set crosses the wire; the boundary is visible in the type.
- **The server stays the truth.** Local state is a cache: a
  `SetValue` from the server overrides a local edit; a reconnect
  starts from the full tree (session continuity is ui-durable's,
  unchanged).
- **The capability list survives.** `Submitted` names a `Form` key
  that must be on the shown tree; its Json is decoded by the form's
  schema (total: damage is data). A forged key is dropped before
  `update`, as every event is today.

## The protocol

Transport-agnostic, as `Wire` already is: lines in, lines out, over
channels, a Link, a WebSocket or stdio.

```
client → server   hello {vocab, version}          first, once
server → client   tree {ui}                        full tree, then
server → client   patch {…}                        narrow patches
client → server   event {…}                        Pressed, Submitted, …
either            close
```

- Codecs are DERIVED: `Schema[Ui]`, `Schema[Event]`, `Schema[Patch]`
  from the enums, so JSON and CBOR both arrive from one definition
  and version drift follows the codec-evolution rules Schema already
  has (an optional field decodes absent; an unknown node is an error
  the hello made impossible). The hand-mapped `WireJson` becomes the
  dialect's test oracle and then retires. Prerequisite: the Schema
  derivation gaps that kept `WireJson` hand-written (codec-vector:
  Vector, recursion, defaults).
- The contract is a DOCUMENT (docs/protocol/frontend.md) plus a
  conformance script: a golden sequence of lines and the frames any
  host must produce for it. TestPortable is the pattern; a Swift
  client passes the same script.

## Deployment is a flag

The same `Ui.run(init)(view)(update)` runs three ways with no change
to the application: in-process on Scala.js or Scala Native with a
local host; behind `Wire.serve` with any client; or split — screens
that are local run in the client, screens that are the server's run
on the wire, joined by `Nav`. This is what "everything Scala can do
works out of the box" means here: the Scala hosts (terminal, React,
DOM; Swing and GTK to come) are hosts like any thin client, and a
Scala client is the FIRST conforming client of the protocol.

## Interface

```scala
enum Ui:                                     // level L
  case Box(children: Vector[Ui], dir: Dir, weights: Vector[Int] = Vector.empty,
           gap: Int = 0, pad: Int = 0, key: String = "")
  case Text(s: String, style: Style = Style.none)
  case Image(src: String, alt: String)
  case Input(value: String, key: String, label: String = "",
             kind: InputKind = InputKind.Text, live: Boolean = false)
  case Button(label: String, key: String, role: Role = Role.Plain)
  case Check(on: Boolean, key: String, label: String = "")
  case Select(options: Vector[String], selected: Int, key: String)
  case Scroll(child: Ui, key: String = "")
  // level S — each with a lowering
  case Form(fields: Vector[Ui], submit: String, key: String)
  case List(items: Vector[Ui], key: String)
  case Table(header: Vector[String], rows: Vector[Vector[Ui]], key: String)
  case Tabs(labels: Vector[String], selected: Int, pages: Vector[Ui], key: String)
  case Modal(title: String, body: Ui, key: String)

object Ui:
  def lower(ui: Ui, vocab: Set[String]): Ui   // every node not in vocab, lowered
  def keys(ui: Ui): Set[String]              // the capability list

enum Event:
  ... as today ...
  case Submitted(key: String, value: Json)   // a Form's one event

object Wire:
  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S)
    : Stage[String, String, S]               // reads hello first, lowers per vocab
  def client(host: Host, vocab: Set[String])(lines, send): Unit ! Async
```

Row/Column stay as `Box` in two directions (source-compatible
aliases), so every existing application and test compiles unchanged.

## Behavior

Stage 0 — the vocabulary (ui-vocab, LANDED 2026-09-09):
- [x] `Box` with weights: the terminal divides the row's natural
      width by weight, the DOM maps to flex-grow (and the box carries
      `data-w`, so a patch consumer replacing one child gives it its
      flex without holding the tree — TestDom's law found a Replace
      losing it); Row/Column stay as they were and every existing test
      passes unchanged
- [x] style tokens: a token renders differently on each host and the
      SAME on the test host; no host receives a pixel
- [x] the diff law holds for every new node: diff-then-patch equals
      the next tree (TestVocab's battery: every semantic node in one
      screen, edits, a tab switch, keyed items appearing and
      vanishing; TestDom's battery extended at the DOM)
- [x] `lower` is total over level S and its result is level L only;
      level L is a fixed point; a claimed node is sent as itself
- [x] `keys(s) == keys(lower(s))` for every semantic node, under any
      vocabulary — and only the SELECTED tab's page is a capability
- [x] diff commutes with lowering: `lower(patch(a, diff(a, b))) ==
      lower(b)` under every vocabulary; a form edit is the same narrow
      SetValue at the same path on either tree
- [x] `Wire.serve(init, vocab)` lowers before the first line: a
      level-L client (the Live pages' `live.js`, extended for level L)
      never receives a semantic node; hello arrives in stage 1

Stage 1 — the protocol (ui-protocol, LANDED 2026-09-09):
- [x] `Schema[Ui]`, `Schema[Event]`, `Schema[Patch]` and the envelope
      `Schema[Protocol.Msg]` derived (the enumerations spell
      themselves short: `"h"`/`"v"`, `"secret"`, `"danger"`); every
      shape round-trips through JSON lines and CBOR bytes from the one
      definition. `WireJson` RETIRED — not "equal on the battery", the
      derived shape replaced it (see Decisions), and every consumer
      (Sessions, okay-script's Site and `live.js`, the tests) speaks
      `Protocol` now
- [x] hello: a client claiming nothing receives a tree with no
      semantic node; one claiming `form` receives `Form`; unknown
      names are ignored; an event before any hello is served as
      level L, and that first line is still handled
- [x] the conformance script (`docs/protocol/conformance.jsonl`): the
      client's outs and the server's ins with the tree the client must
      hold after each; `Wire.client` over a recording host reproduces
      every tree, and says every out, hello first
- [x] `docs/protocol/frontend.md` is rendered from the derived schemas
      by `Protocol.document` (a shape language over the Schema
      algebra, recursion by name — `JsonSchema.of` overflows on the
      recursive `Ui`); `TestProtocol` fails when either file drifts,
      `OKAY_RENDER=1` regenerates

Stage 2 — the hybrid (ui-hybrid):
- [ ] typing into a `Form`'s inputs crosses the wire ZERO times;
      pressing submit crosses once with `Submitted(key, json)`, which
      decodes by the form's schema on the server
- [ ] an `Input` marked `live` sends `Edited` per change, as today
- [ ] `Local.Toggle` and `Local.Tab` change what is shown without a
      line on the wire; the server's next patch still applies
- [ ] a `SetValue` from the server overrides a local edit
- [ ] a forged `Submitted` (a key not shown, or a value the schema
      rejects) never reaches `update`

Stage 3 — the first thin client (ui-native):
- [ ] a Compose Multiplatform client (Kotlin, no okay dependency)
      draws level L, claims nothing else, passes the conformance
      script
- [ ] the same server, unchanged, drives the browser and the native
      client at once
- [ ] Scala Native + GTK (or Swing on the JVM) as a host over the same
      seam — the "out of the box" leg

## Results

Stage 1 (ui-protocol) landed 2026-09-09. `okay.ui.Protocol`: the
derived schemas (enumerations as short names), `Msg` (Hello, Tree,
Patch, Event, Close), JSON lines and CBOR bytes, `document`,
`describe` (the shape language), `conformance`. `Wire.serve` reads
the hello first and lowers per its vocabulary; `Wire.client(host,
vocab)` says hello first. `live.js` rewritten to the derived shapes
(~160 lines, still dependency-free). `docs/protocol/frontend.md` and
`docs/protocol/conformance.jsonl` are the contract a Compose client
implements in stage 3. TestProtocol (5); okay-ui 74, okay-script 166,
okay-demo 54, the Live-tagged wire/session/jetty suites, JS and
Native legs green.

Stage 0 (ui-vocab) landed 2026-09-09. Level L: `Box(dir, weights,
gap, pad)`, `Image`, `Scroll`, `Input` kinds (text, secret,
multiline, number) and `live`, `Button` roles, `Style` tones and
sizes. Level S: `Form`, `Items`, `Table`, `Tabs`, `Modal`, each with
its lowering; `Ui.lower(ui, vocab)`, `Ui.keys`, `Ui.Vocab`. Every host
draws the new level L (terminal: weights, gap, pad, tones, secret
masking; React/DOM: flex, classes, input types, textarea, img; the
Live pages' `live.js`: the same) and lowers level S at its entry
(`Ui.diffing` lowers for a Backend, `React.elem` for the React host,
`Frame.render` for the terminal). `Form` renders numeric fields as
`InputKind.Number`. 7 tests in TestVocab, the DOM battery extended;
the 69 existing okay-ui tests, okay-script's 166 and okay-demo's 54
pass unchanged.

## Out of scope
- Animation and gestures beyond press/edit/scroll.
- A client-side scripting language: the Local set is closed on
  purpose; anything richer is a round trip or a Scala client.
- Pixel-level styling; themes are token maps on the host.
- Offline-first with local writes (a journal on the client is
  ui-durable's shape and its own spec).

## Decisions
- **Two levels, lowering on the SERVER** — the client never lowers,
  so a thin client stays thin and the semantic set can grow without
  touching any shipped client. The cost is a handshake; the win is
  that `hello` is the whole compatibility story.
- **Semantic nodes are defined by their lowering** — a node without
  one is not admitted, so "what does this mean on a terminal" is
  never an open question.
- **Hybrid from the start, as data** — the operator's call; a
  per-keystroke round trip is what the web avoids and so do we.
  `Submitted` carries Json decoded by Schema rather than a typed
  value because the wire is untyped by nature and the decode boundary
  already exists.
- **Tokens, not pixels** — a native client must look native; the
  application says "danger", the host says red.
- **Codecs derived, WireJson retired** — one definition, two
  encodings, versioning by the rules Schema already has.
- **The derived shape IS the wire; WireJson retired** — the spec
  first planned "derived JSON equals WireJson's, then retire", which
  is impossible (a derived sum is `{"Case": {...}}`, the dialect was
  `{"t": "text"}`), and keeping two shapes would have made the
  document a lie about one of them. The derived shape is a little
  longer on the wire and much better for a client in another
  language: every field present, by name, and the document names
  exactly what the codec writes.
- **A shape language, not JSON Schema, in the document** — the
  codec's `JsonSchema.of` has no `$ref` and overflows on a recursive
  type; the wire's document needs recursion by name and nothing else
  JSON Schema offers. `Protocol.describe` renders the algebra in one
  screen a Kotlin developer can read whole.
- **Hello first, but not a gate** — a client that sends an event
  before any hello is served as level L; a second hello is ignored.
  The Live pages' journal (ui-durable) keeps hello lines verbatim and
  the refold reads them like any other.
- **Row/Column stay; Box is the general node** — the spec first said
  "Row/Column are Box"; an enum case cannot be an alias with its own
  extractor, and rewriting every `case Row(c, k)` in the repository
  for no semantic gain is not surgical. Level L is nine nodes, Row
  and Column being Box without weights, gap or pad.
- **`Items`, not `List`** — a case named `List` inside `Ui` shadows
  Scala's in every `import Ui.*`, including the patch paths.
- **Semantic nodes lower at the HOST's entry in-process** — a Backend
  is a level-L consumer by definition, so `Ui.diffing` lowers before
  the diff; the React host and the terminal lower in their renderers.
  Nothing between the application and the host needs to know.
- **First native target: Compose** — the operator's call
  (2026-09-09). Kotlin, Compose Multiplatform, so one thin client
  covers Android and desktop; the Kotlin side reads CBOR or JSON by
  the protocol document alone and depends on nothing of okay. SwiftUI
  follows over the same conformance script.

## Results
(none yet — the spec is the deliverable of claim frontend-spec)
