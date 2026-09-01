# okay-ui — the toolkit that is not a toolkit

## Overview
A UI is four things this library already owns and one it does not.
Events arrive when they arrive — a `Source`. Several inputs (the user,
a timer, a network answer) join by readiness — `merge`. State evolves
by folding events — `Stage.transduce`. Drawing on a concrete toolkit
is an interpretation — a handler's job, the MCP thesis verbatim: the
program must not know who executes. The one missing thing is the
VIEW: a pure value describing what is on screen. This module is that
value, the loop around it, and the seam to anything that can draw.

The architecture is Elm's (model/view/update) with two honest
differences: effects in `update` are the effect row rather than a
`Cmd` encoding, and the renderer is swappable without touching the
program — one application, drawn by a terminal, by the DOM, by React,
by Swing, by a native toolkit; the first three are in scope now, and
all three cost zero dependencies.

## Interface

```scala
/** the view: a VALUE — no functions inside, so it has equality,
 * diffs, and could cross a wire. Widgets carry KEYS; events name
 * keys; the app's update interprets. */
enum Ui:
  case Text(s: String, style: Style = Style.none)
  case Row(children: Vector[Ui], key: String = "")
  case Column(children: Vector[Ui], key: String = "")
  case Button(label: String, key: String)
  case Input(value: String, key: String, label: String = "")
  case Check(on: Boolean, key: String, label: String = "")
  case Select(options: Vector[String], selected: Int, key: String)

enum Event:
  case Pressed(key: String)
  case Edited(key: String, value: String)
  case Toggled(key: String, on: Boolean)
  case Chosen(key: String, index: Int)
  case Key(ch: Char)                  // raw, for whole-app bindings
  case Resized(w: Int, h: Int)
  case Closed

/**
 * The seam, in the form React taught everyone: the HOST is handed the
 * whole tree and draws it however it likes — React reconciles, a
 * terminal repaints, Swing rebuilds. Two functions, like Link.
 */
trait Host:
  def render(ui: Ui): Unit ! Async
  def events: Source[Event]

/** a patch-consuming target (raw DOM, native toolkits): the core
 * diff turns it into a Host — diffing is OUR job, not every
 * backend's */
trait Backend:
  def apply(p: Patch): Unit ! Async
  def events: Source[Event]
object Host:
  def diffing(b: Backend): Host

enum Patch:
  case Replace(path: List[Int], ui: Ui)
  case SetText(path: List[Int], s: String)
  case SetValue(path: List[Int], s: String)
  ...
def diff(old: Ui, next: Ui): Vector[Patch]

/** the loop: pure update, external world as MERGED event sources —
 * subscriptions are `merge`, not a Cmd type */
object Ui:
  def run[S](init: S)(view: S => Ui)(update: (S, Event) => S)
            (host: Host, external: Source[Event] = Source())
            (using Scheduler, CanBlock): S ! Async   // answers the final state (Closed)

/** the fifth algebra over Schema: a FORM — rendered from the same
 * Schema that decodes it, so a form cannot drift from its parser */
object Form:
  def ui[A](using Schema[A]): Json => Ui             // render the partial value
  def edit(value: Json, e: Event): Json              // fold one event in
  def decode[A](using Schema[A]): Json => Either[String, A]
```

Backends shipped, all dependency-free:
- **terminal** (`src/main/scala-jvm-native`): ANSI painting, raw-mode
  input by `stty` (POSIX), a `Host` that repaints the frame — a frame
  is a Vector[String], so rendering is testable as a value
- **React-likes** (`src/main/scala-js`): `Ui => element` over the
  global `React.createElement` (works for anything with that shape —
  Preact included); the mapping itself is PURE
  (`Ui => Elem`, a data tree) and tested on the JVM, the js glue is
  five lines
- **the test host** (test scope): applies renders to a value, feeds
  scripted events — the whole loop is exercised with no screen,
  which is the point of the seam

## Behavior
- [x] the loop: scripted events through update, each state viewed,
      the test host sees the frames in order, `Closed` answers the
      final state
- [x] external sources merge in: a "timer" source and the user's
      events interleave by readiness into ONE update fold
- [x] diff: changing one Text yields SetText at its path, not a
      Replace of the root; unequal shapes Replace at the highest
      differing node; equal trees yield NO patches
- [x] keyed children (all keyed, keys distinct in both trees): a
      SHUFFLE is one Reorder plus narrow content patches — no child
      is Replaced for having moved; vanished keys Remove, new keys
      Insert; mixed or unkeyed children fall back to positional
- [x] the law survives the extension: diff-then-patch equals the next
      tree across shuffles, removals, insertions and edits combined
      (200 seeded rounds, plus nested keyed lists)
- [x] `Host.diffing(backend)` applied to the frames equals rendering
      each frame whole (the patch path and the repaint path agree)
- [x] the terminal host renders a frame as lines: Row lays out
      side by side, Column stacks, Input shows its value and focus;
      asserted on the STRING frame, no tty in the test
- [x] the React mapping is pure: a Ui tree becomes the element tree
      (type/props/children) a createElement host expects, keys
      carried; asserted on the JVM
- [x] a Form renders from `Schema[A]`: a product becomes labeled
      Inputs (an Option field unrequired, a Boolean a Check), edits
      fold into the partial Json, and `decode` answers the SAME `A`
      the schema parses — round-trip asserted
- [x] elicitation closes: okay-mcp's `Peer` gains `elicit`, a server's
      `elicitation/create` renders a Form on a scripted host, the
      accepted value decodes against the requested schema and goes
      back; decline goes back too
- [x] one application runs UNCHANGED on the terminal host and the
      test host (same frames modulo painting) — the seam's claim

## Design
The tree carries NO functions — Elm puts `msg` values in attributes,
React puts closures, this puts KEYS and lets events name them. That
is what makes `Ui` a plain value: equality for the diff, pure
rendering, JVM-testable React mapping, and (later) a tree that can
cross a wire — an MCP server could describe a form; elicitation
already does, as a Schema.

The loop is `Stage.transduce` in a coat: state is the transducer's
parameter, the input is `host.events merge external`, the output is
frames. Async work does not need a Cmd type: spawn a fiber, feed a
source, merge it — subscriptions are what `merge` is for.

Layout in v1 is the minimum the terminal needs: Row divides width by
natural size, Column stacks, no flex weights. The DOM maps Row/Column
to flexbox and forgets the problem.

## The architecture above v1 (designed now, built in phases)

### Scenarios are programs (phase 2)
The Elm fold is the LOW level. A wizard, a dialog, an elicitation is
imperative by nature — show, await, validate, branch — and that shape
is an EFFECT this library already knows how to run:

```scala
enum Dialog[+A]:
  case Show(ui: Ui) extends Dialog[Event]     // show, await one event

def wizard: Contact ! (Dialog + Throws % String) =
  for
    name <- Form.ask[Name]("who are you?")    // built on Show
    _    <- if name.valid then pure(()) else abort("try again")
    addr <- Form.ask[Address]("where?")
  yield Contact(name, addr)
```

A scenario RUNS INSIDE the loop: its handler holds the continuation
as the update function until the awaited event arrives — delimited
continuations doing what they are for. Both styles coexist because
both are the same machine: a screen is a fold, a flow is a program,
`Dialog` is the bridge. MCP elicitation is a one-step scenario.

Phase 2 interface and behavior (claimed: ui-scenarios):

```scala
enum Dialog[+A]:
  case Show(ui: Ui) extends Dialog[Event]

object Dialog:
  def show(ui: Ui): Event ! Dialog                  // show, await one event
  def run[A](host: Host)(prog: A ! Dialog): A ! Async   // standalone
  def screen[A](prog: A ! Dialog): Screen[A]        // the scenario AS a screen
object Form:
  def ask[A](message: String)(using Schema[A]): Option[A] ! Dialog  // a typed form flow
  def askSchema(message: String, schema: Json): Option[Json] ! Dialog
```

- [x] a wizard is a PROGRAM: show, await, validate, branch — and
      retry is recursion, not a combinator (an invalid step loops
      itself with an error line shown)
- [x] `Dialog.run` drives a scenario over any Host: renders on Show,
      resumes with the next event; the host's end answers what the
      scenario has (a scenario is not entitled to an ending it did
      not reach — the run answers Option)
- [x] the SAME scenario runs INSIDE `Ui.run` via `Dialog.screen`: the
      continuation is literally the screen's state, one event steps
      it, `done` shows the answer
- [x] `Form.ask[A]` answers Some(A) on ok (decoded by the same
      Schema), None on cancel; an undecodable value does not submit —
      the error is SHOWN and the flow continues
- [x] MCP elicitation is a one-step scenario: the demo's form-elicit
      collapses to `Dialog.run(host)(Form.askSchema(...))`

### Screens and navigation (phase 2)
A screen is a `(view, update)` pair over its own S; navigation is a
STACK of screens — push/pop is Mark/Restore over values, exactly the
backtrackable-context shape okay-agent already has. Routing is an
Event; the address bar (DOM) is one more event source merged in.

Contract (claimed: ui-screens). A screen is CODATA — what it shows
and what it becomes — which settles the heterogeneous-state problem
without an existential: each screen closes over its own state, and a
parent passes a CONTINUATION into a child it wants an answer from.
Closures are fine HERE and forbidden in the Ui tree, and the line
between the two is exactly the wire: screens never cross it, trees
do.

```scala
trait Screen:
  def view: Ui
  def step(e: Event): Nav

enum Nav:
  case Stay(s: Screen)     case Push(next: Screen)
  case Pop                 case To(s: Screen)

object Nav:
  def run(root: Screen): (List[Screen], (List[Screen], Event) => List[Screen], List[Screen] => Ui)
    // packaged for Ui.run: state is the stack, events route to the top
  def scenario[A](prog: A ! Dialog)(done: A => Nav): Screen
    // a Dialog program as a pushable screen — phases 2a and 2b meet
```

- [x] events route to the TOP screen only; Stay steps it, Push covers
      it, Pop reveals what was under, To replaces
- [x] a popped stack ends the app (the empty stack is Closed)
- [x] a parent gets a child's ANSWER through the continuation it
      passed — no shared mutable state, no message bus
- [x] a Dialog scenario is a pushable screen: the wizard from
      TestDialog runs as a child screen and its answer lands in the
      parent
- [x] the stack survives the loop: Ui.run over Nav.run renders the
      top screen's view at every step

### The wire: client-server bindings (phase 3)
The tree carries no functions — by decision, and this is where it
pays twice. `Ui`, `Event` and `Patch` get derived `Schema`s, so:

- **server-driven UI** (LiveView-shaped): update and view run on the
  SERVER; events go up the wire, patches come down. The client is a
  dumb Backend — terminal, DOM, React, none know the difference.
  Transport: okay-http WebSocket or the MCP duplex session (a page is
  then something a server SERVES, like tools and resources).
- **hybrid**: forms fold locally (every keystroke stays client-side),
  submit crosses the wire as one typed value, decoded by the SAME
  Schema that rendered the form.

Phase 3 contract (claimed: ui-wire). Transport-agnostic on purpose:
the server is a PURE `Stage[String, String, Nothing]` — event lines
in, patch lines out — so it runs over channels, an okay-mcp `Link`, a
WebSocket or stdio with the machinery that already exists, and is
tested with none of them.

```scala
object Wire:
  /** the server side: the first line out is the FULL TREE, then
   * narrow patches; an event naming a key not on the shown tree is
   * DROPPED — the tree is the capability list */
  def serve[S](init: S)(view: S => Ui)(update: (S, Event) => S)
  : Stage[String, String, Nothing]

  /** the client side: keep the tree, apply what arrives, render to
   * any Host; the host's own events go back as lines */
  def client(host: Host)(lines: Source[String],
             send: String => Unit ! Async): Unit ! Async

object WireJson:   // hand-mapped, the MCP-dialect precedent —
                   // Schema derivation needs Vector+recursion+defaults
                   // first (backlog: codec-vector)
  def uiJson(u: Ui): Json;      def uiOf(j: Json): Option[Ui]
  def eventJson(e: Event): Json; def eventOf(j: Json): Option[Event]
  def patchJson(p: Patch): Json; def patchOf(j: Json): Option[Patch]
```

- [x] every Ui, Event and Patch shape round-trips through Json
- [x] the serve stage's first line is the full tree; later lines are
      the narrow patches the diff makes, not full repaints
- [x] a FORGED event — a key that is not on the shown tree — is
      dropped: update never sees it (asserted by a counter that would
      have moved)
- [x] end to end over in-memory channels: a scripted user drives a
      server-held counter through a client Host; frames on the client
      equal the server's views
- [x] a second client connecting to a fresh serve of the SAME state
      begins with the full tree (reconnect's shape; session
      continuity itself is ui-durable's task)
- [x] a damaged line is dropped, not a crash — totality at this wire
      like every other

### Low level, in the UI context (phase 3, designed now)
- **codecs**: `Schema[Ui]`/`Schema[Event]`/`Schema[Patch]` are
  derivations, not designs — JSON and CBOR both arrive free, CBOR for
  the patch stream where chatter matters. Version drift across
  client/server is the codec-evolution problem Schema already has
  rules for (optional fields decode absent).
- **security**: an inbound Event is UNTRUSTED input with a natural
  validator — the currently shown tree. An event naming a key that is
  not on screen is rejected: THE TREE IS THE CAPABILITY LIST, and the
  server shows only what it is prepared to hear about. Form input
  decodes by Schema (total: damage is data), so structure cannot be
  injected; server-driven mode keeps logic and secrets off the client
  entirely; rate limiting is a bounded channel, which is the default.
- **fault tolerance**: the session state is a fold over events — that
  is EVENT SOURCING by construction: journal events intent-first
  (the okay-agent Durable shape), recover by refold, snapshot by
  writing S (a value). Reconnection is cheap because the tree is
  retained: send the full tree once, then patches; an unacknowledged
  patch resends from the kept tree, idempotently (patches against a
  named base version).

  Contract (claimed: ui-durable, on okay-persist stage 0). The
  journal is the INBOUND LINE STREAM, verbatim — forged and damaged
  lines included — because `Wire.serve` is deterministic about
  dropping them, and refolding through the SAME pure stage is the
  whole guarantee; filtering before journaling would be a second
  implementation of the rule. One session = one key
  (`Topic.route` picks the partition); a snapshot is the same
  partition under `key + "#snap"`, value `{upTo, s}` by `Schema[S]`,
  and until persist-stage1's compaction `latest` is a fold of the
  tail — correct, not yet cheap.

  - [x] refold determinism: a live journaled run and a recovery from
        its journal reach the SAME state — with forged and damaged
        lines in the stream
  - [x] crash and continue: k lines live, crash, recover, feed the
        rest — the final state equals the uninterrupted run's
  - [x] intent-first: a line appended but never processed (crash
        between append and update) IS in the recovered state — even
        when it follows a journaled Closed, because a Closed in the
        journal ended a CONNECTION, not the session's life: the
        refold folds segment by segment through the same stage
  - [x] snapshot: recovery from `latest` refolds only the tail
        (counted, not assumed), and equals the full refold
  - [x] two sessions sharing a topic and partition do not bleed:
        keys filter
  - [x] `serve` = recover + journal + continue, in one call, over any
        line transport
- **scaling**: a session is a fold with a value state — shard by
  session id, move a session by moving a value, fan a broadcast in as
  one more merged source. Nothing in the loop holds a lock; the
  channels' backpressure is the overload story (a slow client parks
  its own producer, not the server).

## ui-dom-patch — the raw-DOM Backend (the patch consumer arrives)

`Host.diffing` was built for a consumer that never came in v1; this
is it: React-less DOM, zero dependencies, the browser driven by the
core diff. The design leans on three pieces that already exist —
`React.elem` builds (the Elem tree IS the DOM plan), `Ui.patch`
mirrors (the backend keeps a value copy of the tree, so events
interpret against a value), `React.event` interprets (one delegated
listener at the root, walking up to the nearest `data-key`).

```scala
object Dom:   // okay-ui scala-js — js.Dynamic in, zero dependencies
  /** a Backend over a real (or fake) document and a mount node */
  def backend(document: js.Dynamic, root: js.Dynamic): Backend
```

- Patch paths walk `childNodes`: a Ui child maps to exactly ONE root
  element (the label wrapper of Input/Check is a leaf's root — paths
  never descend through it; Select's options are DOM children but
  never Ui children), so index-for-index navigation is sound.
- SetValue/SetChecked find the `input` inside a possible wrapper;
  SetSelected sets `selectedIndex`. Reorder appends the survivors in
  target order (appendChild MOVES a live node — the DOM's own
  primitive matches the patch's meaning). Remove/Insert are
  removeChild/insertBefore at the index.
- Events: ONE listener per kind (click/input/change) on the mount
  root; the handler walks parentNode to the nearest data-key and asks
  `React.event(mirror, key, kind, value)` — the same pure
  interpretation the React host uses, against the mirror tree kept by
  `Ui.patch`.

Behavior:
- [ ] the law, at the DOM: for scripted frame sequences (shuffles,
      removals, insertions, edits — the keyed battery's shapes),
      rendering through Host.diffing(Dom.backend(fake)) leaves the
      fake DOM equal to building React.elem(last frame) from scratch
- [ ] each narrow patch narrows: SetText touches textContent,
      SetValue reaches the input inside its label wrapper, Reorder
      MOVES nodes (the fake counts creations — a shuffle creates
      nothing)
- [ ] delegated events round-trip: a click on a button's node emits
      Pressed(key), input on a nested input emits Edited with the
      value, change on a check emits Toggled(!on) read from the
      MIRROR, a click on a keyless node emits nothing
- [ ] the fake document is the test's (createElement/appendChild/
      replaceChild/removeChild/insertBefore/setAttribute/
      addEventListener/textContent) — what Dom.scala uses and nothing
      more, so the glue's real surface is written down
- [ ] okayUi's JS tests exist again: the js test dir replaces
      `Test / sources := Seq()` and this suite runs under Node in the
      matrix

## Out of scope (v1)
- native toolkits (GTK/Cocoa/Win32) — satellites once the seam has
  proven itself on three free backends; Native runs the terminal host
- styling beyond bold/color, themes, animation
- focus traversal beyond linear tab order; mouse in the terminal
- accessibility trees (the Host seam is where they would attach)
- Windows terminals (raw mode is stty in v1)

## Decisions
- **The primary seam hands over the WHOLE TREE (`Host`), patches are
  derived** — reversed from the first sketch by the React
  requirement: hosts that reconcile themselves want the tree, and a
  patch-consumer is `Host.diffing(backend)` with the core diff. The
  other direction (a Host from a patch stream) needs keeping the
  tree, which is the diff again.
- **No functions in the tree** — keys instead, because a closure in a
  value breaks equality, diffing, serialization and JVM-testing of
  the React mapping. The cost is stringly keys; a typed key layer can
  come later without moving the tree.
- **Update is pure in v1; the world merges in as sources** — the
  effect row stays available (an app can run its own programs and
  feed a source), but the LOOP does not interpret commands. Elm's Cmd
  exists because Elm has no effect system; this library has one, and
  it composes by merge already.

## Results
v1 shipped 2026-09-01: core (tree, diff, patch, loop over merged
sources), the terminal's pure half (frames as values, key
interpretation) with the thin stty host, the React-shaped rendering
(pure `Ui => Elem` + five-line js glue + a Host over a root), Form in
both directions (typed `Schema[A]` and the dynamic JSON Schema
elicitation carries), and MCP elicitation end to end — a server's
question rendered as a form, filled by a scripted user, answered
typed. 15 tests in okay-ui, 3 in okay-mcp, 2 in okay-demo; all ten
behavior items covered; JS and Native legs compile. (v1 shipped
without Form on Native — okay-codec had no Native leg; codec-native
closed that the same day: the omission dated from P5 and was never a
decision, and the whole lex/parse/codec suite now runs as native
binaries, Form riding along.)

Phase 2 (ui-scenarios) shipped the same day: Dialog (one operation,
a GADT), Running as the stepped scenario, run over any Host, and the
scenario AS a screen — the continuation literally the state. Form.ask
and askSchema are form flows with retry-by-recursion, and the demo's
hand-rolled elicitation loop collapsed to
`Dialog.run(host)(Form.askSchema(...))`, which is the phase's claim
in one diff. 4 more tests.

Phase 3's wire (ui-wire) shipped 2026-09-01: WireJson (hand-mapped,
the dialect precedent — codec-vector filed for the derivation),
Wire.serve as a pure Stage (full tree first, then the diff's narrow
patches), Wire.client rendering to any Host, and the capability rule
asserted from the hostile side: an update that would THROW on a
forged key never hears about it. 6 tests; the end-to-end asserts the
client's frames equal the server's views over channels — and any line
transport (Link, WS, stdio) is the same stage by construction.

ui-screens shipped 2026-09-01: Screen as codata (view + step), Nav as
the stack, `Nav.screen` for the plain triple, `Nav.scenario` fusing a
Dialog program into a pushable screen whose answer arrives through
the parent's continuation. The heterogeneous-state problem never
appears: closures carry each screen's state, and the closure/value
line is exactly the wire. 5 tests, including the wizard pushed from a
parent and its answer renaming it.

ui-durable shipped 2026-09-01, on persist-core stage 0: the journal
is the inbound line stream verbatim (hostile lines included — the
stage's determinism about dropping them IS the argument), recovery
refolds segment by segment (a journaled Closed ended a CONNECTION,
not the session — found by the intent-first test), snapshots bound
the refold (counted, not assumed) until persist-stage1 makes latest
a lookup. Six tests, every one an equality between a live run and a
recovery.

The seam's claim is a test, not a sentence: TestPortable runs one
application on the test host and asserts the terminal renders exactly
those frames; TestElicitForm is the whole circle in one assertion.
