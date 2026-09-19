# ui-product — what the first product with a screenshot found

## Overview

specs/ui.md, specs/frontend.md, specs/ui-toolkit.md and specs/ui-html.md
are closed: one application is drawn by the terminal, React, the raw
DOM, Swing, GTK 4, a scriptless HTML form, and over the wire to a
browser, a Compose window, a SwiftUI package and an Android APK; the
laws (`keys(s) == keys(lower(s))`, diff commutes with lowering,
diff-then-patch equals the next tree) hold on every node and every
backend. Until 2026-09-17 that is what tested the toolkit: the laws
and the demos.

Then okay-watch (`../okay-watch`, its specs/ui.md) drew the analyst's
page as a `Ui` value, served it by the plain road and the socket from
ONE `Analyst.view`, and the operator looked at the screenshot. In two
days the product found four things the laws could not: a `Link` in a
`Table` cell reached the browser as text (react-host-vocab, fixed
upstream the same day), `Table` could not say how wide a column is
(ui-table-weights, fixed upstream the same day), and two more that are
still living in the product's own code and stylesheet — which is what
this spec is for. The pattern is the finding: **the architecture is
proven; the layer between it and a page a person reads well was never
exercised by anything with a reader.** Every lane below is one thing
the product had to write itself, and the criterion for each is the
same: the product's copy is deleted and the page is unchanged.

The four, in the order they pay:

1. **The browser draws a `Table` as boxes.** The richest client claims
   only `link`; a table arrives lowered into `Box` of `Box`, and the
   product wrote forty lines of `nth-child` CSS to dress rows of boxes
   as a table — one column added and every rule shifts. Nothing in
   the DOM says "table", so nothing assistive can read it as one.
2. **Text has no typographic intent.** The product's stylesheet says
   it in a comment: *what a cell says decides how it is set* — an
   identifier is monospaced so it can be read against an explorer, a
   number is tabular so it can be compared down a column, prose wraps
   and never ellipsizes. Today that is fifteen positional selectors
   in a product; it is a TOKEN the tree should carry, like `Tone`.
3. **The value a form starts from exists twice outside okay-ui and
   the two disagree.** okay-script's `Forms.defaults` sets every
   `Check` to false; okay-watch's `Analyst.blank` sets every `Check`
   to false AND every `Select` to its first option, because "a submit
   that changed nothing sends no events" and a sum's case knob left
   unset decodes as "choose one". By reading, `Live.form[A]` over a
   schema with a sum field has that defect today (`Live.scala:118`
   takes `Forms.defaults`; `Form.scala:428` answers "choose one" for
   the absent knob). Not yet shown by a failing test — that is the
   lane's first step.
4. **The HTML host ships no stylesheet for its own tokens.** `React`
   writes `okay-bold`, `okay-tone-danger`, `okay-row`, `okay-form`;
   the only CSS that knows those names is `Mobile.app.css` in
   okay-script and whatever each product writes. A product on `Html`
   starts from unstyled HTML and copies a token map it cannot see.
   `LiveJs` moved to okay-ui for exactly this reason (ui-html stage 2):
   the client of the tree lives beside the tree; so does its style.

## Stage 1 — the browser claims what HTML can say (ui-browser-vocab, LANDED 2026-09-18)

**THE SET IS `table`, AND THE SPEC'S FIRST SKETCH OF FOUR WAS WRONG.**
Written before the code was read, this stage said the browser claims
`Table`, `Items`, `Tabs` and `Disclosure` because HTML has an element
for each. It does — and the element is not the cost. The cost is what
a node's element structure inserts between a patch PATH and the child
that path names, and it is different for each of the four:

- **`Table` inserts nothing**, because no path ever descends into one:
  `Ui.diff` has no `Table` case, so a changed table is a `Replace` at
  its own path. A consumer only ever builds a table or swaps one
  whole, so `Dom`, `live.js` and any future claiming client need no
  new walking at all. THE PRICE is that same sentence read the other
  way: a changed CELL now replaces the table where the lowering gave a
  narrow `SetText`. Stated, not hidden. Trigger for revisiting: a page
  that measures the difference (a table whose cells change every tick
  while its shape does not — okay-watch's five-second push is the
  candidate, and today nothing in its tables changes between ticks).
- **`Items` would insert an `<li>`**: every patch consumer would have
  to unwrap it on the way down and wrap it on `Insert`/`Reorder`/
  `Remove` — in `Dom.scala`, again in ~240 lines of hand-written
  JavaScript, and again in every client that ever claims it — to gain
  `<ul>` over `<div>`. Trigger: a measured assistive-technology
  requirement for list semantics, not a preference for nicer markup.
- **`Tabs` would oblige every claiming client to switch tabs itself**
  (the hybrid rule's local behaviour), in each client's own language,
  and `role="tablist"` earns its keep only with arrow-key handling.
  The lowering's buttons round-trip correctly today. Trigger: a page
  whose tab switch measurably wants the round trip gone.
- **`Disclosure`'s `<details>` toggles natively and tells nobody**, so
  on the scriptless road the server's `open` and the browser's would
  disagree from the first click, where the lowered button is a POST
  that keeps them in step. Trigger: the live road only.
- **`Modal`'s `<dialog>` needs a script to open**, which is where the
  browser's vocabulary stops, as this spec already said.

What landed, therefore, is the first of the four and the mechanism the
other three would need, written down where the next reader meets it
(`React.Vocabulary`'s own comment).


The browser is the one host whose medium has these elements, and the
one that was already rich. Three renderers serve it — `React.elem`,
`Html` (which renders through `elem`) and `LiveJs` — so the set is ONE
constant all three read, because the two-roads law (react-host-vocab)
says a page served both ways is one page only if the socket's hello
and the scriptless render claim the same nodes.

```scala
object React:
  /** what a browser draws natively: the anchor, and the one semantic
   * node whose element structure costs the patch consumers nothing.
   * `LiveJs` GENERATES its hello from this value, so the two roads
   * cannot disagree about which nodes are lowered */
  val Vocabulary: Set[String] = Set(Vocab.link, Vocab.table)
```

- `Table` → `<table data-key=k class="okay-table">`, a `<colgroup>` of
  `<col style="width:N%">` when the weights fit the header (a SHARE
  turned into a percentage; no pixel crosses the wire), `<thead>` of
  `<th scope="col">`, `<tbody>` of `<tr>`/`<td>` wrapping each cell's
  own element. Empty or mis-sized weights write no colgroup, which is
  the even split the lowering always gave.
- Nothing else changes shape. `Items`, `Tabs`, `Modal` and
  `Disclosure` keep arriving as their lowering, for the reasons above.

Patch paths need no new machinery, and that is the whole reason this
node was the one to claim: `Ui.diff` has no `Table` case, so no path
ever descends into a table and the `<td>` wrapper is invisible to
every consumer. `React.event` still finds a widget in a cell, because
it walks `Ui.focusable`, which lowers with `Set.empty` and is
unchanged.

Behavior:
- [x] `React.elem` renders `Table` as `<table>` with a `<colgroup>`
      from the weights (a share as a percentage — integer division,
      browsers normalise, no pixel crosses the wire), a `<thead>` of
      `<th scope="col">` and a `<tbody>` of `<tr>`/`<td>`; empty or
      mis-sized weights write no colgroup, which is the even split the
      lowering always gave; `Html.render` emits the same markup
      through the same `elem`, and `col` is a void element there
- [x] the two roads agree: for `Table`, `Items`, `Tabs`, `Modal`,
      `Disclosure` and a table nested in each, `Html.render(t)` equals
      `Html.render(Ui.lower(t, React.Vocabulary))` — the tree the
      socket sends a client whose hello says exactly that set
      (TestBrowserVocab; TestLink's own two-roads test now reads the
      constant rather than a literal `Set(link)`)
- [x] the laws do not move when a node is claimed: `keys(table) ==
      keys(lower(table))`, the tab order is the cells' widgets, and
      `React.event` still finds a button in a cell by key
- [x] the DOM law battery carries a claimed table: through
      `Ui.diffing(b, React.Vocabulary)` the fake document equals
      `React.elem(last frame)` built from scratch, across a table
      gaining a row and changing a cell
- [x] `Dom.host` is the door: it lowers with `React.Vocabulary`, so a
      table reaches the document as a `<table>` and an unclaimed
      `Items` as the lowering's boxes — `Ui.diffing`'s `Set.empty`
      default would have lowered BOTH, which is react-host-vocab's
      defect one layer out
- [x] a REAL browser builds it (ui-table-gap-and-proof,
      `TestTableBrowser`, Live-tagged): the scriptless render is a
      `<table>` with its `<colgroup>`, and a press over the socket
      lands a patch inside it — the half a fake document cannot prove,
      because `live.js` is JavaScript no Scala test executes
- [x] `live.js` builds a real table and says exactly `React.Vocabulary`
      in its hello — GENERATED from the constant, not typed, and
      guarded by a test that fails when the set names a node the
      script cannot build
- [x] okay-watch deleted its forty lines of table dressing and the
      page renders real `<table>`s (their 56289bc, okay submodule at
      7e756f87) — the criterion, ticked from their side

## Stage 2 — text carries its intent (ui-text-intent, LANDED 2026-09-18)

`Style` gains what the product's stylesheet was saying by position:

```scala
/** what a text IS decides how a host sets it: an identifier is read
 * against something else, a number is compared down a column, prose
 * is read. Tokens, as Tone is — a host maps them to its idiom */
enum Kind:
  case Prose, Ident, Number

enum Align:
  case Start, End

final case class Style(bold: Boolean = false, dim: Boolean = false,
                       tone: Tone = Tone.Plain, size: Size = Size.Normal,
                       kind: Kind = Kind.Prose, align: Align = Align.Start)
```

- The wire: two new optional fields on `Style`, so an old client reads
  a new server (the codec-evolution rule `Protocol` already leans on)
  and the document/conformance re-render.
- React/Html: `okay-kind-ident` (`font-family: ui-monospace`),
  `okay-kind-number` (`font-variant-numeric: tabular-nums`,
  `text-align: end`), `okay-align-end`; **prose wraps and never
  ellipsizes** is the base rule of stage 4's stylesheet, stated there.
- Terminal: `Kind.Number` and `Align.End` right-align within the
  column a `Box`'s weight gave the cell — `Frame` pads on the left;
  `Ident` is unchanged (a terminal is already monospaced).
- Compose/Swift/GTK/Swing: the token is read and mapped where the
  toolkit has a word for it (`FontFamily.Monospace`, `TextAlign.End`);
  where it has none the token is ignored, recorded per host.
- `Table` cells: the author sets the token on the cell's `Text`; a
  table does not guess from content, because "an eight-character
  string" is not a rule (an IBAN is prose to a sorter and an
  identifier to a reader).

**THE WIRE CHANGED, AND THIS SPEC SAID IT WOULD NOT.** The stage as
written promised "a `Style` with both defaults encodes as it always
did (byte-equal on the conformance script)". That was wrong and the
first run said so: the derived codec writes EVERY field, so every
styled `Text` on the wire gained two keys and `conformance.jsonl`
re-rendered. The precedent for what to do was already here —
`Table.weights` did exactly this in ui-table-weights — and the rule it
set is the one followed: a new field is written, an OLD client ignores
it, and a NEW client reads an absent one as the default. Both halves
are now tested here, the second against a hand-written line from a
server that predates the lane.

**AND BOTH THIN CLIENTS HAD TO FOLLOW, for a reason worth keeping:
their conformance test RE-ENCODES the tree it holds and compares it to
the script.** So a client that decodes the two tokens but does not
write them back fails the script — which is the conformance test doing
exactly its job. `okay-compose/protocol` and `okay-swift`'s
`OkayProtocol` carry `Kind` and `Align` now, and both draw them
(Compose: `FontFamily.Monospace`, `TextAlign.End`; SwiftUI:
`.monospaced` design and `monospacedDigit`). `swift test` 3 of 3,
`./gradlew :protocol:test` green.

Behavior:
- [x] `Text("DE00", Style(kind = Ident))` is `okay-kind-ident` in
      React and `Html`; `Kind.Number` with `Align.End` writes both
      classes; a DEFAULTED `Style` writes neither, on every host
- [x] the terminal right-aligns an `Align.End` cell inside the column
      its weights gave it — the padding moves from one side to the
      other and the row's width does not change
- [x] `live.js` writes the same two classes from the same short names
- [x] Swing draws a monospaced identifier and a right-aligned label;
      GTK adds its own `monospace` and `numeric` style classes.
      NOT DRAWN, recorded: Swing has no tabular-figures switch, GTK's
      alignment would need a `gtk_label_set_xalign` binding nothing
      has asked for
- [x] the wire: both tokens round-trip through JSON lines and CBOR
      bytes, spelled short (`"ident"`, `"end"`) like every other
      enumeration; a `Style` from a server that predates the lane
      decodes to the defaults; the document and the conformance script
      are re-rendered
- [x] the two thin clients carry and draw the tokens, and their
      conformance suites pass against the re-rendered script
- [x] okay-watch replaced its fifteen positional selectors with
      tokens on the cells (their 56289bc): `Style(kind = Kind.Ident)`
      on every identifier a reader checks against an explorer,
      `Kind.Number` on every figure compared down a column

WITHDRAWN from this stage, with the reason: "`Form.of` marks a numeric
field's rendered value `Kind.Number`". A form renders a number as an
`Input` of `InputKind.Number`, which already says what it is — there
is no `Text` there to mark. The tokens are for what a page DISPLAYS,
and a form displays fields.

## Stage 3 — one blank, in Form (form-blank, LANDED 2026-09-18)

```scala
object Form:
  /** the value a form STARTS from: every `Check` false, every `Select`
   * on its first option — the value the shown tree already displays,
   * so a submit that changed nothing decodes. A sum's case knob is a
   * Select, so the first case is chosen; an Option field stays absent
   * (absent is what "not required" means) */
  def blank[A](using Schema[A]): Json
```

Order of work, per no-failing-test-no-fix: the test that shows the
defect first, then the function.

THE MECHANISM, SHARPER THAN THIS SPEC FIRST STATED IT. The stage was
written as "a Live form over a schema with a sum field decodes
'choose one'". Reading the code found which ROAD: on the scriptless
road `Html.events` emits an event only for a field whose post DIFFERS
from what was shown, so a user who fills the text fields and submits
says nothing at all about a `Select` sitting on its first option, and
the submission is refused for a field they can see is answered. The
LIVE road never had it — `live.js` and `Ui.submit` both collect every
field's current value — which is exactly why okay-watch (a plain-road
page) hit it and okay-script did not.

AND A THIRD WIDGET, FOUND BY THE TEST RATHER THAN BY DESIGN: a list.
An absent array is a missing field to `Form.errors` and to the codec
alike (measured — the two walks agree), while the form SHOWS an empty
list. The tree has no event meaning "be empty", but it has two that
compose into one: the `+` the form draws and the `-` on the item it
just made. The blank presses both, which keeps it inside the edit
vocabulary rather than writing JSON at a path — the property that
makes it unable to drift from the renderer.

- [x] the failing test first: the plain road's post, folded — with the
      empty object the submit is refused, and that assertion stays in
      the suite as the record of the defect
- [x] `Form.blank[A]` for a schema with nested products, a sum, a
      list, an Option and a Boolean: every Check false, every Select
      (a sum's `$case` knob included) on its first option, the list an
      empty array, the Option absent — and the decode succeeds once
      the text fields are filled
- [x] `Forms.defaults` is `Form.blank[A]`, one line; okay-script's
      tests pass unchanged
- [x] `Form.of[A](Form.blank[A])` renders the same tree as
      `Form.of[A](JObj(empty))` — the blank is what the tree already
      shows, made into a value
- [x] okay-watch deleted `Analyst.blank` (their 56289bc)

## Stage 4 — the tokens' stylesheet, beside the tree (ui-html-css, LANDED 2026-09-18)

```scala
object Html:
  /** the level-L stylesheet for a browser: the tree's containers
   * (`okay-row`, `okay-col`, `okay-box`, weights as flex), the text
   * tokens (`okay-bold`, `okay-dim`, `okay-tone-*`, `okay-size-*`,
   * `okay-kind-*`, `okay-align-*`), the claimed elements of stage 1,
   * and the rule that a cell WRAPS and never ellipsizes. Colours and
   * sizes are CSS custom properties (`--okay-danger`, `--okay-base`),
   * so a product themes by setting six variables, not by rewriting
   * selectors. No layout the tree did not say */
  val css: String
```

- okay-script serves it at `/__okay/okay.css` beside `live.js`;
  `Mobile.app.css` keeps only what a PHONE adds (44px targets, 16px
  inputs, the viewport) and imports the base by reference in the same
  file rather than by a second request.
- okay-watch's `UiPage.style` shrinks to its palette (six variables)
  and its type scale — the arms race the product's spec describes
  (`overflow-wrap:anywhere`, the header's `normal`) is the base
  rule's, stated once.

Behavior:
- [x] `Html.css` names every class `React.elem` can write: the test
      walks a tree using every node and every token, collects the
      classes the renderer actually wrote, and fails on any without a
      rule — so a token added to the tree without a rule is a red test
      rather than a page that quietly renders it unstyled. The walk is
      shown to be non-vacuous (three classes named, a name nothing
      writes shown absent)
- [x] the base rules: a cell wraps and never ellipsizes
      (`overflow-wrap: anywhere` on `td`, `normal` on `th`), an
      identifier is monospaced, a number's figures are tabular; and
      NOTHING about a `Box`'s weights, because layout is the tree's and
      React writes the flex inline
- [x] theming is six custom properties (`--okay-fg`, `--okay-muted`,
      `--okay-accent`, `--okay-danger`, `--okay-line`, `--okay-base`),
      read by the rules rather than restated
- [x] okay-script serves the base at `/__okay/okay.css` and
      `Mobile.css` is `Html.css` plus what a PHONE adds — one file,
      one request, no `@import`. What stayed there is exactly the
      phone's own: 44px tap targets, 16px inputs, a wrapping row, a
      reading measure on a wide screen
- [x] okay-watch's stylesheet shrank to its palette, its doubled type
      scale, the filter strip, the case's forms and the door (their
      56289bc); the base is `Html.css`, inlined in their own `<style>`
      because that product serves its own routes rather than
      okay-script's container

## The consumer closed every criterion (2026-09-18)

All four stages ended with the same line — "okay-watch deletes its
copy and the page is unchanged" — and all four are ticked from that
side now (their commit 56289bc, this repository at 7e756f87). The page
looks as it did and cannot drift: a column's width lives in the
`<colgroup>` the author's weights render to, and what a cell IS
travels with the cell.

ONE THING THE MOVE ITSELF BROKE, and it is the lesson worth keeping
from the consumer side: their two widest tables scrolled because the
stylesheet made them `display:block`, which stops a table being a
table box — so the `<colgroup>` this arc existed to deliver would have
stopped applying at exactly the two tables that needed it. The fix was
not CSS: `Ui.Scroll` is the level-L node for "this box scrolls", every
host draws it, and saying it in the TREE is what the two levels are
for. A stylesheet that has to change an element's display type is
usually saying something the tree should have said.

## Out of scope
- **`Modal` as `<dialog>`** — needs a script to open; the plain road
  is why the browser's vocabulary stops where HTML without a script
  stops. Trigger: a product wanting a modal on the live road only.
- **A terminal beyond v1's keys** (arrows, Shift-Tab, a cursor inside
  `Input`, a `Scroll` that scrolls, mouse) — recorded in BACKLOG under
  `ui-terminal-v2` with its trigger: a product that runs on the
  terminal host for a reader, not a demo. okay-watch's `--tty` is a
  host test, not a reader.
- **Native hosts drawing a product screen** — Swing, GTK, Compose,
  SwiftUI have each drawn the conformance script and a counter; none
  has drawn okay-watch's page. The next product on a native client
  will find its own four things; nothing to build ahead of it.
- **Typed keys** — specs/ui.md's "a typed key layer can come later";
  okay-watch lives on `open:<id>` and has not asked.
- **Dates and money as nodes** — every timestamp on the product's
  page needed formatting on the product's side, and that is the
  product's: a locale and a format are not the tree's business. If a
  second product formats the same way, a `Kind.Time` token is the
  shape, not a node.

## Decisions
- **The browser claims, the server still lowers.** Stage 1 is the
  frontend spec's own mechanism, not a new one: `live.js`'s hello
  names more, `Wire.serve` lowers less, and a thin client that says
  nothing still gets boxes. No lowering logic moves to any client.
- **One vocabulary constant for three renderers.** react-host-vocab
  showed what a constant standing where a host's vocabulary belongs
  costs: two roads disagreeing on one tree. `React.Vocabulary` is read
  by `React.elem`, `Html` and written into `LiveJs.source`, and the
  two-roads test is what enforces it.
- **Intent as a token, not a table rule.** The product's first
  instinct was "the table should know an id column is monospaced";
  the product's own comment says why not — what a cell says decides,
  and only the author knows what it says. So the token sits on the
  `Text`, where `Tone` already does, and `Table` stays ignorant of
  content.
- **`blank` derives from the tree, not from the Schema directly.**
  Both existing copies fold `Ui.focusable(Form.of[A](empty))` through
  `Form.edit`, so the blank is BY CONSTRUCTION the value the shown
  form displays — a second derivation from the Schema could drift
  from the renderer, which is the one drift the form road forbids.
- **A stylesheet is part of a host.** `LiveJs` moved into okay-ui
  because the client of the tree belongs beside it (ui-html stage 2);
  `Html.css` is the same argument for the same host. Products theme
  through custom properties; they do not restate the token map.
- **Products tick their own boxes.** Each stage ends with "okay-watch
  deletes its copy" and that is the criterion, but it is their lane,
  after their submodule bump, gated in their repository — this spec
  marks those lines as criteria, not as checkboxes this repository
  can complete.

## Results
(none yet — the spec is the deliverable of claim ui-product)
