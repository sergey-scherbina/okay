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

## Stage 1 — the browser claims what HTML can say (ui-browser-vocab)

The browser is the one host whose medium HAS tables, lists, tabs and
disclosures. Claiming them is the frontend spec's own mechanism
("a rich client claims `Table` and draws a native table") applied to
the client that was already rich. Three renderers share one
vocabulary, because the two-roads law (react-host-vocab) says
`Html.render(t)` and the tree `Wire` shows a client whose hello names
the same set must agree — so the set is ONE constant and all three
read it.

```scala
object React:
  /** what a browser draws natively: the anchor, and the four semantic
   * nodes HTML has an element for. `Html` renders through `elem`, and
   * `LiveJs` says exactly this set in its hello, so the two roads
   * cannot disagree about which nodes are lowered */
  val Vocabulary: Set[String] = Set(Vocab.link, Vocab.table, Vocab.items, Vocab.tabs, Vocab.disclosure)
```

- `Table` → `<table data-key=k>` with `<thead>` from the header and
  `<tbody>` of `<tr>`; `weights` become `<col style="width:…%">` in a
  `<colgroup>` (a share of the width, never a pixel) — an empty vector
  writes no colgroup, as it lowered to equal shares before.
- `Items` → `<ul data-key=k>` of `<li>`; item keys stay on the item's
  own element, as the lowering put them.
- `Disclosure` → `<details data-key=k open?>` with a `<summary>`
  carrying the title. Toggling is `Toggled(k, open)` from the
  `toggle` event — which is the local behaviour the hybrid spec
  already gives a claiming client, now the browser's own element.
- `Tabs` → a `<div data-key=k role="tablist">` of buttons keyed
  `<k>$tab<i>` (the lowering's own keys — `keys(s) == keys(lower(s))`
  is why the same event contract holds) and the selected page; a
  claiming `live.js` switches locally as the hybrid stage says.
- `Modal` stays lowered: HTML's `<dialog>` needs a script to open and
  the plain road has none. Recorded, not hidden.

Patch paths: the DOM patch consumer (`Dom`, `live.js`) walks
`childNodes` index for index against the tree it holds, and a claimed
`Table` puts `<thead>`/`<tbody>`/`<tr>`/`<td>` between the tree's
children and the DOM's. The rule that keeps paths sound is the one
`Input`'s label wrapper already obeys: a Ui child maps to exactly one
element the path descends INTO, so `at(path)` learns the four shapes'
wrappers (`table > tbody > tr > td`, `ul > li`, `details > (summary |
body)`) from the mirror tree, never from the DOM. `Ui.path` says which
child index a Table row/cell is; the consumer says which element.

Behavior:
- [ ] `React.elem` renders `Table`, `Items`, `Disclosure`, `Tabs` as
      the elements above; `Html.render` emits the same markup
      (through `elem`, as today); `Modal` is lowered as before
- [ ] the two roads agree: for a tree holding every semantic node,
      `Html.render(t)` and the tree `Wire.serve` shows a client whose
      hello says `React.Vocabulary` carry the same elements
      (TestLink's two-roads test, extended to the whole set)
- [ ] the DOM law at the claimed nodes: through
      `Ui.diffing(Dom.backend(fake))` every keyed battery shape
      (shuffles, removals, insertions, edits — inside table rows and
      list items) leaves the fake DOM equal to `React.elem(last)` built
      from scratch; a shuffle of table rows creates no `<tr>`
- [ ] `live.js` says `React.Vocabulary` in its hello, applies patches
      inside the four shapes (`TestLiveJs` or the e2e browser suite:
      a `SetText` on a cell lands in the `<td>`), toggles a
      `Disclosure` locally, switches `Tabs` locally
- [ ] `Html.events` on a claimed `Disclosure`: a posted `open` field
      is the `Toggled` a lowered one's button would have sent — the
      plain road stays whole without a script
- [ ] okay-watch deletes its `nth-child` table rules and the page
      renders `<table>`s: the product's test that pins the cases row's
      shares now reads `<col>` widths (their lane, after the bump —
      the criterion of this spec, not a checkbox this repository can
      tick)

## Stage 2 — text carries its intent (ui-text-intent)

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

Behavior:
- [ ] `Text("DE00…", Style(kind = Ident))` renders as a span with
      `okay-kind-ident` in React/Html, as the same characters in the
      terminal; `Kind.Number` + `Align.End` right-aligns in the
      terminal's column and writes the two classes in the browser
- [ ] the wire: a `Style` with both defaults encodes as it always
      did (byte-equal on the conformance script); one with a token
      round-trips through JSON and CBOR; `docs/protocol/frontend.md`
      re-rendered
- [ ] `Form.of` marks a numeric field's rendered value `Kind.Number`
      (it already picks `InputKind.Number`), so a form and a table
      say the same thing about a number
- [ ] okay-watch replaces its fifteen positional selectors with
      tokens on the cells (their lane; the criterion)

## Stage 3 — one blank, in Form (form-blank)

```scala
object Form:
  /** the value a form STARTS from: every `Check` false, every `Select`
   * on its first option — the value the shown tree already displays,
   * so a submit that changed nothing decodes. A sum's case knob is a
   * Select, so the first case is chosen; an Option field stays absent
   * (absent is what "not required" means) */
  def blank[A](using Schema[A]): Json
```

Order of work, per no-failing-test-no-fix: first the test that shows
the defect — `Live.form[A]` (or `Form.ask`) over a case class with a
sum field, the user edits one text field and submits without touching
the Select: today the decode answers "choose one". Then `Form.blank`,
then `Forms.defaults` becomes `Form.blank` (one line) and the test
passes; okay-watch's `Analyst.blank` is deleted at its next bump.

Behavior:
- [ ] the failing test first: a Live form over a schema with a sum
      field, submitted with the Select untouched, decodes — and the
      same test against `Forms.defaults` before the change is the
      record of the defect
- [ ] `Form.blank[A]` for a schema with nested products, a sum, a
      list, an Option and a Boolean: every Check false, every Select
      (a sum's `$case` knob included) on its first option, the Option
      absent, the list empty — and `Form.decode` of it succeeds
      whenever every required text field has a value
- [ ] `Forms.defaults` is `Form.blank`; TestForms passes unchanged
- [ ] `Form.of[A](Form.blank[A])` renders the same tree as
      `Form.of[A](JObj(empty))` — the blank is what the tree already
      shows, made into a value

## Stage 4 — the tokens' stylesheet, beside the tree (ui-html-css)

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
- [ ] `Html.css` names every class `React.elem` writes: a test walks
      the renderer's class vocabulary (a tree with every node, every
      token) and asserts each class has a rule — a token added without
      a rule fails here rather than rendering unstyled
- [ ] the base rules: a `Box` weight is `flex`, a cell wraps
      (`overflow-wrap:anywhere` on `td`, `normal` on `th`), a `Form`
      is a column, `Tone.Danger` reads `--okay-danger`
- [ ] okay-script serves the file and `installable`'s `app.css`
      carries the base; TestMobileHead's four files are five, or
      `app.css` embeds the base — either way one request per page
- [ ] okay-watch's stylesheet is its palette and scale (their lane;
      the criterion)

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
