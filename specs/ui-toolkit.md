# ui-toolkit — Form v2 and the composed dialogs

## Overview
specs/ui.md landed Form as the fifth algebra over Schema with honest
v1 limits: flat products of primitives; a nested product, sum or list
renders as a Text naming itself; one error per submit; no cross-field
validation. This spec lifts the limits — per the user's direction,
derivation and cross-field validation arrive TOGETHER, not staged —
and adds the small composed dialogs (confirm/alert/prompt/choice)
that every scenario was about to hand-roll.

The law of v1 is kept and extended: **the form cannot drift from the
parser** — what the form renders it renders FROM the Schema, what it
submits the same codec decoder accepts, including the nested, sum and
list shapes exactly as the wire writes them (`{"Case": {...}}`, JSON
arrays).

## Interface
- `Form.of[A: Schema]: Json => Ui` — now total over the algebra:
  - a nested `SProduct` renders as a titled section; field keys are
    dotted paths (`addr.city`), and `edit` routes by them;
  - an `SSum` renders as a `Select` of case names plus the chosen
    case's subform; choosing swaps the case (value resets to that
    case's empty object, keeping the codec's one-key shape);
  - `SList`/`SVector` render items in order with add/remove buttons
    (`k$add`, `k[i]$del`); item keys index (`k[i]` prefixes);
  - `SIso` renders as what it wraps; `SOption` marks optional.
- **Errors as data, per field**: `Form.errors[A](value): Vector[(path,
  message)]` — each failing field's message renders UNDER that field;
  the single-blob v1 message survives only for form-level failures.
- **Cross-field validation**: `Form.ask[A](message, checks*)` where a
  check is `A => Vector[(path | "", message)]` — run only after the
  decode succeeds (a check reads the typed value, not strings);
  submit refuses until both layers are clean.
- Composites, as Dialog programs over the machinery ask already uses:
  `Toolkit.confirm(text): Boolean`, `alert(text): Unit`,
  `prompt(text): Option[String]`, `choice(text, options):
  Option[Int]` — all `! Dialog`.

## Behavior
- [x] a nested product renders as a titled section, edits route by
      dotted path, and the submitted value decodes by the codec
- [x] a sum renders as case-Select plus the chosen case's subform;
      choosing swaps the subform; the submitted value keeps the
      codec's `{"Case": {...}}` shape and decodes
- [x] a list renders its items with add/remove; item edits route by
      index; the submitted value is the codec's array and decodes
- [x] per-field errors: a submit with two bad fields shows two
      messages, each under its field, not one blob
- [x] cross-field checks run on the DECODED value, name their field
      (or the form), and hold submit until clean
- [x] the composed dialogs answer through the same event contract
      (ok/cancel/choice), scripted-host tested
- [x] the drift law, extended: for a case class with nesting, a sum
      and a list, the Json the form submits round-trips through the
      codec decoder unchanged

## The typed wizard (ui-pwizard — the PState alternative)
An ALTERNATIVE spelling of wizards beside the monadic Dialog, nothing
changed or removed: in a Dialog flow collected values thread through
lambdas; in `PWizard` they thread through a state whose TYPE GROWS —
PState's typestate (Atkey; theory textbook ch. 3) with the machine as
the threaded answer type. A step is `Cont[A, S2 => Machine, S =>
Machine]`: it NAMES its state requirement, so the compiler enforces
step order — asking the age before the name is a type error, proven
by compileErrors. `ask`/`get`/`mod`/`step` (the last with a built-in
validation retry loop); `toDialog` bridges any machine into an
ordinary Dialog program, so a typed wizard runs anywhere Dialog runs.

- [x] the typed wizard collects through a growing state, views read
      the typed state-so-far, validation retries in place
- [x] the bridge runs the same wizard as a Dialog program over a Host
- [x] misordered steps do not compile

## Dialog scopes (dialog-delim — Delim as an option)
Delim integrated into Dialog WITHOUT touching it: scenarios may run
in the `Delim + Dialog` row (`Scope.Row`), where a typed prompt
delimits a cancellable sub-flow. `Scope.push` installs scopes,
`cancel(p)(value)` exits the NAMED scope from any depth with no
Option threading on the steps between, and one `run`/`scoped` erases
the row at the top — after which it is an ordinary Dialog program.
The multi-prompt capability is the point and is tested: an inner
scope aborts ACROSS its own boundary to the outer one, which nested
handlers cannot express (theory textbook ch. 2, the Dybvig–Peyton
Jones–Sabry design points). Nesting discipline stated: scopes nest
by push under ONE run — a prompt lives in the machine that pushed it.

- [x] a scope cancels as a unit: no Option threading between steps
- [x] an inner scope aborts across its boundary to the outer prompt,
      and the outer's remaining steps never render
- [x] plain Dialog scenarios run unchanged beside scoped ones

## ui-direct: the three roads reach the toolkit
- **Direct wizards**: a Dialog scenario reads as straight-line code
  under `direct[[A] =>> A ! Dialog] { ... .reflect ... }` (the `?`
  spelling collides with Effects' own row-`?`; the named mark is the
  row idiom, tested). v1's no-marks-under-lambda holds wizards fine —
  they are sequential by construction.
- **askWith(policy)**: ask's retry POLICY lifted out via conditions —
  an invalid submit signals InvalidSubmit(errors, attempt); the
  forgiving policy IS ask (reask), patience(n) gives up to None, a
  repairing policy Resumes with a forced value; a valid submit never
  consults the policy. The machine runs per submit over a pure
  program; the dialog loop is ask's own.
- **Ambient Host doors**: Dialog.hosted / Nav.hosted — the app's one
  host as a capability; run(host) stays.

- [x] the direct wizard equals the flatMap wizard on the same script
- [x] askWith: forgiving≡ask, patience gives up, repair resumes, a
      valid submit never consults the policy
- [x] hosted ≡ run(host)

## Recursive-schema depth safety (2026-09-11, form-recursive-depth-safety)

`editAt`, the render pipeline (`render`/`field`/`sumUi`/`listUi`) and
the validation pipeline (`errorsOf`/`listErrors`) all recurse on a
Schema+`Json` pair's own depth for a RECURSIVE schema — the same
defect shape `okay-codec`'s decode and write sides both had
(`remove-codecs-maxdepth`, `encode-side-depth-safety`), one layer up.
Found by re-auditing that same grep a second time, at a direct
question ("did you fix everything?"), not from a bug report.

Three ways the value gets deep with nothing bounding it: `editAt`
recurses once per PATH SEGMENT, and a path is a dotted STRING an
`Event` carries — `submitted`'s own doc comment already says a
REMOTE submission road exists ("the edits a client folded locally
... folded here through the SAME edit a live edit takes"); a batch of
many edits (`Event.Submitted`) each adding one level to a recursive
field accumulates an arbitrarily deep value with no check anywhere;
and `Form.of`/`ofWith` take an arbitrary `Json` directly, not only one
built through local edits.

Same `Codecs.NativeThreshold`-then-`Cont.defer` split as the codec's
own doors: `editAt` mirrors `Json.mergePatch` (value-returning),
`render`'s four mutually-recursive functions mirror `Json.into`/
`intoC` (no mutable/ordered side effect in this pipeline — it only
COMBINES immutable `Ui` values, so unlike `Cbor.putC` there is no
hazard in letting `eachField`'s eager per-field callback build several
`Cont` values ahead of running any of them).

**Found along the way, not by design: `Json.encode` (used by
`Json.write`, distinct from `Json.print`) was missed entirely by
`encode-side-depth-safety` — this lane's own tests needed `Json.write`
on a deep fixture and hit its native recursion directly.** Fixed the
same way, and its first draft (string interpolation/`mkString`) was
itself quadratic — see specs/codecs.md's own entry for the full story.

**`render`/`errorsOf` cost O(depth²) MEMORY, not O(depth) — inherent,
not a bug to fix.** `key(prefix, name)` rebuilds the whole dotted path
as a fresh string at every level, so N nested UI elements each
carrying their own O(N)-length key cost O(N²) total bytes: giving
every nested element its own addressable dotted key is the FEATURE,
and no data structure removes that cost once N strings of total length
O(N²) must exist. `editAt` has no such cost (`List[Seg]`, never
rebuilds a string) and is tested at 100 000; `render`/`errorsOf` are
tested at 5 000 — comfortably past `NativeThreshold` (24), proving the
native-recursion cap is gone without demanding gigabytes of heap to
prove it (MEASURED: 100 000 levels of `render` exhausted a stock test
JVM's heap outright).

Behavior:
- [x] `Form.edit` follows a path 100 000 segments deep, correctly,
      with no native stack cost
- [x] `Form.render`/`Form.errors` handle a value 5 000 levels deep
      with no native stack cost
- [x] ordinary shapes below the threshold are unaffected (all three
      pipelines)
- [x] `Json.write`/`Json.encode` handle a value 100 000 levels deep,
      in the milliseconds `Cbor.write` already did (not the tens of
      seconds a quadratic first draft needed)

## Out of scope
- Layout/styling beyond bold/dim (specs/ui.md owns Style).
- Async validation (a validator that needs IO is a scenario's job).
- The dynamic (`askSchema`) side gaining nesting — elicitation's spec
  restricts it to flat objects; it stays v1 by DESIGN, stated here.
- Removing `render`/`errorsOf`'s inherent O(depth²) key-string cost —
  not fixable without removing per-element addressable keys, which is
  the point of dotted paths (this spec's own "Decisions" section).

## Decisions
- **Derivation total over the algebra now, not staged** — user's
  call; the alternative (land sections first, sums later) was
  declined explicitly.
- **Dotted paths in keys, not nested state** — events carry one
  string key; parsing a path at the edit site keeps Ui itself flat
  and the diff/patch machinery untouched.
- **Checks after decode** — a cross-field rule reads types, not raw
  strings; the decode boundary already exists and is the same
  decoder the wire uses.
