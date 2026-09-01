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

## Out of scope
- Layout/styling beyond bold/dim (specs/ui.md owns Style).
- Async validation (a validator that needs IO is a scenario's job).
- The dynamic (`askSchema`) side gaining nesting — elicitation's spec
  restricts it to flat objects; it stays v1 by DESIGN, stated here.

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
