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
