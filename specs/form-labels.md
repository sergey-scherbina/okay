# form-labels — a field's label, stated by the caller

## Overview

`Form.render` labels every field by its schema field name: a case
class field `perMinute` renders as a leaf whose `label` (and whose
title, for a nested product or a list) is the literal string
`perMinute`, camelCase and all. That is exactly right for a form a
programmer reads (the field IS the identifier), and exactly wrong for
one an end user reads: a configuration page showing `pagesPerAddress`
and `perMinute` as field labels says nothing a reader would call a
label.

Found from a consumer (okay-watch, BACKLOG `form-labels`): its
configuration page renders several `Form.of`-built forms, and every
one of them shows its schema's own field names verbatim.

Two ways to fix it were on the table: a `Schema` description carried
by the derivation itself, or a map the caller passes at render time.
This lane takes the SECOND — see Decisions for why.

## Interface

```scala
object Form:
  // unchanged, and still the zero-friction default: no labels map,
  // every name is the field name, exactly as today
  def of[A](using s: Schema[A]): Json => Ui
  def ofWith[A](errors: Vector[(String, String)])(using s: Schema[A]): Json => Ui

  // NEW: an optional label per field, keyed by its DOTTED PATH
  // (`addr.city`) or, failing that, its BARE field name (`city`) —
  // the same two-level lookup `render` already does for nothing else,
  // so one entry covers a name repeated at several depths and a more
  // specific entry can still override one occurrence of it
  def of[A](labels: Map[String, String])(using s: Schema[A]): Json => Ui
  def ofWith[A](errors: Vector[(String, String)], labels: Map[String, String])
               (using s: Schema[A]): Json => Ui

  def render[A](s: Schema[A], value: Json, errors: Vector[(String, String)],
                prefix: String, labels: Map[String, String] = Map.empty): Ui
```

`render` gains `labels` as a fifth, DEFAULTED parameter — every
existing positional call (four arguments) keeps compiling and keeps
its current behavior unchanged. `of` gains an OVERLOAD rather than a
parameter on the existing zero-arg method: `of[A]` today has no
explicit parameter list at all (only the `using`), and every call
site in this monorepo uses it bare (`Form.of[Order]`, relying on
eta-expansion to `Json => Ui`) or applies the returned function
directly (`Form.of[Payment](value)`). Adding an explicit parameter
there — even a defaulted one — would turn every bare `Form.of[A]`
into a partially-applied `Map[String, String] => (Json => Ui)`
instead, breaking every caller. An overload keyed on a second
parameter list's PRESENCE, not a default within the existing one,
is the only non-breaking shape.

## Behavior

- [x] a field with no entry in `labels` (or no `labels` argument at
      all) renders EXACTLY as `Form.of[A]` does today — the field
      name, unchanged; this is the regression bar, since the whole
      point is that nothing already using `Form.of`/`render` moves
- [x] a `labels` entry keyed by a field's DOTTED PATH labels that one
      occurrence only, even when the same bare name recurs elsewhere
      (`addr.city` labels the address's city but not a different
      `city` field two levels away)
- [x] a `labels` entry keyed by a BARE field name labels every
      occurrence of that name that has no more specific dotted entry
- [x] a dotted entry wins over a bare one for the same field, when
      both are given
- [x] the "(optional)" suffix `option` already appends still appends
      to the LOOKED-UP label, not the raw field name, so a labeled
      optional field reads `<label> (optional)`, not `<field>
      (optional)`
- [x] a labeled nested product's own title (the bold `Ui.Text` above
      its fields) uses the lookup too, not only its leaves — a title
      is rendered from the same `RenderEnv.name` a leaf is

## Out of scope

- A `Schema`-level description carried by the derivation (`case class
  Config(perMinute: Int)` gaining an annotation or a description
  argument the macro reads). Rejected for this lane; see Decisions.
- i18n / multiple label sets per locale. `labels` is one flat map;
  a caller wanting more than one language builds more than one map.
- `ofSchema`/`askSchema` (the JSON-Schema-driven dynamic form):
  unaffected. That road already reads a property's own JSON Schema
  object for its shape and could carry a `description` field of its
  own by a completely different route (JSON Schema already HAS one);
  out of scope here because the trigger is the STATIC, derived-Schema
  form, not the elicitation one.

## Decisions

- **A caller-supplied map, not a `Schema` description.** A label
  belongs to the FORM, not to the wire shape: `Schema[A]` also drives
  JSON/CBOR/YAML encoding and JSON-Schema generation for tool specs,
  none of which have ever needed a human label, and giving every
  field of every schema in the monorepo an optional description
  (touching the derivation macro, `SProduct`/`SSum`'s case classes,
  and every hand-written `Schema` instance) is a change many times
  the size of the one consumer that asked for this. A map threaded
  through the one algebra that actually renders text is the smaller,
  reversible move, and nothing stops a later lane from deriving that
  map FROM a `Schema` description if one is ever added — the two are
  not in tension, only one is built now.
- **Keyed by dotted path with a bare-name fallback**, not bare name
  alone. A flat `Map[String, String]` keyed only by bare field name
  cannot distinguish two same-named fields at different depths (an
  address's `city` and a birthplace's `city`); the dotted path
  `render` already computes for every field (`RenderEnv.key`) is the
  precise key, and the bare-name fallback is what makes a single
  `"perMinute" -> "requests per minute"` entry cover a field that
  happens not to repeat, without forcing every caller to spell out
  the full path for the common, non-repeated case.
- **An overload for `of`, a defaulted parameter for `render` and
  `ofWith`.** The difference is not a style choice: `of[A]` has no
  explicit parameter list today, so any change to it that is not a
  brand-new overload breaks the bare, parenthesis-free call every
  existing site makes.

## Results

**Landed 2026-09-19.** `Form.of[A](labels: Map[String, String])` and
`Form.ofWith[A](errors, labels)` overloads, `Form.render`'s new
defaulted `labels` parameter, and `RenderEnv.field` doing the
dotted-then-bare lookup. `TestFormLabels` (7 tests) drives every
Behavior box above against a schema where the same bare field name
(`city`) recurs at two depths through two occurrences of the same
nested type (`Addr`), which is the shape the dotted/bare distinction
exists for. Every PRE-EXISTING `Form`/`okay-script` test (24 in
okay-ui's own Form suites, plus every `Form.of`/`ofWith` caller in
okay-script) passes unchanged — the overload and the defaulted
parameter are additive, never a call-site change. Full monorepo gate:
green, every module.

Trigger: okay-watch's configuration page (BACKLOG `form-labels`),
whose forms show raw field names like `perMinute` and
`pagesPerAddress`. Wiring okay-watch's own call sites to pass a
labels map is that consumer's own change, not this one's.
