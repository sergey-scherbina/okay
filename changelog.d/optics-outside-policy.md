## optics-outside-policy - a projection policy, declared once, audited without a document

The operator lifted the wait ("Все это нужно", 2026-09-23). The seat
the entry had waited for is gone from this tree (okay-leads has no
sources) and the incident that priced its absence lives in a private
repository, so the policy is a library over what every record here
has: a `Schema` and its `Json`.

- okay-codec `Policy[A]`: `Policy.hide[A]("customer.email",
  "lines.price")` — dotted keys checked against the schema by NAME at
  construction (`Either`), a segment through a list applying to every
  element, through a sum to the case that has it. `touches` is the
  DESCRIBE interpreter (no document); `project`, `redact`,
  `optic(key)`, `text(a)` are the RUN interpreters.
- THE LAW (`TestPolicy`, 6): the keys `project` removes are exactly
  `touches`, restricted to what the document has — on the full record,
  the other sum case, an absent `Option` (written `null`, so present).
- Decisions: reified keys with optics DERIVED (a profunctor optic
  cannot be asked what it looks at); one optic per key (two
  traversals compose only through a bind — a first draft's cast to
  fake it was refused); `Either` at construction; over `Json`, with
  `text` as the typed door.
- specs/optics-outside.md stage 7; docs/optics.md §6; the okay-codec
  module page.

Gate `affected master` green, no warnings.
