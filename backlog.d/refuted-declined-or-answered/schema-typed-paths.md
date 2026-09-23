- ANSWERED (typed-path-key, 2026-09-23): `Schema.path[A].field("address")
  .field("city")` — the chain by field NAME with intermediate types
  inferred and the name checked against the Mirror — is
  `TypedZipper(a).field("address").field("city")` (specs/zipper.md
  stage 2), `asAffine` gives it back as an optic on `A` (stage 3),
  and `pathKey` gives it as the dotted key a form routes by (stage 5),
  so `Form.drillAt(cursor)` opens a drill-down where the type said.
  What the entry filed as its cost question stays open under its own
  name: `optics-field-fuse` — the planner cannot read the by-name
  constructor, so a `field` chain pays the interpreter today.
