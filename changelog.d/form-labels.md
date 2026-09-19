## form-labels - a field's label, stated by the caller

`Form.render` labeled every field by its schema field name — a
consuming form showed `perMinute` and `pagesPerAddress` where a
reader wants a label. `Form.of` and `Form.ofWith` gain overloads
taking a `Map[String, String]`, keyed by a field's dotted path
(`addr.city`) or, failing a more specific entry, its bare field name
(`city`) — the same two-level fallback covers a name repeated at
several depths while still letting one occurrence be overridden on
its own. `Form.render` carries the map through as a new, defaulted
fifth parameter.

Both are additive: `of[A]` had no explicit parameter list at all, so
a new PARAMETER there (even defaulted) would have broken every bare
`Form.of[Order]` call site in the monorepo by turning it into a
partially-applied function. The overload is the only non-breaking
shape; every existing `Form`/`okay-script` caller is unchanged and
every existing test passes untouched.

`TestFormLabels` (okay-ui, 7 tests) drives the lookup, the fallback,
the override, and the "(optional)" suffix and a nested product's own
title both reading the looked-up label, not the raw field name.
