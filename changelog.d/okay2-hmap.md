## okay2-hmap - HMap, a map whose type lists its entries, for the Scala 2 core

The Scala 3 core's static heterogeneous map in okay2 (spec stage 20,
docs §24): the type lists the entries, `get` is resolved by the
compiler, a key the map does not hold is a compile error with its own
message, and nothing casts. Scala 3's tuple of `(key.type, V)` pairs is
a type-level list here (`HMap.Cons`/`HMap.Nil`), held at run time as
that same list; `Select` is derived by induction over it, the tail case
at lower priority so a key added twice resolves to the newer entry
instead of an ambiguity. TestHMap mirrors the Scala 3 suite. Planned as
okay2-eager-hmap; Eager landed first by a sibling (0f9274f4), and the
duplicate was dropped unlanded.
