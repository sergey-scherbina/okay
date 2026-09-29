## refine-stage2 - a Schema is a pattern, a path is a conversion, a pattern is a search

- `Refine.schema[A](name)(using Schema[A]): Refine[Json, A]` — a derived
  Schema as a pattern: decode declines in the codec's own words, encode
  writes (through the text encoder, since the codec has no `A => Json`).
- `Format.value: Refine[Doc, Json]` — a detected JSON or YAML document
  projected to the one `Json` value; XML and CBOR decline saying so. Its
  write renders JSON, so `Format.detect andThen Format.value andThen
  Refine.schema[Swap]("swap")` reads a YAML swap and writes it back as
  JSON — the conversion the prisms promised.
- `r.search(a): B ! Choose` — a pattern as a search: Took one answer,
  Unclear a choice point, Declined an empty one; `runChoice` lists the
  readings, `Logic.ifte` writes the soft cut.
- Decided away: a registry type (an `Or` is flat, `Refine.first` is the
  registry). Deferred: the `Judge` seam, until a `cut`-ing consumer and a
  second orderer exist. Docs and pinned snippet updated.
