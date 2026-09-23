- [ ] polyglot-schema-stubs — the seam is typed on ONE side only. A
      value from Clojure/Frege/Python/R is checked at run time against a
      `ClassTag` or an `RFrame` column type and refused by name; the
      other language never sees the okay type at all. Generate its
      declaration from the same `Schema` the codecs fold: a Python
      `dataclass` + `.pyi`, a TypeScript `.d.ts` (with
      polyglot-typescript), a Clojure malli schema, a Frege `data` with
      its natives. One `Schema.fold` algebra per language, the way
      okay-openapi derives JSON Schema. Then a renamed field fails the
      OTHER side's type checker, not a run.
