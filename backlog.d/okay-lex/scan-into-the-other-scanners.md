- [ ] scan-into-the-other-scanners — `Yaml`, `Xml` and okay-rag's
      `Code` scanner still answer the pair, so they still pay the
      `Tuple2` per character and a `Vector` per token; the default
      `stepInto` keeps them correct, not fast. `Markdown` is done
      (2026-09-19, changelog.d/scan-into-the-other-scanners-markdown.md):
      extends `ScanInto`, writes `stepInto` directly, min 20.4 vs
      29.9 μs/op measured (`compare/…/TextBenchmark.scala`,
      `lexMarkdownElementwise`) — the shape to repeat for the rest.
      `Code`/`Yaml` recurse into their own `step` — the conversion is
      real work, not a rename, for those two specifically. okay-rag's
      code chunker over a real file is the honest workload for `Code`.
