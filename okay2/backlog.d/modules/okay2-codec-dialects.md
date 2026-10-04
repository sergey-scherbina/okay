- [ ] okay2-codec-dialects: the rest of okay-codec on okay2 (operator,
      2026-10-04: "Делай то что нужно для okay2 только сразу всё").
      Landed so far: `Schema` and JSON (stage 41), XML (stage 43),
      `JsonSchema` (with okay2-http-routes, stage 44B), `Columns`
      (okay2-spark-columns), and `Cbor`, `Validate`, `Digest`/`Compat` and
      the `Codecs` provider registry (okay2-codec-cbor, stage 54). Still
      unported: `Edn`, `Yaml` and `Markdown` (the next lane), then the
      staged codecs (`Staged.json`/`cbor`/`strict`, a Scala 2 macro where
      Scala 3 uses quotes).
