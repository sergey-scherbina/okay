- [ ] format-cheap-decline — `Format.detect` runs every text dialect's
      TOTAL parser to the end of the document before declining: measured
      in okay-fin (Throughput, 2026-09-29, load ~15), on 801 FpML XML
      files json 166 ms + yaml 112 of detect's 458; on 726 CDM JSON files
      xml 244 + yaml 129 of 468. A necessary condition first, on the
      first non-blank character (BOM skipped): JSON `{`/`[`, XML `<`, the
      block YAML dialect not `{`, `[`, `<?`, `<!`. Behaviour: XML now
      declines text before its root element (not well-formed XML anyway).
      Gate: TestFormat (new cases) + `affected master staged`. (2026-09-29)
