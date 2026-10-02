## format-cheap-decline - Format's dialects decline by their first character

- `Format.json` needs `{`/`[`, `Format.xml` `<`, `Format.yaml` anything but
  `{`, `[`, `<?`, `<!` — checked on the first non-blank character BEFORE
  the total parser runs. Necessary conditions: the verdicts are the full
  parse's, the reasons sooner (`begins with '<', not { or [`). Text before
  an XML root element is now declined.
- Measured in okay-fin (Throughput, 801 FpML + 726 CDM files): a
  declining dialect read the whole document first — JSON 166 ms + YAML
  112 of detection's 458 on XML, XML 244 + YAML 129 of 468 on JSON.
