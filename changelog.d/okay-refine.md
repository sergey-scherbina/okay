## okay-refine - reading anything out of anything: patterns as prisms in a typed hierarchy, the format level first

- New module `okay-refine` (JVM/JS/Native; specs/refine.md, stage 1).
  `Refine[A, B]` is a pattern: a prism whose `read` may decline with a
  reason and whose `write` is total; `andThen` is a path (reads AND
  writes back), `<|>` a choice, `map` an iso, `widen` into a sum. A run
  answers a `Verdict` — `Took(value, path, declined)`, `Unclear(candidates,
  declined)`, `Declined(tried)` — with every `Refusal` by path and in the
  pattern's own words; every alternative runs, so two takers are
  `Unclear`, never first-wins. A step's `.prism` is the optics prism, so
  its laws apply.
- `Format.detect: Refine[Array[Byte], Doc]` — cbor, and UTF-8 text then
  json / xml / yaml, each decided over the codec's OWN lossless tree (no
  error node, structure at the root), `write` the dialect's `render`, so
  a detected document writes back to its bytes. A bare scalar is no
  document; a damaged one is declined in the tree's words.
- Found on the way, recorded in the spec's Results: the block YAML
  dialect took every JSON object (a root scalar is the tell, declined
  now); the XML dialect reads `<?xml …?>` as an unclosed tag — backlog
  `xml-processing-instruction`, pinned by a KNOWN GAP test, needed
  before the FpML prover of stage 2.
- Docs: docs/modules/okay-refine.md (snippets pinned by
  TestDocExamplesRefine), README, docs index row; two rows in
  specs/stack-safety-okay.tsv (`run`/`write`, bounded by the authored
  pattern tree's depth).
- Decided with the operator: the domain (FpML, ISDA CDM) lives in a
  private repository; okay keeps the mechanism and one public prover.
