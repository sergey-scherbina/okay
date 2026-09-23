## scan-into-the-other-scanners (Yaml, Code) - the last two, closing the entry

`Markdown` and `Xml` converted from `Scan` to `ScanInto` mechanically
(2026-09-19): neither `step` recurses into itself. `Yaml.scan` and
okay-rag's `Code.scanner` do — `PendingDash`/`PendingColon` falling
through to Plain-mode processing of the SAME character, `Quoting`'s
empty-string fall-through, `Pending`'s two-char-marker-miss
fall-through — so the conversion is real work: every recursive
`step(...)` call became a `stepInto(...)` call writing into the same
`Growable` sink, with the exact order the original pair built
preserved by hand at each site (a token this branch owns, THEN
whatever the recursive call emits).

One correctness trap caught before landing: `Code`'s Base/InIdent/InWs
branch computed a `closing` vector unconditionally in the original,
but only EMITTED it on some sub-branches — the two same-mode `keep`
continuations (still InWs, still InIdent) discarded it on purpose, so
as not to flush a token still being built. A first pass called
`flushedInto` unconditionally before the match; fixed to call it only
where the original tuple actually used `closing`.

`TestCodec`/`TestLaws` (26 tests, both scanners) and the full
`okayRagJVM` suite (73 tests, incl. `TestCode`, `TestLanguages`)
unchanged. Neither driver (`Yaml.cst`, `Code.parse`/`parseFile`/
`source`, and `compare`'s own `Scan.all`) needed a change — all
already called `.stepInto`.

Measured (`compare/src/jmh/scala/okay/TextBenchmark.scala`, new
`lexYamlElementwise`/`lexCodeElementwise` lanes, `git stash` A/B on
each scanner's own file, same box, same session, `-prof gc`):

| scanner | before | after | ratio |
|---|---|---|---|
| `Yaml` (block map/seq, quoted scalars, comments) | 26.670 µs / 258 264 B | 18.882 µs / 196 912 B | 1.41x / 1.31x |
| `Code` (`Language.scala`: doc comment, strings incl. triple-quoted, line/block comments, nested braces — the honest workload the entry asked for) | 87.981 µs / 660 353 B | 64.596 µs / 446 695 B | 1.36x / 1.48x |

All four scanners this backlog entry named (`Markdown`, `Xml`, `Yaml`,
`Code`) are on `ScanInto` now; the entry is closed
(`BACKLOG-ARCHIVE.md`).
