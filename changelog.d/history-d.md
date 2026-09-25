## history-d - the benchmark history is a directory, one file per measurement

At the operator's ask: `src/jmh/history.tsv` took every measurement at
its tail, so two lanes measuring in one hour conflicted on its last
lines with the same "keep both" answer every time — the reason
changelog.d and the boards became directories. A measurement is now
`src/jmh/history.d/<UTC yyyy-mm-ddTHHMMSSZ>-<measure>.tsv`, named by
the instant it was recorded and what was measured, holding the rows it
produced in the same eight TAB-separated columns. `scripts/history.sh`
reads the archive and the directory together, oldest first
(`[pattern]`), makes a correctly named file (`new <measure>`) and
checks the shape (`--check`: the name, eight columns, a date first, no
empty file, and the archive's frozen length). `history.tsv` stays as
the archive — its rows have dates and no times, so they are not given
invented instants — and `TestHistoryEntries` (okay-deploy) runs the
check in the gate, so a row appended to it out of habit is refused
with the command to use instead. `./bench.sh history`, the A/B
driver's closing line, AGENTS.md, WORKFLOW.md and docs/benchmarks.md
say where to write.
