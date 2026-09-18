## source-log-closed - the stage was built a day before the entry asking for it

`dataflow-source-log` said FIRST and described stage 11 — `Flow.topic`
seeking by epoch, and a sink whose append is the commit. Checked
against the files before writing a line of it: the stage is BUILT.
`TestSeek` asserts a resumed job opens at the journal's epoch and reads
`total - Σpositions`; `TestStagingTopic` has the topic writer;
`TestStaged` kills the coordinator between the append and the journal
commit at four epochs and finds every pane in the output once, with the
dedup state being the OUTPUT read back from the tail. The windowed half
was MEASURED (`dataflow-windowed-seek`) and BUILT (`dataflow-horizon-seek`,
dbfeb48e) the same morning, by a sibling, under a different name.

What the entry asked for and deliberately did not get is
`Sink.stagingTo(topic)` as a METHOD: okay-cluster depends on
okay-persist in test scope only, on purpose, so a `stagingTo` would
drag a store into a compile graph that stops at okay-codec. The spec
marks that box `[~]` with the reason. What is genuinely left of stage
11 is one box that needs a BROKER, not a decision.

**AND A RULE, because this is the fourth sighting in one day.**
`ui-terminal-v2` demanded a fix that had landed an hour earlier; the
dataflow sprint entry named five groups of open work of which four were
finished; this entry named a stage built that morning;
`optic-law-rewrites` was cited as filed in two places and filed in
neither. AGENTS.md now says what to do about it: when a lane lands,
grep the boards for the AREA you touched — not your own slug, which is
the one name certainly current — and read every entry naming a file, a
type or a spec box your lane moved. It is one grep and a minute, and
the alternative is a reader who stops and audits instead of working,
which is what happened four times today.
