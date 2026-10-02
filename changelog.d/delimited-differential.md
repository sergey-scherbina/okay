## delimited-differential — the reference as an oracle for the machine

Operator ask, 2026-10-02. TestDelimitedDifferential generates programs
over `Delimited` (three prompts, `reset`/`dollar`, `shift`/`shift0`/
`abort`, bodies resuming `k` once, twice, never, then continuing, or
resuming with a computation) and runs each on the frame machine and on
DelimitedReference, outcomes compared, `NoPrompt` included: 4 500
programs a run, on JVM, Scala.js and Native. A type-correct mutant of the
machine's capture fast path (prompt unchecked) passes the hand-written
laws and the depth suite and fails all three differential sets.
specs/cont-js-depth.md.
