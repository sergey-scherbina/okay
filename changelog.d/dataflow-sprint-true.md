## dataflow-sprint-true - the sprint's "what is actually open" named four finished groups

The dataflow entry in `sprint.d/doing/` listed five groups of open
work. Checked against `backlog.d/` file by file: the exactly-once trio,
recovery, coordinator election, rescale and netem are all TICKED. Four
of the five groups were finished, and the one genuinely open item in
them — `dataflow-rescale-windowed` — was named only in passing beside
a closed sibling.

Why this matters more than a tidy-up: a sprint entry is the first thing
the next agent reads, so it is the worst possible place for finished
work to sit. This repository has the failure written down twice already
(`backlog-cleanup`, and `terminal-record-true` earlier today); this is
the third sighting, which is enough to call it the shape rather than
the incident.

Rewritten to the five that are unticked, by value, with what each
actually is: `dataflow-source-log` (its own entry says FIRST, and it is
the only large one verifiable on ONE machine), `dataflow-rescale-windowed`,
`federation-refusal`, `shipped-terms` (a decision, not a task) and
`dataflow-machines` (blocked, and not to be pretended at). The
optimizer box is kept and marked open ON PURPOSE, with the spec's own
reason: the combine is where a keyed stage begins here, so nothing
reaches the exchange uncombined and there is nothing to push.

Boards only.
