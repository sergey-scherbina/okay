## merge-wait-docs — one user page for the Merge / Wait / Pause givens: every strategy, who asks the wait, what each rung costs, when to pick which

The API landed by ready-merge-chunk-forward's second landing and
drive-poll-then-park — a `merge` decided by three givens — was
described only inside docs/guide.md §6's streams narrative, so a
reader could not be sent to it. docs/merge-and-wait.md is that page:
the three-row table of what each given decides and its default;
`Merge` with its three joins and the four `Source` spellings that
dispatch to them, `Ready` against `Shared` with the measured ratios
(0.73-0.93x elementwise, 1.03x chunked, 1.12x flushing in one round —
the open item named); `Wait` with `Register`, `Spin`, `Ladder` and
`Cycle` as a table of what and when, the rung costs measured on this
box (yield 125 ns, parkNanos 10-12 us, the ~50-chunk window), the
ladder-against-cycle numbers, and a strategy of your own in one
method; `Pause` with the platform's rungs, JS's `threads = false`, and
a counting one that shows the default ladder's 100/50/4/1; who asks
the wait (`ReadyMerge`, the blocking runner, the callback drive's one
poll); a Choosing section; and the literature (Disruptor's
WaitStrategy, Karlin–Manasse–McGeoch–Owicki, Rust's `select_all`,
Kiselyov). Every Scala line of the page is pinned —
`TestDocExamplesMergeWait` (okay-stream, JVM: 4 tests, the counting
`Pause` asserting 100/50/4/1) for the examples, the library sources
verbatim for the signatures — under `TestDocSnippets`. Linked from
docs/README.md and from the guide's own paragraph. Docs-only lane.
