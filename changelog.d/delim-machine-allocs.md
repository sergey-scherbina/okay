## delim-machine-allocs - the Delim machine stops allocating for type facts: a capture 902 -> 726 B, a push 358 -> 278 B

Three per-operation allocations in `Delim`'s machine that carried no
information, each its own commit with its own three-round alternating
`-prof gc` A/B on DelimBenchmark (specs/delimited-control.md, "Machine
allocations"; history.d `delim-machine-allocs`):

- (1) `step` answers a `Step` — `Next`, or `Out` for the finished
  program — instead of `Either`: no `Right` per Delim operation
  (-16 B per push, generator 0.95x). The first cut fed the `Step` back
  into `loop` and cost writerTellUnderDelim 4% in all three rounds;
  the landed shape reads it at the call site.
- (2) THE PRIZE: `Push`/`Dollar`/`Watched` no longer add an identity
  `K` frame whose only job was to retype the prompt's answer to the
  operation's. The delimiter frame carries it as a witness
  (`r <:< X`, `<:<.refl`, lifted with `liftCo` — no cast): -64 B per
  push, -128 B per capture, delimPushOnly 0.79x, delimGenerator 0.83x.
- (3) a foreign operation resumes at its head bind's `f(x)`, not at a
  `pure` the loop pops a step later: writerTellUnderDelim -40 B/op,
  time unmoved. REFUTED, three shapes of skipping the `K` allocation
  for a foreign op under a bind (-72 B, 0.87x there): each made the
  Delim-only lanes 2-5% slower with no byte changed.

Also a data lane, `DelimDepthBenchmark.delimCaptureDepth`: a level the
machine holds as frames costs ~425 B / ~42 ns per capture and
~225 B / ~21 ns per further call of k; a level of plain maps ~137 B and
~49 B. The slope is filed on continuations-as-data-spike and in
docs/continuations/20-the-costs-measured.md.

Commits: spec fc257fb81, (1) cd8251b95, (2) 447524bbc, (3) 55ee861d7.
