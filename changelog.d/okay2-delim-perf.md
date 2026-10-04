## okay2-delim-perf - okay2's Shift machine catches up with the Scala 3 core: a push 0.81x, a capture 0.80x, a dollar resume 0.71x

The four okay2 perf twins, done in one lane. Operator: "Делай то что
нужно для okay2 только сразу всё" (do what okay2 needs, all of it at
once).

**The benchmark.** okay2 had no `DelimBenchmark`. Its four lanes are now
in okay2's jmh sources: `delimGenerator`, `delimPushOnly`,
`delimDollarOnly` and `delimDollarResume`.

**Three changes to okay2's `Shift` machine,** each a twin of a Scala 3
core change, each with its own A/B against master. All have no cast.
- **(1) A watched dollar is its own operation and frame** (`Watched`,
  `Segs.Watch`, matched last). The plain `Dollar` and `Segs.Ret` carry no
  `Shots` field, so they need no null test. -16 B a dollar.
- **(2) A capture copies its prefix in one pass.** Captured copies are
  their own `Frames` type, with once-set holes read only by `reify`, and
  `copy` is one typed `@tailrec` loop; scalac 2 accepted it as written. A
  dollar closes the copy with a copy of its own frame (`AtDollar`, one
  chain). `Wrap`, `On`, `Frame`, `Unwound`, `Walk` and `AtRet` are gone.
  -104 B a capture.
- **(3) The delimiter frames carry the prompt's answer to the
  operation's,** as `up: X <:< Y`. This replaces the identity `Segs.K`
  under every `Push`, `Dollar` and `Watched`, with plain 2.13 `<:<`
  algebra and `liftCo`. -64 B a push.

**Measured, all three together, against master** (history.d
okay2-delim-perf):

| lane | ratio | bytes |
|---|---|---|
| delimPushOnly | 0.81x | 334 -> 270 KB |
| delimGenerator | 0.80x | 1094 -> 862 KB |
| delimDollarResume | 0.71-0.73x | 1108 -> 844 KB |
| delimDollarOnly | 0.82x | 364 -> 292 KB |

**Stage 45 priced** (okay2-split-at-rest-measure), against its parent:
delimShift 0.98x, msplitObserve 0.99x, but produceFold 1.09x and
writerMap 1.05x slower. That is filed as
okay2-split-at-rest-regressions.

Tests: okay2's delimited-control suites (169), then okay2's whole suite.
