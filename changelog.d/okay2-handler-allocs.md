## okay2-handler-allocs - okay2's handler loops allocate nothing per operation for the split

`Split.at[F]` — the split as a NAME-BASED PATTERN (a value class with
`isEmpty`/`get`, the one cast still inside `Split`): a loop makes it
once per run and matches `case Bind(Inject(Mine(op)), k) =>`, so the
handled arm is the loop's own tail call. `Split.split` handed each step
back through two closures, a `Tuple2` and an `Either`. Rewritten on it:
`State.handleAt`, `Effects.relay`, `Effects.handleWith`,
`Reader.runAt`, `Writer.loopWith`/`foldUntilAt` (at the caller's
`TypeableK`, so `Writer.byValue` still reaches them), `Once.runAt`,
`Resource.runAt`, `Refs.handle`. Measured A/B against master on the
okay2-bench lanes (history.tsv `okay2-handler-allocs-*`): stateEffect
0.66x the time and 0.61x the bytes — 88 B per run from the Scala 3
core's, 1.24x its time where it was 1.83x; relayPrebuilt 0.75x, now
0.98x the Scala 3 core; handlePrebuilt 0.82-0.97x (a noisy base arm),
1.03x the Scala 3 core. All 753 okay2 JVM tests green; backlog item
closed; docs §8 states the new ratios.
