## shift-stacked-key — the keyed reset runs on Scala.js, and nested resets take no stack

A machine run outermost is a value now: `Shift.run` (so every door, the
keyed `reset` included) answers `Delay(Frames.Own(program))`. A running
machine steps into it in its own loop, as it already did `Frames.Resume`;
any other interpreter forces it once. The keyed `reset`'s `ThreadLocal`
room (`ResetRoom`/`runReset`, which switched stacks past ~300 levels) is
gone: the keyed `reset` did not LINK on Scala.js (`ThreadLocal.withInitial`)
and now runs there, and 100 000 nested resets run on a 128 KB JVM stack
with no stack switch (TestResetDepth, cross — JVM, JS, Native;
TestResetSmallStack). Measured: `shift0_seq` 1.00x, 100 outermost resets
an op 1.18x (a `Delay` and an `Own` per run). specs/shift-merge.md,
specs/shift-effect.md (the room superseded).
