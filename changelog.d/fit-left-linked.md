## fit-left-linked — a left spine fitted with no list

`Run.fitLeft` fits a left-nested chain of binds deeper than `Run.LeftDepth`. It
used to keep each continuation in an `ArrayList`, whose array grew and was
copied on the way down, and then bind them back from the innermost one. Now
each kept continuation is a node of a chain of its own (`Kept`: `KeptBind` is
itself the kernel bind's continuation, `KeptMap` is the map's), innermost first,
in the same order and with the same number of objects as before, but no list.
okay-std's `HandlerBenchmark` in an interleaved A/B: `stateEffect` 27.3 →
25.6 µs (−6%), `writerTell` −3%, `stateSmall` unchanged. On the profile,
`ArrayList.add` was 3.5% of the CPU and `Object[]` 4.7% of the bytes. The gate
also found `TestCatsEffectLaws`' onCancel law failing on the JVM on master;
that is recorded in its backlog item and is not this lane's.
