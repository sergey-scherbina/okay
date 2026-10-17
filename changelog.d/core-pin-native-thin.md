## core-pin-native-thin — okay-core pinned at f62b79d: control's own $ and shift0 on the thin path

okay-core puts the machine's own `$`/`shift0` (what `Cps`/`Carrier` and okay-freer's classic run on) on the thin
path of the std delimiter: `Shift0K`, a shift0 and its rest (its maps fused, no frame), and eval's fast case for a
`$` resumed and bound with no frames — native shallow capture 0.285 → 0.105 µs. The two faces stay two types (a `$`
moves the answer, a handler's level has none); one path. okay-std's lanes, c1f75fc → f62b79d, `jmh-lane.sh -f 2`:
BuildShapeBenchmark.stateFoldM 16.15 → 16.25 µs, rowFoldM 18.53 → 18.70, HandlerBenchmark.handleForward 135.3 →
134.3 — the same. The whole build compiled with no warnings (266 module compiles); okay, okay-std, okay-async,
okay-stream, okay-tx, okay-durable and okay-freer pass on the JVM, okay and okay-std on JS, okay, okay-std and okay-stm
on Native; okay-cats 328 of 329, the one its onCancel law (backlog cats-js-oncancel-law).
