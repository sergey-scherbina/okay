## foreign-engine-typeclass - one API over the polyglot map-reduce: Engine[M], Reduces[M], JvmModule

specs/foreign-map-reduce.md stage 3, the operator's "один и тот же код
работал со всем этим прозрачно через имплиситы и тайпклассы", refined
mid-lane to "базовые методы плюс расширения в отдельных классах типов,
каждое опционально, а фасад собирает".

- `Engine[-M]` (the base: map) and `Reduces[-M]` (an extension: reduce),
  typeclasses by the MODULE's type, each instance optional — Python,
  R and the JVM (`JvmModule`: Scala/Clojure/Frege functions by name)
  have both; a test's own module type has whichever it gives.
- `flow.mapIn[B](module, fn)` and `Reduce.in[A, Acc](module, step, merge)`
  ask for `Engine[module.type]` / `Reduces[module.type]`: contravariance
  makes the module's type unnamed at the call, and a reduce on a module
  without `Reduces` a compile error while its map still works.
- The interpreter is a given (`Engine.py(path)`, `Reduces.py(path)`);
  `mapPy`/`mapR`/`Reduce.py`/`Reduce.r` stay as the named cases.
- Proven with ONE job text, `StatsJob[M]`: `TestEngine` (5, default gate:
  the JVM over 3 workers, a fake engine, base-only maps and cannot
  reduce by `compileErrors`, a missing name refused at build, a JVM
  exception a considered refusal), `TestPyEngine` (Live, run here) and
  `TestREngine` (Live, run here in the container) — the same text, a
  `PyModule` and an `RModule`.
