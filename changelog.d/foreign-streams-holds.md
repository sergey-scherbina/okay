## foreign-streams-holds - Stateful[M] and Models[M]: a stage with state per partition, a model per interpreter

specs/foreign-map-reduce.md stage 4, the operator's "Продолжай стриминг и
холдс" — two more extensions of the engine typeclass, each optional,
NAMED for what they are since foreign-facade took `Streams` (a stateless
flow of frames) and `Holds` (a handle on one worker) meanwhile.

- `Stateful[M]` / `flow.statefulIn[B](module, "open", "step", "finish")`:
  `open` makes the partition's state on the far side, `step(frame,
  state)` folds each chunk, `finish` flushes; the state lives in ONE
  interpreter leased for the partition (`Pool.lease`), a death loses it
  and the partition recomputes elsewhere — no chunk retry. An empty
  partition still opens and finishes.
- `Models[M]` / `Model.in(module, "fit", params)` /
  `flow.mapModel[B](model, "scale")`: a model fit once from its parameters
  and passed as every chunk's second argument, materialised once per
  interpreter of the pool (a `WeakHashMap` of refs per worker) since a
  model lives in one process; on the JVM made once.
- Python, R and the JVM (`JvmModule.stream/model/mapWith`) have both.
  Proven with one job text each: `TestStatefulModels` (5, default gate —
  the JVM over 3 workers; opened and finished once per partition, the
  finals summing to the total; compile errors for a module without them;
  missing names refused at build), `TestPyStatefulModels` (2, Live, run
  here) and `TestRStatefulModels` (2, Live, run here in the container).
- FOUND: a test counting opens from four partition threads with a
  `@volatile var` lost an update — partitions are threads.
