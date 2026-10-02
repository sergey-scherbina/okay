## okay-macros-package-satellites - the satellites' macro implementations moved into okay.macros; okay-macros-package closed

Stage 3, the last stage, of okay-macros-package (operator: "Может быть
перенесем макросы в пакет okay.macros", i.e. move the macros into the
okay.macros package). After stages 1 (the core) and 2 (Handler), every
module whose code lives in package `okay` now keeps its macro
implementations in `okay.macros`. Each is `@publicInBinary
private[okay]`, and the public `inline def`s splice
`okay.macros.X.impl`:
- **okay-direct:** `stagedImpl`, `blockBody`, `directImpl`,
  `applicativeOnly`, `stripped` and `tpe2A` moved out of `object Direct`
  into `DirectMacros`, beside the existing `macros/` compiler. Direct.scala
  keeps only the API.
- **okay-optics:** `Fuse`'s planner and emitters (`setImpl`, `modifyImpl`,
  and the seven `Optic` splices: `setFnImpl` … `traverseOfImpl`) moved
  into `FuseMacros`, and `Focus.impl` into `FocusMacros`.
- **okay-workflow:** `ProcMacro` moved (git mv) to `macros/`. It had been
  public only to avoid E192, and is now private to okay.

18 inventory rows were re-filed, each matched by its own source line.
It is a move only: no behaviour change.

Modules with packages of their own (okay.codec, okay.js, okay.refine,
okay.staging, okay.deploy) keep their macros where they are: the ask was
okay's own package.

Tests: okay-direct, okay-optics, okay-workflow (JVM); every dependent
compiled.
