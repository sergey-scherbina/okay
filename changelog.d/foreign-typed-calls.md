## foreign-typed-calls - Python and R functions as typed Scala functions

Stage 2 of specs/foreign-highlevel.md.

- `Py.fn[Out]("mod:name")(args...)` and `R.fn[Out]("pkg::name")(args...)`
  answer `Either[Condition, Out] ! PyEval` / `! REval`. Arguments and
  answer go through `Schema` with the new `PyCodec` and `RCodec`, typed
  folds over the GADT. A case class is a dict or a named list, and a sum
  names its case in `type`.
- Python frames gain `PyFrame.of`/`rows[A]`, the pair R already had.
- A decode failure names its path.

Fixed on the way, each shown failing first:

- A Python dict answered by a call was sent as a frame. It failed in
  the shim or reached okay as None. Wire v2 (shim 2) adds `PyValue.Dict`.
- The same for an R named list. Wire v3 (shim 3) adds `RValue.Named`.
- A Long past 2^53 lost digits on the way to Python.
- The R frame codec truncated a Long to 32 bits without any error.

`PyEval` and `REval` are covariant now, so they can be used in an okay
program at all. Tests: 12 new unit tests in the default gate, and 13
live ones (python3, and R in docker). Docs: "Typed calls" in
docs/modules/okay-py.md and docs/modules/okay-r.md.
