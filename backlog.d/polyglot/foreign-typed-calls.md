- [ ] foreign-typed-calls — a Python or R function as a TYPED Scala
      function, both sides through `Schema`: `Py.fn[In, Out]("statistics:median")`,
      `R.fn[In, Out]("stats::median")`, answering `Out ! (PyEval +
      Throws[Condition])`. A case class crosses as a dict (Python) or a
      named list (R), a `Vector[A]` of products as a frame. Today every
      call is built from `PyValue`/`RValue` by hand, and only R has
      `rows[A: Schema]`/`RFrame.of`; Python frames get the same pair
      here. The decode failure is a `Condition` naming the field, as
      `RFrame.rowsOf` already does.
