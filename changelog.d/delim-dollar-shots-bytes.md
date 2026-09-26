## delim-dollar-shots-bytes - a plain `$` no longer pays for the guard it does not use

- `Delim.dollarResumed` makes its own nodes, `Delim.Watched` for the
  operation and `Segs.Watch` for the machine frame, each with its
  `Shots` count. The plain `Delim.Dollar` and `Segs.Ret` carry no count
  field and the machine does no null test on them.
- The watched cases are matched last in the walk and the machine step,
  so neither the plain dollar nor a capture passing binds pays a type
  test for them. A capture still takes a fresh count per captured
  context, as before.
- Measured, two alternating rounds against master: delimDollarOnly
  388 008 -> 372 008 B/op, exact, and 24.75 -> 23.56 us.
  delimGenerator and stateLexTail are unchanged in bytes.
- No behaviour changed: the dollar, layered, lexical and stacked suites
  are green unchanged. specs/shift0-dollar.md records the design and
  the numbers.
