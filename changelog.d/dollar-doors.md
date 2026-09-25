## dollar-doors - `$` at the evidence level, control to a dollar a compile error, shift0 in direct blocks

The shift0/$ arc (specs/shift0-dollar.md) left three doors at the
raw-prompt level or at run time. Closed 2026-09-25, plus one value
pinned.

- `Delim.dollar(ret) { body }`: `ret $ body` at a fresh delimiter with
  `Prompted[R]` in scope for the body, the same word as the primitive
  told apart by its first clause (a prompt there is the primitive, a
  return function here is this one — the `shift` rule). Installs only,
  like `scope`.
- `Delim.Stacked.control` requires `Plain[p.type]`, and only a `Reset`
  (what `reset` and `delimited` hand their body) provides it. A
  `dollar`, a `Layered` layer and a `Lexical` instance hand a bare `In`,
  so a control-capture to any of them no longer compiles, with the
  reason in the message; the machine used to throw
  `UnsupportedOperationException`. `control` to a reset is pinned as
  allowed (102, by hand).
- `!Delim.shift0[A](k => …)` inside a direct block, one type argument,
  the inline mirror of `!Delim.shift[A]` (TestDirectShift0, 2).
- `abort` to a dollar skips `ret` ("gone", where the flatMap encoding
  gives "<gone>"), pinned as a value in TestDollar and said in
  docs/continuations/11-four-captures.md, with the two new doors.
- TestDollar 13, TestStackedShift0 10, TestDirectShift0 2; okay-direct's
  Delim suites unchanged.
