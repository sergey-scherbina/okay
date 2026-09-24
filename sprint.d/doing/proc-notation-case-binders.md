- [ ] proc-notation-case-binders — a `Proc.direct` block cannot use a name
      bound by a `case` pattern in a later question: `x match { case
      Right(p) => !total(p) }` fails with "a reference to value p was used
      outside the scope where it was defined" while the macro expands. An
      `if` over a `val` works (`if e.isRight then !total(e.getOrElse(0))`),
      which is what foreign-workflow stage 2's block had to write. Found by
      foreign-in-durable-workflow (2026-09-24). A `match` over an Either
      is the natural way to branch on an activity's answer, so a pattern's
      binders should join the environment the way a `val`'s do (Paterson's
      `case` in proc notation binds exactly this).
