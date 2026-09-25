## stack-safety-catch-up - ContMacro.rewrite's bound measured and written down; no UNAUDITED row left in okay

- `ContMacro.rewrite` recurses once per level of a user's tail-shaped
  `shift` body, at compile time. Decision 2 of specs/stack-safety.md
  called the compiler's own recursion a borrowed bound. It is measured
  now, by a new probe (`scripts/probe-contmacro-depth.sh <n>`):
  - 2 075 levels compile, and the macro rewrote them (`Cont$.tailShift`
    in the bytecode);
  - 2 150 to 4 200 overflow in PostTyper, 4 800 in Typer, 10 000 in the
    parser. All three run before the Inlining phase where the macro
    expands, and no trace holds a ContMacro frame.
- The row is BOUNDED with that evidence. A future Scala re-runs the
  probe; a ContMacro frame in an overflow is the signal to convert the
  walk.
