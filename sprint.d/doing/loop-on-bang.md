- [~] loop-on-bang — specs/fold-until.md stage 2: `!.loop(s)(f: S =>
      Either[S, A] ! F): A ! F`, `tailRecM` for programs, stack-safe by
      `flatMap`'s laziness (the recursive call sits inside a Bind, made
      when the interpreter resumes it). In `object !` beside `tailcall`
      — NOT a top-level `loop`: Generate.scala's `loop(f)(a)` has the
      same two-list shape and the overload would be ambiguous. The
      trigger is the proof: rewrite `Toolkit`'s four dialog loops
      (okay-ui, `def loop` + trailing `loop(z)` each) over it and count
      the lines; Actor.scala:287 and Dialog.run are NOT candidates — the
      `Either` from `Async.attempt`/`Writer.uncons` would sit inside the
      loop's own `Either` and read as the opposite of what it means
      (record that as a Decision). Laws: a 1M-iteration counter on the
      default stack; a loop over `State` sees each step's state; the
      `TestToolkit` suite unchanged and green.
