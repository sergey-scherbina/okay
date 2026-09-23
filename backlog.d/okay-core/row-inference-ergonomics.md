- [ ] row-inference-ergonomics — PRIORITY: LOW-MEDIUM. A day's worth
      of core code spelled row unions by hand where inference should
      have: `push[R, F](…)`/`run[R, F](…)` inside `Delim.Stacked`
      because an argument typed `R ! ([A] =>> Delim[A] | F[A])` does
      not recover `Delim + F`; `translate[A, Once, Async + F]` with
      every argument; `Once + Async + Pure` written out because `X +
      Pure` and `X` do not unify without help; `TestPipe`'s "union ACI
      lets the ascription". Each is a known trap with a known spelling,
      and the spellings live in memory files rather than in the
      library. THE LANE: (1) a page in docs/typepedia.md, "rows: what
      infers and what you spell", with the six shapes; (2) where a
      spelling repeats — a program at `Once + Async` handed to a
      `+ F` signature — a one-line overload that takes the narrower
      row (`SharedOnce.run` beside `runIn` is the pattern); (3) `+` as
      a match type or a `given` that normalises `X + Pure` to `X`, if
      a spike shows the compiler takes it. (2026-09-23)
