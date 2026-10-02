## shift-effect-typed - D-F shift, answer types told apart, direct style

The second round of the `Shift % R` probe (specs/shift-effect.md),
operator's ask 2026-10-02.

- Danvy-Filinski's `shift` beside `shift0`. Its body runs under its
  `reset` and may capture to it again: two shifts in sequence give 55
  (75 with State), a capture from inside another's body gives 32.
- Captures of different answer types share a program. A compile-time key
  per answer type (`Key`, a package-private macro in okay-direct) makes
  `Shift`'s test `ByValue`, so `Distinct` passes `Shift % Int + Shift %
  String` and each `reset` takes only its own. On (b) a `reset` whose row
  still holds a Shift pushes on the outer machine (`Nesting`), so a
  capture crosses a `reset` of another type.
- Direct style (`ShiftDirect`): `reset` over a `direct` block, the body of
  `shift` a block, `shift` inside a block answering the value itself.
- 19 tests on each implementation. JMH: keys cost (a) 1.05x and (b) 1.11x
  on `seq`. D-F `shift` on (b) is 75.3 µs against 60.6 for `shift0`; on
  (a) the D-F lane gives no number and overflows past 1 000 captures. The
  recommendation stays (b). Next steps in backlog `shift-effect-level1`.
