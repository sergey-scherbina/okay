## shift-effect-core - Shift % R in the core: top-level shift, shift0, reset

Level 1 of the API, step 2 (operator, 2026-10-02; specs/shift-effect.md).
The continuation is an effect in `A ! F`, on Delim's machine.

- `src/main/scala/Shift.scala` adds `shift` (Danvy-Filinski's, the body
  under its reset), `shift0` and `reset` at the top level, with `Shift % R`
  in the row. A compile-time key per answer type (`Shift.Key`, interned,
  holding its prompt) lets captures of different answer types share a
  program. `Shift.Nesting` makes a `reset` whose row still holds `Shift` or
  `Delim` push on the running machine. `Shift.cont`/`Shift.embed` are level
  2's doors to `Cont[A, R ! F, R ! F]`.
- Direct style needs nothing of its own: `reset(direct { … })` with
  `shift(k => direct { … })`, marked with `.?` or auto-coloured.
- TestShift (13) and TestShiftDirect (3), green. The probe's handler
  implementation, ShiftFx and ShiftDirect are gone.
- JMH: `shift0` 56.1 µs on 1000 captures against Delim's 55.0 (1.02x; the
  probe's per-shift lookup was 1.11x), D-F `shift` 69.1 µs.
