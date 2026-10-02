## shift-effect-probe - shift/reset as an effect in A ! F, measured

The operator's level 1 (2026-10-02): the user knows `A ! F`, `pure`,
`perform`, `shift`, `reset`, `handle` and nothing else. So a capture is
`Shift % R` in the row and `reset` is its handler (specs/shift-effect.md).

- Probe in okay-direct's tests: one API (`ShiftApi`), (a) a deep handler
  over `Effects.handle`, (b) on Delim's machine with one shared prompt.
  `TestShiftFx`, 13 tests on each, all green. The suite covers the laws,
  D-F's `k(1) + k(10)`, State after the capture, multi-shot under Choose,
  nested resets, direct style (`shift`'s body a `direct` block, no new
  macro), 100 000 captures, and level 2's `cont`/`embed` round trip with
  an answer-type-modifying `Cont` whose answer carries a Writer.
- JMH (`ShiftFxBenchmark`, history.d `shift-effect-probe`): on 1000
  captures (b) 54.8 µs equals Delim today (53.9) and beats Cont (60.8)
  and (a) (86.9). On 100 small resets (a) 7.29 µs beats (b) 9.53.
- Found: nested resets overflow between 10 000 and 30 000 on both,
  because each `reset` is its own handler run. Filed with the next steps
  as backlog `shift-effect-level1`.
