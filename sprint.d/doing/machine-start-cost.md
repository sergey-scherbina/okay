- [ ] machine-start-cost — PRIORITY: LOW (2026-10-04). A run assembles a
      machine each time: `Run`, `Steps`, `Nested`, a `Prompt` with lazy vals.
      It is what made stateSmall 4.3x when handlers ran as frames
      (handlers-as-frames); on today's paths it is paid once per
      `Shift.run`. Measure before acting.
