- [ ] handler-forms — level 2, step 2 (operator, 2026-10-02: "1. Ок"): ONE
      door for the effect author, four constructors on `Handler` that each
      give a level-1 value, `p.handle(h)`: `answer` (an answer per
      operation, over `!.relay`), `state` ((state, op) => (state', answer),
      the State loop), `into` (each operation a program in other effects,
      over `!.translate`), `control` (`resume` once, twice or not at all,
      over `Effects.handle`, with no `Cont` in sight). A probe on docs' `Users`
      and on Reader / State / Maybe re-expressed, each measured against
      the built-in code one lane at a time. specs/handler-forms.md.
      (2026-10-02)
