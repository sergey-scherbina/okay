## shift-patterns - exit and a generator over Shift % R

Level 1 (operator's roadmap, 2026-10-02). `Shift.exit(v)` leaves the
nearest `reset` block with `v`. `Shift.collect { … Shift.emit(w) … }` is a
generator: it answers everything emitted, in order, with State beside it
and 100 000 emits in constant stack. These sit in `object Shift`, because a
top-level `collect` clashes with Stream's. TestShiftPatterns, and the user
page's "named patterns" section, pinned. `pause`/`ask` remain Delim's.
