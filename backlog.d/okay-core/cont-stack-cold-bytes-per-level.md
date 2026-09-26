- [ ] cont-stack-cold-bytes-per-level — PRIORITY: MEDIUM (reliability on
      the count road). `StackSwitch.coldBytesPerLevel` is 1 200 B, but a
      Cont level measured 1 858 B under `-Xint` and 1 726 B on the first,
      cold run of a default JVM (cont-stack-8mb-flake, 2026-09-26,
      specs/cont-stack.md Results). The exact road is safe (a grant is
      one `margin` slice and the margin absorbs a 2x overshoot), but the
      COUNT road's first room is `defaultStackBytes / 1 200 / 2`: on a
      2 MB default that is 873 levels, 1.62 MB interpreted, against
      ~1.66 MB the stack has past the guard zones — the halving meant
      for the caller's own frames is spent on the underestimate, and on
      Linux's 1 MB default (436 levels, 810 KB) the margin is the guard
      zone alone. THE LANE: red first — a cold program on the count road
      (`-Xint`, no native access, a default-size thread, the caller a
      few hundred frames deep) that overflows at the first room; then
      raise the constant to the measured interpreted level (≈1 900 B)
      or derive the room from it, and re-measure statePara on the count
      road (a smaller first room can mean an earlier switch).
