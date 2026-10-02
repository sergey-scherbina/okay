- [ ] shift-effect-level1 — the probe's next steps (shift-effect-probe,
      2026-10-02, specs/shift-effect.md Results). (1) on (b), a `reset`
      inside a running machine pushes its prompt on that machine instead
      of starting a second one: removes the nesting limit (10 000 pass,
      30 000 overflow, both implementations). (2) the machine's start
      per small `reset`: `twoShot` 9.53 µs on (b) against 7.29 on (a).
      (3) the operator's decision on level 1's names: `Shift % R` and
      `shift`/`reset` in place of `okay.shift`/`okay.reset` (Cont moves to
      level 2) and of `Delim.delimited`/`Delim.shift` for the one-prompt
      case. TRIGGER: the operator's go on (3).
