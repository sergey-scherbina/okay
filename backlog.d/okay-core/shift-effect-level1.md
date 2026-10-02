- [ ] shift-effect-level1 — the probe's next steps (shift-effect-probe and
      shift-effect-typed, 2026-10-02, specs/shift-effect.md Results).
      (1) nested resets of the SAME answer type start a machine each on
      (b): 3 000-10 000 deep, depending on the JIT. (2) the machine's start
      per small `reset`: `twoShot` 9.53 µs on (b) against 7.29 on (a).
      (3) the prompt looked up by key per `shift` on (b), +11% on `seq`:
      a prompt cached in the call site's key. (4) direct style still
      names `[R, A, F]` at every `shift`: R and F from the block's
      `DirectCtx`. (5) the operator's decision on level 1's names:
      `Shift % R`, `shift`/`shift0`/`reset` in place of `okay.shift`/
      `okay.reset` (Cont moves to level 2) and of `Delim.delimited`/
      `Delim.shift` for the one-prompt case. TRIGGER: the operator's go on (5).
