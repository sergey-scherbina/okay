- [ ] classic-package — THE DECISION FIRST, the operator's (open as of
      2026-10-07): where the classic Free-tree library lives once `A ! F`
      is to be the machine's program in the core. Measured: of the core's
      50 files all but five data files (Aggregate, Fold, HMap, TRef,
      Validated) and the shared three (Effects trait, Control, Prog) depend
      on the tree, so "the classic" is the core as it stands; and two `!`
      cannot share package `okay` (both define `!`, `pure`, `effect`).
      Option 1: the classic into module `okay-classic`, package
      `okay.classic`, over okay-freer; the 328 module files with
      `import okay.*` get `import okay.classic.*`, qualified names
      (`okay.Answers` 153, `okay.Row` 54, `okay.Writer` 40, `okay.Shift` 39,
      `okay.Reader` 16, `okay.State` 14, `okay.Free` 13, `okay.Handler` 5)
      rewritten by script; classic re-exports `Effects`/`Control` by alias;
      the core `okay` keeps Effects, Control, Prog, okay-cont's machine and
      library, and `okay.!` becomes the machine's (bang-is-cont). One day
      of mechanics, one full gate. Option 2: the core stays as it is and the
      machine's program lives in `okay.cont` as `okay.cont.!` (landed,
      stage 41) — nothing to do, but the artifact called the core is then
      `okay-cont`. Spec: specs/freer-min.md stages 29–41; the opaque `!`
      was tried and not taken (stage 37).
