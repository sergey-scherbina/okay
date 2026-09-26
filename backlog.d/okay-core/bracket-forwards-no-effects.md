- [ ] bracket-forwards-no-effects — PRIORITY: LOW (naming/doc, or a
      small API). Found by the core review 2026-09-26. `bracket`
      (Resource.scala:151) takes `use: R => A ! F` but runs it to the end
      INSIDE with `use(r).runWith` under a `Handler[F]` and
      `try/finally`. So `use` cannot forward any effect to an outer
      handler: every effect of F is answered in place, comonadically. Its
      doc says so ("for a release scoped to a whole program of arbitrary
      effects, use the Resource effect"), but the NAME is the one every
      other library (cats `bracket`, ZIO `acquireRelease`) uses for the
      effect-polymorphic version, so a reader expects the Resource
      effect's semantics. Choose: rename (`bracketNow`, `usingNow`), or
      make `bracket` the Resource effect's `acquire`+`scoped` sugar and
      keep this one under the new name. Check the callers first
      (`grep -rn 'bracket\[' --include=*.scala`).
