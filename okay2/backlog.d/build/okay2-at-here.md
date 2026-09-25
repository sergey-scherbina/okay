- [ ] okay2-at-here — `At.here` as the caller's `file:line` (today
      `<unknown>`). okay2-module-plan showed that a def macro in the
      core works for any use site in another compilation run. The open
      question is whether the core itself SUMMONS an `At` anywhere. If
      it does, that summon would expand in the defining run and fail,
      and those summons need a small `okay2-macros` subproject.
      Check with `grep` before building one. (2026-09-25)
