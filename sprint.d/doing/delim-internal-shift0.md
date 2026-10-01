- [ ] delim-internal-shift0 — PRIORITY: MEDIUM, operator ask 2026-10-01:
      the library's own captures whose body never captures to the same
      prompt again (`emit`, `onReturn`, the exit by value) use `shift0`,
      not the derived `shift` — the same meaning without the `reset` the
      derivation wraps the body in (an `Inject`, a `Dollar0`, a closure
      and a step a capture). `Delim.shift` for users unchanged.
