- [ ] shared-once-knot — PRIORITY: LOW (trigger). `SharedOnce` (once-across-fibres) says out
      loud that a KNOT — a program demanding its own handle while it
      runs — is a HANG, where `Once.run` throws: the waiter is the
      fibre that would have stored, and `Async` names no fibre to tell
      a self-demand from a sibling's. A thread heuristic misfires when
      a continuation resumes on another thread. If a fibre identity
      ever exists in `Async` (a fibre-local, a `Fiber` reference the
      running program can ask for), the knot becomes the same
      `IllegalStateException`. TRIGGER: a fibre identity in okay-async,
      or the first hang somebody spends an hour on. (2026-09-23)
