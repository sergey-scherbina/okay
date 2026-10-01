- [ ] cont-shift-op — PRIORITY: HIGH, operator ask 2026-10-01: Cont with
      its OWN operation type instead of riding Cont0 — `enum Shift =
      Strict(body) | Cps(body)` (operator's names), a Cont program a
      Freer over it, the frame machine its trampoline (a Shift leaves it
      as a head form), and `c / k0` the handler that answers it. No root
      prompt, no flags or cases in Cont0, no clause lambda per leaf.
      Spec: specs/cont-shift-op.md. A probe: A/B on the Cont lanes decides.
