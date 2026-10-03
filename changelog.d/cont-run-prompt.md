## cont-run-prompt — Cont on its own operation, a frame per reset (2026-10-03)

Commits: see `git log --grep cont-run-prompt`.

- Operator ask: "каждый reset делал свой делимитер, вложенные shift его
  использовали", then "для Cont нужен свой тип эффекта ContShift". A Cont
  leaf is now `Cont.Op[S, R, A]` — `Strict`, `Program`, `Lazily`, the
  three forms ContMacro picks — typed exactly as the leaf it is. Each
  `run`/`reset` puts a frame of its own on the machine (`Root`, a
  `Cont0.Handling`), and the machine hands an `Op` to the nearest one.
  The static root prompt is gone.
- The road cont-shift-op refuted (2026-10-01) failed for want of a
  boundary in the machine; a frame is one, and a deep one: `k` carries
  its run's frame. The refuting case, `k(x + 1) + k(x + 1)` chained, is
  a TestCont test now (d = 0..6, lazy and strict against `Func`).
- Machine (Delimited.scala), each change general: `Cont0.Framed` — an
  operation that always looks for its frame, so Cont leaves the
  process-wide `Handling.ever` off (`Handling(name, opens = false)`);
  `frameFor` — the head frame first, no `uncat`; `framed` — a capture
  straight to that frame, no `Shift0` built, its clause
  `Handling.clauseOf` (an `Op` is its own; others a closure as before);
  `found`/`nearest` take the delimiter and clause rather than a node;
  `retOf` is `frameOf`; `asShift0` labels with `h.what`, not a string
  built per operation.
- Measured, master vs branch, arms alternated (history.d
  cont-run-prompt): statePara 0.94 (bytes 0.88), fib100 0.95 (0.93),
  contAnswer 1.01 (0.98), stateForeign 1.01, handlePrebuilt 1.01. The
  first cut was contAnswer 1.60x; the record of each step is there.
