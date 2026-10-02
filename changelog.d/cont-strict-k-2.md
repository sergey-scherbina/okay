## cont-strict-k-2 - a Cont leaf is one operation: the clause folded into Shift0; contAnswer's 11% read under hsdis

- `Shift0.strict` marks a Cont body that takes a strict `k`; the
  machine builds that `k` itself (`Cont.strictBody`), and a lazy
  leaf's body is its clause — the closure every Cont leaf carried is gone
  (041b7c2cf). Against Cont.step: statePara 1.63-1.66x (437 KB, was 469),
  fib100 2.44-2.51x (39.4 KB, was 41.1), contAnswer 1.21x (326 KB, was
  342).
- contAnswer's 11% since the macro emits the program itself, read off
  `loop$1` under hsdis: 62 loads from `sp` before, 103 after — the
  macro's continuation, inlined into the loop, raised the register
  pressure (backlog cont-strict-k, cont-frames-register-pressure).
