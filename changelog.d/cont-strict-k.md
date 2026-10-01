## cont-strict-k - Cont on the machine: the strict `k` re-enters with its run's root node; the macro emits a lazy body as the program itself

- A Cont run builds its root delimiter node once and a strict `k`
  captured up to it re-enters with that node (`Frames.enterIn`), where
  every call built a `Reset`; `Frames.machine` starts at given
  registers, `run`/`runOn`/`enterIn`/`enterAt` its typed cases (05a95cc06;
  a `Resume`-free `enterAt` before it, 8a5e104c7, was neutral — C2 had
  scalar-replaced the `Resume`). statePara 1.85x -> 1.67-1.71x the old
  runner, fib100 2.70x -> 2.45x.
- The macro emits a lazy body as the program (`Cont.call`, `Cont.done`,
  `Cont.lazyLeaf`): `Body`, `Cps`, `cps`, `bodyProgram`, `runBody`
  deleted (c70d048da). contAnswer 16 KB lighter and 11% slower (1.09x ->
  1.20x) — kept on the operator's call; a `Rest` wrapper class was
  refuted (1.35x). In both slow lanes `k` leaves its run, so a strict
  call staying in the machine does not apply; the leads are in backlog
  cont-strict-k. Rows in history.d.
