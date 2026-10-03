- [ ] cont-strict-k — PRIORITY: HIGH (operator: Cont runs on the frame
      machine, optimize from here). The strict `k` — an opaque shift body
      gets `k` as a function that runs the rest NOW, a nested run of the
      machine — is where Cont on the machine is slow. After the lane of
      this name (2026-10-01): statePara 1.67-1.71x the old runner (was
      1.85x), fib100 2.45x (was 2.70x), contAnswer 1.20x (was 1.09x —
      see below). In BOTH slow lanes `k` LEAVES the run (PState returns a
      function that calls `k` later; Generate's `take` hands `k` out and
      `put` hides it in the outer Free's flatMap), so "a strict call that
      stays in the machine" does not apply to them: each call is a fresh
      entry. Allocation profile (async-profiler event=alloc, statePara):
      per leaf `Inject` + `Shift0` + the clause closure, per capture a
      `Kept` (+ `Next`), per call a `Return`; the root node is shared now.
      Leads: (1) a Cont leaf as one `Cont0` operation (the clause folded
      into it, ~5% of bytes); (2) the macro's lazy road for more shapes
      (closed as cont-stack-layer1-c 2026-10-02), above all PState's
      function answer (its DEPTH answered by cont-fun-answer's `Bounce`
      loop, 2026-10-03; the strict `k`'s price per call is unchanged) and
      Generate's `put`, re-measured ON THE MACHINE (the 2.8x that kept
      them opaque was the old runner's); (3) contAnswer lost 11% when the
      macro began emitting the program itself (less work, fewer bytes:
      the continuation is now the macro's lambda, inlined into `loop$1`;
      a `Rest` wrapper class made it 1.35x) — READ under hsdis
      (2026-10-01): `loop$1` on contAnswer went from 62 loads from `sp`
      to 103 (1356 -> 1286 instructions): the macro's continuation,
      inlined into the loop, raised the register pressure — the same
      cause as cont-frames-register-pressure, now in its third place.
      Done since: (1) the leaf as one operation (cont-strict-k-2:
      statePara 1.63-1.66x, fib100 2.44-2.51x, contAnswer 1.21x, bytes
      down on all three). Rows: history.d 2026-10-01T…-cont-strict-k*.tsv.
      RESTATED by cont-core-design (2026-10-01): the machine was cut
      to λ$ and three returns measured back in (Cat, nearest also at a
      resumed k's head, enterAt for the strict k); `Kept` and the strict
      flag are gone. Against master 4759dbff7 (the post-strict-k-2
      machine): statePara 1.04-1.08x, fib100 1.03x, contAnswer 1.21x.
      What the flag's removal costs is a clause lambda per `shiftLeaf`
      (statePara ~49 KB/op in the alloc profile): see
      cont-core-remaining-costs.
      AFTER cont-run-prompt (2026-10-03, profiled on 4e0a720d0): the leaf is
      one `Cont.Op` and the per-call root walk is gone; what a capture still
      allocates is the machine's own: `Return` 13%, `Dollar`, `Bind`, `Next`,
      `Run`, `Frame` ~10% each (statePara). DONE 2026-10-03
      (cont-strict-k-detached): a capture no longer copies a delimiter that
      is already over nothing (a strict `k`'s run, a resumed `k`'s head) —
      statePara 0.95x (-48 KB), contAnswer 0.93x (-24 KB), fib100 0.98x.
      THE `Next` CARRIER, MEASURED 2026-10-03 (history.d
      cont-strict-k-next-carrier), REFUTED as written: (a) a resumption's
      `Next` as an inline loop arm changed nothing (C2 already scalar-replaces
      it); (b) the frame-taken capture as the loop's own arm (`foundHere`,
      tail calls, no `Next`) is statePara 0.83x but contAnswer 1.17-1.21x and
      fib100 1.04-1.11x, inlining one arm or both: the clause call `f(k)` moves
      into `loop$1`, and contAnswer's clause — the macro's continuation — is
      inlined there (cont-frames-register-pressure, its fourth place). A road
      that keeps the clause call OUT of the loop and still carries no `Next`
      is the open question; nothing portable pins a call site out of line.
      SINCE cont-atm (2026-10-03) Cont runs on `Delimited`, not the λ$
      machine: everything above is that machine's history. On the new one
      (history.d cont-atm-machine) statePara 0.73x, fib100 0.74x,
      contAnswer 0.89x master's; the strict `k` is `Delimited.force`, a
      nested run counted by `StackSwitch`.
