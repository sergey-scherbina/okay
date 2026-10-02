- [ ] cont-stack-layer1-c — the rest of Layer 1 B, after
      RESTATED 2026-10-01 (cont-on-frames-probe): the runner this item
      was written against is gone — Cont runs on the frame machine, and
      there the transform's value is LARGER: a transformed body is a
      program over a lazy `k` (contAnswer 1.09x the old runner), while
      every shape still left opaque takes the strict `k`, a nested run
      of the machine (statePara 1.85-1.90x, fib100 2.62x). Item (0)
      below priced the old walk and is moot; the list of opaque shapes
      is the work, now worth more.
      cont-stack-layer1-b landed the answer-using bodies (`k(1) + k(10)`,
      `a :: k(x)`, interpolation, a block with `val x = k(1)`, a tail
      `if`/`match` with `k` in its branches, PState's `s => k(s)(s2)` as
      a `Fun` answer) on the explicit pending stack. What the transform
      still leaves opaque, each a lane with a red-first shape test and
      the 1M-on-128-KB zero-switch assertion:
      (0) FIRST, the price: `HandlerBenchmark.contAnswer` (1000 levels
      of `k(x + 1) + 1`) reads 1.24x walked against direct at 0.81x
      the bytes (history.d `cont-stack-layer1-b-contAnswer*`), so the
      24% is dispatch and loop shape, not objects — profile it
      (`-prof async`, memory async-profiler-not-prof-stack; the `Body`
      match, `rest.apply`, `Pending` push/pop, the `b ne null` on the
      hot loop) and either close the gap or make the expansion
      Scala.js-only (the platform where no switch exists), leaving the
      JVM and Native on the direct road Layer 2/3 already protect;
      (1) DONE 2026-10-02 (cont-stack-layer1-c, join points): a conditional
      NOT in tail position (`1 + (if c then k(1) else 2)`, a `match`
      feeding an expression) binds the rest ONCE as a local function and
      every branch ends in it; a million on 128 KB with zero switches
      (TestContMacro, red first: StackOverflowError);
      (2) PARTLY DONE 2026-10-02 (cont-stack-layer1-c): `k` called inside
      the lambda of `map`, `foreach`, `foldLeft` on an immutable `Seq`
      (`List`, `Vector`, `Seq`) — the lambda's body a program over the lazy
      `k`, the traversal `Cont.traverse`/`Cont.foldIn` (binds the machine
      runs, on an immutable list, multi-shot safe); a million on 128 KB
      with zero switches (red first: StackOverflowError). Then (the same
      day) `k` passed as a VALUE (`List(x).map(k)`) and an assignment from
      `k` (`v = k(1)`, `seen += k(x)`), a million each on 128 KB. LEFT:
      `flatMap`, `fold`, `Option`, `Either`;
      (3) VISIBLE user functions along the path `k` flows (an `inline
      def`, a same-compilation `def` through `Symbol.tree`, TASTy with
      `-Yretain-trees`), rewritten and cached;
      (4) `direct { !k(…) }` inside a body, through okay-direct's
      machinery;
      (5) `while` with `k` in its body needs a trampolined local loop (an
      iteration that never calls `k` must not recurse on the host); `try`
      around `k` stays opaque ON PURPOSE: with a lazy `k` the rest would run
      outside the `try`, and its exceptions would no longer be caught.
      Was: `try` around a call (the `finally` would have to run after
      the rest, which the pending stack can hold as a part), `while`
      with a call in the body, a lambda that is not the whole answer.
      NOT (6), unless for Scala.js alone: a FUNCTION answer (PState's
      `s => k(s)(s2)`) walked as a `Fun` with an `Ap` node was built in
      cont-stack-layer1-b and measured 2.8x the direct road on
      statePara (89 vs 32 µs), so it was taken out; on the JVM and
      Native Layer 3 runs it switch-free anyway. JS has no switch, so
      there — and only there — the walked function answer would lift
      the engine-stack bound on deep state-passing programs; a
      platform-conditional expansion is the shape if anyone needs it.
      Literature: Rompf, Maier & Odersky, ICFP 2009 (the selective CPS
      transform and its wall at code it cannot read).
