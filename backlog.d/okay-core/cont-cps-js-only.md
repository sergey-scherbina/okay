- [ ] cont-cps-js-only — Layer 1 B's CPS walk of answer-using shift
      bodies (`Cont.Cps`, `Body`, `Pending`, cont-stack-layer1-b) pays
      where it buys nothing: on the JVM and Native, Layer 2/3 already
      run the direct road switch-free until the stack is really out
      (~2 switches of 33 us on a 1M-level program), while the walk
      costs 1.24x on EVERY shift (history.d
      `cont-stack-layer1-b-contAnswer`: 25.44 vs 20.50 us/op, 0.81x
      the bytes — dispatch and loop shape, not objects). On Scala.js
      it is the ONLY stack protection there is: `scala-js/
      StackSwitch.scala` has `firstRoom = Int.MaxValue`, `more = 0`,
      `fresh = body(MaxValue)` — no switch exists. So the expansion
      belongs to JS alone.
      STEP 0, before anything moves — a finding to check: the last two
      `scripts/history.sh contAnswer` rows (2026-09-27/28) read
      245 904 B/op on master, the DIRECT road's bytes; the walked road
      measured 197 992. Either the transform no longer fires on
      `HandlerBenchmark.contAnswer`'s body (`k => k(x + 1) + 1`,
      src/jmh/scala/okay/HandlerBenchmark.scala) on today's master, or
      the fastpath rounds changed the direct road's allocation. One
      `-prof gc` round through `scripts/jmh-lane.sh` says which; if the
      transform does not fire, the 1.24x is the 09-26 lane's number,
      not master's, and that is a defect of its own.
      STEP A — the macro learns the platform, the runner is untouched:
      `-Xmacro-settings:okay.platform=js` on the JS projects in
      build.sbt, `ContMacro.cpsBody` reads `CompilationInfo.
      XmacroSettings` and answers `None` elsewhere, so a body becomes
      `shiftLeaf` as before layer1-b. A setting rather than a
      classpath probe for `scala.scalajs.js.Any`: it is visible in the
      build and can be switched off to measure. TestContMacro's "1M
      answer-using shifts on a 128 KB stack: ZERO switches" stops
      being true on the JVM, as it should — it keeps `assertEquals(a,
      2 * n)` (the program answers, through switches), and the
      zero-switch claim moves to a JS cross test (no counter there;
      the assertion is 1M levels on node without StackOverflow).
      Numbers: contAnswer before/after (expected 1.24 -> ~1.00);
      statePara must not move (the transform never fired there).
      specs/cont-stack.md Decisions gets "Layer 1 B is a JS road";
      cont-stack-layer1-c's item (0) closes and its (1)-(5) are
      rewritten as JS-only work.
      STEP B — only on a proven number: if cont-stack-statepara-time-
      residual (28.20 vs 27.13 us, ~4%) survives a same-series re-read,
      that is the tax of `step`'s two extra parameters and `b ne null`
      on programs with no `Cps`. Then `step` splits by source set:
      `scala-jvm-native/` the three-parameter loop, `scala-js/` the
      five-parameter one. The price is a second copy of the rotation
      (eight cases) against `Free.resume`'s "one rotation" — so only
      with the number in hand; the JIT may already fold
      `pending eq Pending.None` and the tax may be zero.
      Changes runner and macro behavior: the gate is the full
      `affected master staged`.
