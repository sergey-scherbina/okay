- [ ] continuations-as-data-spike — road 4, a SPIKE with a written
      verdict: one effect, one program, defunctionalized `k` measured
      against the closure version on the same lane. Not planned until
      the verdict exists.
      LITERATURE (biernacki-literature, 2026-09-24): this is the
      CPS -> defunctionalization -> abstract machine road of Ager,
      Biernacki, Danvy & Midtgaard, "A functional correspondence
      between evaluators and abstract machines" (PPDP 2003) and "From
      interpreter to compiler and virtual machine" (BRICS RS-03-14),
      mechanised by Buszka & Biernacki, "Automating the functional
      correspondence between higher-order evaluators and abstract
      machines" (LOPSTR 2021). A second reason to run the spike: two
      hand-fused `runFree` rewrites were refuted
      (runfree-inlined-rotation, runfree-inlined-small-step), and
      DERIVING the machine from the interpreter is the step those
      lanes skipped.
      THE DEPTH SLOPE this verdict is to be read against
      (delim-machine-allocs, 2026-09-27; `DelimDepthBenchmark.
      delimCaptureDepth`, N = 1/16/256 levels between a `shift` and
      its prompt, k called 1 and 8 times, bytes load-proof, time at
      load ~5-10). `push` levels (a mark + a bind on the machine's
      stack, what `split` copies and `reify` re-walks): ~425 B and
      ~42 ns per level for one capture called once, and each further
      call of k ~225 B and ~21 ns per level. `bind` levels (plain
      `map`s, which `Free.resume` rotates into one composed k before
      the machine sees them): ~137 B / ~12.5 ns per level, ~49 B /
      ~6 ns per level per further call. So a level the machine holds
      as frames costs ~4.6x a plain `map` level per re-run of k — that is the prize a
      defunctionalized k competes for, and Logic's `choose` (which
      captures through every level since its prompt) pays it.
