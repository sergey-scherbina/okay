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
