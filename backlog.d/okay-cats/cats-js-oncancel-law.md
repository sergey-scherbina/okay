- [ ] cats-js-oncancel-law — found 2026-10-16 (okay-caps-rule's gate, master
      at the lane's base): TestCatsEffectLaws "Async[A ! CatsFx + Async]:
      async.onCancel associates over uncancelable boundary" fails on JS,
      reproduced alone (`okayCatsJS/testOnly okay.cats.TestCatsEffectLaws`:
      109 run, 1 failed); the same suite green on JVM (110). Not touched by
      that lane (okay-tx tests and docs only). To find: whether the JS
      Async run cancels across an uncancelable region differently.
      LATER THE SAME DAY it fails on the JVM too, alone and every time
      (`okayCatsJVM/testOnly okay.cats.TestCatsEffectLaws`: 110 run, 1
      failed), on master's code with okay-core at f0e680f and at 21a4447 —
      not the kernel pin, not fit-left-linked: something landed in okay
      between the two runs (found by fit-left-linked's gate).
