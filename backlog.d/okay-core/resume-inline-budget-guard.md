- [ ] resume-inline-budget-guard — PRIORITY: MEDIUM (small). Found by the
      core review 2026-09-26. `Free.resume` compiles to 323 bytes against
      HotSpot's FreqInlineSize of 325 (Free.scala, Effects.scala's comment
      on `handle`; memory inlining-threshold-two-faces). Whether it is
      pasted into every interpreter loop therefore depends on two bytes:
      one more case, or a scalac change in codegen, silently flips the
      inlining in every caller. It has already moved numbers both ways
      (-6% relay, +15% handle, rows `de-*`). Nothing notices when it
      crosses. Lane: a test (javap on the classfile, or ASM) that asserts
      the bytecode size of `Free.resume` stays under 325, and says in its
      failure message which rows to re-measure. Also the other hot
      methods whose size was tuned by hand (`relay` loop 244 B).
