- [ ] left-nested-build-cost — PRIORITY: MEDIUM (measured). Found by
      handler-single-pass's re-measure, 2026-09-27 (specs/handler-fusion.md,
      "Re-measured 2026-09-27"). The same 1000 State/Writer operations run
      in 32 µs when the program is built by `foldLeft`
      (`xs.foldLeft(pure(z))((m, x) => m.flatMap(...))`, left-nested
      binds) against 13 µs right-nested, and `-prof stack` puts 60-70% of
      the left-nested time in `Free.resume`'s rotation (its lambda
      `f(_).flatMap(g)` allocates a closure and a Bind per step). That is
      2.5x from the SHAPE, twice what fusing the handlers buys.
      Two roads, in cost order:
      (1) the library's own builders build RIGHT-nested. `Maybe.collect`,
          `Chronicle.all` and its re-dictation, and every other
          `foldLeft(pure(...))(flatMap)` in main code (survey with grep)
          become a recursive step (`go(i)` = op(i).flatMap(_ => go(i+1)),
          trampolined by the Free bind). A public `!.traverse`/`foldM`
          that builds right-nested gives users the same thing. Cheap and
          local; measure one converted builder first.
      (2) a continuation QUEUE in `Bind` (van der Ploeg-Kiselyov
          "Reflection without remorse", the type-aligned sequence), so a
          left-nested bind is an O(1) append rather than a rotation
          allocating per step. This is core surgery: `TestInlineBudget`
          guards `resume`'s size, and the 2026-09 note that the queue is
          "not needed" measured stepping, not a left-nested build. Only
          if (1) leaves user-built foldLeft programs slow.
