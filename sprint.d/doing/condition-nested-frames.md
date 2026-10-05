- [ ] condition-nested-frames — PRIORITY: LOW. Found porting `Condition` to
      okay2 (okay2-condition-repair, 2026-10-05). okay's `Condition.run`
      (okay-direct/src/main/scala/Condition.scala) has two costs a
      recursive program meets, which okay2's twin does not:
      (1) a `Within` frame's region is entered by a DIRECT call of `loop`
      (`Right(loop(body, …).flatMap …)`), one host frame per nested frame:
      a recursive program opening a `within` per level overflows. okay2
      defers it (`Free.delay(() => loop(body, …))`), so a hundred thousand
      nested frames run (TestCondition there).
      (2) each `loop` builds `names = menu.map(_.name).toVector` up front —
      O(depth) per frame, quadratic for n nested frames (okay2's twin timed
      out at 100 000 before the `lazy val`).
      Also: the inventory rows for `loop`/`step`
      (specs/stack-safety-okay.tsv:104-105) give the reason "per node of
      the user's source the macro reads", which is not this code's; with
      (1) fixed they become trampolined and the rows go. Port okay2's
      nested test, red first, then the two changes.
