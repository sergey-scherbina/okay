- [ ] cont-first-module-findings — what the first real module on the
      machine's `A ! R` found (okay-cache, with Async under it,
      specs/freer-min.md stage 49), before the other modules move:
      (1) AN API SAYS ITS ROW NOW. Classic `V ! Async` was one word and
      sat in any bigger program by subtyping; on the machine a row is
      fixed, so a single operation is an `Op[Async, V]` (a program over
      any row that has Async) and a program of several steps takes the
      row: `getOrLoad[R <: Row](k)(load)(using Member[Async, R]): V ! R`.
      Honest, but every multi-step API method grows `[R <: Row]` and a
      `using`. A facade word for "a program over any row with E" would
      take that back (a type lambda over `R` cannot be a return type; a
      `Prog[E, A]` class with the row from the bind, as `Op` does, can).
      (2) THE CONVERSION WARNS. `opToProgram` (stage 48) needs
      `scala.language.implicitConversions` at every USE site: 14 feature
      warnings in okay-cache's tests. The tests write `.at` instead (the
      row inferred from the expected type), which needs no conversion;
      either the conversion goes, or the facade finds a form that does
      not warn.
      (3) NO EXPECTED TYPE, NO ROW. An overloaded `run(p)` / `run(op)`
      leaves `getOrLoad[R]`'s row undetermined (inferred `Async +: Row`);
      the tests split it into `run` and `runOp`. Any API that takes a
      program through an overload meets this.
      (4) THE BRIDGES ARE PER PROGRAM. A machine module calling a classic
      one (okay-sql in TestWriteThrough) crosses by
      `AsyncCont.fromClassic`, one Await per classic program, driven by
      the classic drive; the Scala 2 facade crosses the other way by
      `toClassic`, one classic operation per machine stop. Unmeasured.
      (5) DONE (drive-in-place, 2026-10-08): the callback drive was
      2.38x the classic (a capture per Run); now 1.15x (273 us per 10k
      Runs against 236) — a Run on the answering road by `Cap.Split`,
      an Await answered during its registration by the clause's tail
      `k(x)`, only a pending Await captured. What is left is the
      machine's per-operation cost (the answering `AsyncCont.run`,
      1.09x), not the drive's.
      (6) WHAT ASYNC LACKS ON THE MACHINE before okay-stream can move:
      `attempt` (Retry), cancel scopes and `runAsyncCancellable`,
      fibers/`Scheduler.fork`, the drive's poll-then-park. okay-cache
      needed none of them.
