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
      (2) DONE (op-at, 2026-10-08): the `Op` → program conversions are
      gone, the facade's (`opToProgram`) and okay-cont's (`Op.toFree`):
      each warned at every use site. A bare operation where a program is
      expected is written `.at`, its row inferred from the expected type;
      bound by `flatMap`/`map` it needs nothing. `Free.reordered` (stage
      44, a row in another order) stays a conversion, opted into where it
      is used.
      (3) NO EXPECTED TYPE, NO ROW. (Seen again in async-fibers:
      `op.map(f).at[R]` does not compile — `op.map(f)` already chose its
      row, from no expected type; written `op.map(f)` where a program over
      `R` is expected, it is right.) An overloaded `run(p)` / `run(op)`
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
      nothing now. Fibers DONE (async-fibers, 2026-10-08): `spawn`/`fork`/
      `join`, `par`, `race`, `timeout`, `sleep` on the platform's
      `Scheduler` — a fiber runs one classic Await that starts the
      machine's drive (unmeasured: spawn/join's cost against the classic's;
      a late answer resumes on the answerer's thread, not sent home as the
      JVM's `own`/`adaptive` schedulers do for the classic). Earlier DONE (async-cancel, 2026-10-08): `attempt`,
      the cancellable drive (`runAsyncCancellable`), cancel scopes
      (`enter`/`exit`), the drive's one poll before registering; the drive
      measured unchanged (278 us against 273).
