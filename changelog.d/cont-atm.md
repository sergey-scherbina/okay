## cont-atm — one effect-independent machine, `Delimited`; Cont with typed answer-type modification; Shift and the handler frames on it; the λ$ machine deleted

Operator, 2026-10-03: Cont with no casts and no `Any`; a delimiter with
answer-type modification; `Freer` unchanged; "Freer/Delimited полностью
независимы от того какой эффект в нем работает — их задача предоставить
все необходимые примитивы абстрактной машины". specs/cont-atm.md.

- `Delimited` (Delimited.scala) is the machine: `Frames` a segment (the
  operator's `A => F[B, S, R]`), `Stack` segments joined by two kinds of
  boundary typed by their installation — `Delim` (a value passes: a prompt,
  a nested run's barrier, a handler's frame) and `Bound` (the level closes
  and its answer flows out: Danvy–Filinski's `reset`). Primitives in a
  trait; effects plug in by `Step`; runners `Machine`, `Under` (nested
  machines), `Over` (a `Free` row, nested runs stepped into). No cast in it.
- Cont is an effect on it with ATM typed on answer boundaries — no claim,
  no cast; `A ! F` are plain answers.
- `Shift % P` is an effect on it (DPJS's segmented stack: a capture walks
  boundaries, never frames). New: resume with a computation —
  `Shift.withSubCont`, a captured `k` is a `Shift.Resumption` with
  `resumeWith(m)` (DPJS `pushSubCont`).
- Handler frames (`HandleFrames`) are Shift prompts with a `Handling`
  mark; exceptions a machine primitive (`guarding` per run, a throw a
  `Delay(Delimited.Thrown)`); `Handling.ever`/`Catching.ever` gone —
  per run now (backlog handling-ever-per-machine answered).
- `LambdaDollar.scala` (the λ$ machine, `Cont0`) deleted. Its interface
  is a TEST oracle now (src/test/scala-cross/LambdaDollar.scala) whose
  instance is Shift: the laws, the differential oracle and the depth
  programs check the new machine against the reference. TestKont and
  KontBenchmark (written on `Cont0`) deleted.
- Measured against master (history.d cont-atm-machine): statePara 0.73x,
  fib100 0.74x, contAnswer 0.89x, stateForeign 0.91x, the handler folds
  at parity; delimGenerator 1.30x — a prompt is two boundaries, paid per
  `shift` (backlog shift-prompt-one-boundary).
