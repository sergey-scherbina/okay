## delimited-one-door - one door into the machine: Delimited.runHead; Frames.run closed; Delimited a ParaMonad

The operator's ask in cont-js-depth's design conversation: "Одна дверь в
машину" ("one door into the machine").

- **The door.** `Delimited` gains `runHead`, which runs a program to its
  head form: a value, an unanswered operation, or a capture leaving for
  a delimiter outside, each with the rest as its continuation. `run` is
  `runHead` under a boundary.
- **The loop is closed.** `Frames.run`, `enterAt` and `uncat` are
  private, and so is `Own`'s constructor. `Delimited.Machine` lives in
  `object Frames`, beside the loop it alone starts.
- **The callers go through the interface:**
  - Cont's strict-`k` bridge resumes as `runHead(k(x))` and finds its
    root through `retOf`;
  - Shift's nested runs and `Stacked` use `runHead` and its lazy form
    `owned`;
  - TestKont and KontBenchmark use `runHead` too.
- **The reference checks the door.** Its `runHead` is the program
  itself. TestDelimitedDifferential runs sub-programs through `runHead`
  at random points, captures crossing it included, on every platform. A
  mutant `runHead` that put a boundary around the sub-run failed all
  three program sets.
- **`Delimited` is a `ParaMonad`** (`[A, S, R] =>> M[S, R, A]`): `pure`
  takes its type parameters in Atkey's order, and `flatMap` is `bind`.

Docs: docs/delimited.md, "One door into the machine". Specs:
delimited.md, cont-js-depth.md (stage 2c).
