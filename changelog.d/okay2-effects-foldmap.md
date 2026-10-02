## okay2-effects-foldmap - okay2's TailRecM and Effects.foldMap, control without a capture on a tail resume, jmh-lane from a sub-build

okay2 gets the last member of okay's `Effects`, and `control` loses its
capture per operation (specs/okay2.md stage 50).

- **`TailRecM[F]`, provided by the carrier.** Instances for `Option`,
  `Either`, `LazyList` and programs, plus `TailRecM.deferring`. A monad
  without an instance does not compile.
- **`Effects.foldMap`** (also on the syntax) folds through G's own loop:
  a million operations on a 128 KB thread, on Scala.js and on Native.
- **`Handler.control`** answers a tail resume with no capture (the
  core's `Resume`): 36.3 us against 46.5-48.4 us and -72 B an operation
  on 1 000 asks, 1.11x the no-capture floor (history.d
  `okay2-control-resume`).
- **`scripts/jmh-lane.sh`** runs from `okay2/`: it sources
  `bench-window.sh` through `$here` and runs sbt in the caller's build.
  Selftest 15.

Tests: TestStackSafeLoops, TestStackSafeLoopsSmall, TestHandler (control
tail resume 100 000 deep). Docs: docs/okay2.md §4, §16, §23.
