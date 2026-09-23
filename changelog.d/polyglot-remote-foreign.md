## polyglot-remote-foreign - programs as data across a process, multi-shot, and Haskell

A far-side program is written as DATA: an answer, or a named operation
plus the pure function that continues it. The worker keeps that function
under an id until okay forgets the run, so a `Choice` handler can
continue the same continuation twice, and every branch is exact. The
callback dialogue could not do that, because its continuation is a
blocked stack frame.

- Python: `okay.done`, `okay.perform(...).then(f)`, and
  `Py.program(...).calling(cbs)(args)` answering a `PyRun` (`program`,
  `forget`).
- R: `okay_done`, `okay_perform`, `okay_then`, and `R.program`.
- Haskell: the jar ships `/okay/hs/Okay.hs` (base and containers only).
  `HaskellWorker.build(dir)` compiles a `Main.hs` with GHC, and
  `PySubprocess.speaking(Seq(bin))` drives it on the same wire.

GHC 9.14.1 was reinstalled through Homebrew for this, after the operator
remembered it had been installed; ghcup's directories were empty.
Multi-shot `Choice` works from all three languages. Durable journals the
walk, and a replay needs no interpreter. A default-gate test checks the
walker, and a mutant is caught. The shims move to Python 6 and R 7.
backlog `polyglot-haskell` is removed: its road (1), a GHC worker
process, is what this built, and road (2), GHC inside the JVM, stays
refused. Docs: docs/python-and-r.md "Programs as data: many answers,
and Haskell", docs/modules/okay-py.md; spec: specs/remote-foreign.md.
