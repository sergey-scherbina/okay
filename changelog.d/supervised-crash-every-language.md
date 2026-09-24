## supervised-crash-every-language - one crash suite over five languages, and the broken-wire defect it found

- `CrashConformance` (okay-py tests): the worker's process is started by
  the suite and SIGKILLed by its pid, from outside, as an OOM kill or a
  crash arrives: between two choices of a multi-shot program (every
  branch comes back), while idle (the next program runs on a fresh
  process), mid-ask in direct style (`WorkerDied` as data, then the next
  call runs). `TestCrashPython`, `TestCrashTypeScript`, `TestCrashGo`,
  `TestCrashRust`, `TestCrashHaskell` (programs only).
- FIXED: a killed far side broke the wire instead of ending it. The JDK
  closes a dead child's pipes and the next write threw `IOException:
  Stream closed`, which escaped the supervisor. `ForeignWorker.send` and
  okay-r's `RSubprocess.send` now answer it as a dead worker, the same as
  an end of stream (and so a socket a peer reset). Watched red on all
  five languages first.
- Docs: one-language.md, the support table's Recovery column (replay on
  every stdio language) and the suite. Spec: polyglot-one-wire.md.
