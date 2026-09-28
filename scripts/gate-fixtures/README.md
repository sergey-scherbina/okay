Gate logs for `scripts/gate-selftest.sh`'s classifier cases (section 7):
each is the verbatim `[error]`/`[warn]` lines of a real gate log, trimmed
to what `gate.sh --read` classifies on, paths shortened.

- `shape-c-accept-timeout.log`: okayChainNative, 2026-09-25, a Native
  binary that never connected (ComRunner's 40 s accept) and was killed.
- `shape-c-beside-a-real-failure.log`: the same, next to a module with a
  real `==> X`; must stay RED.
- `loaded-frameworks-no-accept-timeout.log`: the loadedTestFrameworks
  line WITHOUT the accept timeout, a mechanism nobody has explained;
  must stay RED.
- `shape-b-run-terminated.log`: shape B as the 2026-09-09 entry quotes it.
- `demoted-timeouts.log`: the shape of one-bind-hot-steps' whole-build
  gate, 2026-09-27 (gate-demote-timeouts): a run the bench window moved
  to the efficiency cores, whose only failures are munit timeouts
  (TestGenerate's 1M at 254 s against 120, TestCoreAsync at 30). The
  last line is the marker gate.sh appends to its own log when it ran
  demoted; classified DEMOTED, not RED. The same lines WITHOUT that
  marker are a plain RED — the selftest derives that case from this file.
- `demoted-beside-a-real-failure.log`: the same, next to a real
  `==> X`; must stay RED, naming the real one.
