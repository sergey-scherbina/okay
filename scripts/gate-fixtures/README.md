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
