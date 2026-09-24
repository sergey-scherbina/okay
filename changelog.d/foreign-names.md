## foreign-names - the foreign engine under neutral names

The engine began as the bridge to Python, so it was `Py`, `PyEval` and
`PySubprocess`. It now carries Python, TypeScript, Haskell, Go and Rust
over pipes, TCP and in-process calls, so its canonical names are neutral
(operator, 2026-09-24):
- `ForeignEval` is the effect;
- `ForeignWorker` is the engine (`speaking`, `connect`, `over`);
- `Foreign` is the API (`Foreign.fn`, `.program`, `.callback`, and so on,
  exported from `Py`).

The old names stay as aliases, so nothing breaks, and Python's docs keep
`Py`. The Go, Haskell and conformance tests, `GoWorker`, `Hs`, and the Go,
Haskell and one-wire docs now use the neutral names.
