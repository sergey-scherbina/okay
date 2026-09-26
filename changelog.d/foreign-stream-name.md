## foreign-stream-name — Foreign.stream, not Py.stream (2026-09-26)

A stream a far side drives or is fed is a Go or Rust call — Python's shim
claims no mux — so the language-neutral places say `Foreign.stream` and
`Foreign.releasing` (`Foreign` re-exports `Py`; the same functions): the
wire conformance suite, docs/one-language.md and the scaladoc. Python's own
test and page keep `Py` for a Python source.
