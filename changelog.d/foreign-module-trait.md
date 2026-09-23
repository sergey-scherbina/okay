## foreign-module-trait - a Python module or R package behind a generated Scala object

Stage 5 of specs/foreign-highlevel.md. It is built as a GENERATOR rather
than the trait macro the backlog named. That macro needs experimental
API, and a hand-written trait repeats the Python and drifts from it.

- `okayPy/runMain okay.py.PyFacade <module> <Object> <package>` asks the
  worker to describe the module, through `okay.describe` in the injected
  `okay` module, and prints a typed Scala object.
- Type hints become types, and an open or unknown hint becomes a type
  parameter.
- Defaults are left out and named in the comment.
- `okay.r.RFacade` does the same for an R package or module. R has no
  types, so it fixes names and arity only.

Shims: Python 5, R 6. Tests: 2 golden facades that compile with the
tests and are regenerated and compared live, calls through both, and 2
mapping tests in the default gate. Docs: "A generated facade" in
docs/modules/okay-py.md and docs/modules/okay-r.md.
