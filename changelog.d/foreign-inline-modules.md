## foreign-inline-modules - Python and R source beside the Scala

Stage 4 of specs/foreign-highlevel.md. `Py.module("scoring", """...""")`
and `R.module(...)` keep a few lines of Python or R in the Scala file
that calls them.

- The source must be a compile-time constant, checked by
  `requireConst`, and the module types have no public constructor.
- The engine ships modules when a worker starts, as files on Python's
  path, or `sys.source`d by the R shim into one environment per module.
  No wire operation evaluates source.
- `m.fn[Out]("f")` and `m.hold("Class")` address the module's functions.
- The literal is dedented, so it follows the Scala's indentation.
- An R module that does not parse refuses at start and names itself.

Tests: 5 live, and 3 compile-time rule tests in the default gate. Docs:
"Modules beside the Scala" in docs/modules/okay-py.md and
docs/modules/okay-r.md.
