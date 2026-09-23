## py-r-highlevel - a higher-level Python and R layer, filed

A backlog lane, answering the operator's question of whether okay can
offer something higher-level for working with Python and R (2026-09-23).
It adds eight items to the `polyglot` section, in order:

- foreign-journalled: the missing `Journalled` instances.
- foreign-typed-calls: a foreign function as a typed Scala function
  through `Schema`.
- foreign-object-handles: a fitted model kept on the far side as a
  `Resource`.
- foreign-inline-modules: Python/R source beside the Scala that calls
  it, shipped as a module, with no runtime eval.
- foreign-module-trait: a whole module or package behind a trait.
- foreign-streaming: a generator as a `Stage`, chunked.
- foreign-callbacks: Python/R performing okay operations.
- foreign-managed-env: `uv`/`renv` from a declared environment.

It also corrects docs/modules/okay-py.md, whose first paragraph still
said a call is journalable by `Durable`. specs/r.md had corrected that
claim on 2026-09-07; the module page had not.
