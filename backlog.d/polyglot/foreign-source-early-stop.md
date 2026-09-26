- [ ] foreign-source-early-stop — a far-side SOURCE (`Py.source`/`R.source`,
      foreign-one-mux) whose consumer stops early leaves its iterator held on
      the far side until the worker ends (split from stateful-early-stop,
      whose cluster half landed as the partition `Scope`; specs/foreign-one.md
      Decision 20). The source is a program in `Writer % O + ForeignEval`,
      and its release is a far-side `release` request, so the finaliser must
      reach the handler that holds the ref — okay's `Resource` releases with
      a plain `R => Unit`. Shape to try: `SourceRow` gains `Resource`, so a
      source cannot be run outside a scope (a compile error, not a leak), and
      the handler answers the open with a releaser bound to itself (a
      host-side answer the journal and the supervisor's replay must skip, not
      record). Trigger: a caller reading the head of a large far-side source.
      Gate: a Python generator under a consumer taking 4 of 10 000 rows,
      released once (the far side's held count back to zero).
