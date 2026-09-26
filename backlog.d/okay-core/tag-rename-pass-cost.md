- [ ] tag-rename-pass-cost — PRIORITY: LOW (trigger). Found by the core
      review 2026-09-26. `Tag` (Instances.scala) is the typed road to two
      instances of one parameterised signature in a row (`State % Int`
      twice). Performing under a key costs a wrapper per operation, which
      is fine. `Tag.tag`, which puts an ALREADY WRITTEN program's
      operations under a key, is an `interpret` over the WHOLE program:
      one extra walk and one rebuilt node per operation, per key, before
      any handler runs. A program with k tagged instances pays k walks
      on top of the handlers' own. Options: fuse the rename into the
      handler (`Tag.handler` taking the untagged program with a key), or
      make it a view (the test reads the key, the handler unwraps)
      without rebuilding. TRIGGER: the first consumer that tags a large
      program, or handler-single-pass, which would change the answer.
